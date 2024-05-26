#' Compute Bayes Factors for Each Participant and Summarize Results
#'
#' This function splits a dataset by participants, fits linear models for each
#' participant, computes Bayes Factors (BFs) using the `bain` package, and
#' summarizes the results.
#'
#' @param .df A data frame containing the data.
#' @param .participant A string specifying the name of the participant column in
#' the data frame.
#' @param formula A formula specifying the linear model to be fit.
#' @param hypothesis A string specifying the hypotheses to be tested using the
#' `bain` package.
#'
#' @return A list containing:
#' \describe{
#'   \item{GPBF}{A matrix of the geometric product, evidence rate, and stability
#'   rate for each hypothesis.}
#'   \item{BFs}{A matrix of the Bayes Factors for each participant and hypothesis.}
#'   \item{BF_summary}{A summary matrix of the mean, median, standard deviation,
#'   minimum, and maximum of the Bayes Factors for each hypothesis.}
#'   \item{N}{The number of participants.}
#'   \item{bain_res}{A list of `bain` results for each participant.}
#'   \item{Plot}{A `ggplot2` object visualizing the distribution of Bayes Factors
#'   by hypothesis.}
#' }
#'
#' @examples
#'  # Run analysis
#'  res <- BF_for_everyone(.df = Loblolly, .participant = "Seed",
#'                         formula = "height ~ age", hypothesis = "age > 2.5")
#'
#'  # View GPBF results
#'  res$GPBF
#'
#' @references
#' Klaassen, F. (2020). Combining Evidence Over Multiple Individual Analyses. In
#' R. van de Schoot & M. Miočević (Eds.), Small Sample Size Solutions: A Guide for
#' Applied Researchers and Practitioners (1st ed., pp. 13). Routledge.
#' <doi:10.4324/9780429273872-11>
#'
#' @export
BF_for_everyone <- function(.df, .participant, formula, hypothesis) {
  # split data
  dsets <- split(.df, .df[[.participant]])

  # get participant specific bain results
  bain_res <- lapply(dsets, function(data){
    # fit linear model
    lm_model <- do.call(stats::lm, list(formula = stats::as.formula(formula),
                                        data = data))

    # fit bain model
    bain::bain(lm_model, hypothesis = hypothesis)
  }) |> stats::setNames(names(dsets))

  # get number of participants
  .N <- length(dsets)

  # get number of hypotheses
  .n_hyp <- length(strsplit(hypothesis, split = ";")[[1]])

  # process results to extract Bayes Factors
  results <- lapply(bain_res, function(x) {
    # Always extract BF1c
    BF1c <- x[["fit"]][1, 7]
    BF_out <- c(BF1c = BF1c)

    # Extract BF12, BF13, ..., BF1n based on number of hypotheses
    if (.n_hyp > 1) {
      additional_BFs <- sapply(2:.n_hyp, function(j) x[["BFmatrix"]][1, j])
      BF_out <- c(BF_out, stats::setNames(additional_BFs,
                                          paste("BF1", 2:.n_hyp, sep = "")))
    }

    return(BF_out)
  }) |> stats::setNames(names(bain_res))

  # convert the list of results to a matrix
  results_matrix <- do.call(rbind, results)
  colnames(results_matrix) <- names(results[[1]])
  rownames(results_matrix) <- names(bain_res)

  # compute gPBF
  res <- apply(results_matrix, 2, function(x) {
    # Remove NA values
    x <- x[!is.na(x)]

    # Compute geometric product
    GP <- exp(mean(log(x)))

    # Compute evidence rate
    ER <- abs((GP < 1) - mean(x > 1))

    # Compute stability rate
    SR <- mean(ifelse(GP < 1, x < GP, x > GP))

    c(GP, ER, SR)
  })

  rownames(res) <- c("Geometric Product", "Evidence Rate", "Stability Rate")

  # BF summary
  BF_summary <- apply(results_matrix, 2, function(x) {
    c(
      Mean = mean(x, na.rm = TRUE),
      Median = stats::median(x, na.rm = TRUE),
      SD = stats::sd(x, na.rm = TRUE),
      Min = min(x, na.rm = TRUE),
      Max = max(x, na.rm = TRUE)
    )
  })

  # Prepare data for plotting
  rm_temp <- results_matrix |> as.data.frame()
  rm_long <- do.call(rbind, lapply(names(rm_temp), function(x){
    data.frame(Comparison = x, BF = rm_temp[[x]], check.names = FALSE)
  }))

  # declare temporary variables for plot
  Comparison <- NULL
  BF <- NULL

  # create plot
  p <- ggplot2::ggplot(rm_long, ggplot2::aes(x = Comparison, y = BF)) +
    ggplot2::geom_boxplot(outlier.shape = NA)+
    ggplot2::geom_jitter(show.legend = FALSE,  height = 0, alpha = 0.25)+
    ggplot2::facet_wrap(~Comparison, scales = "free_x") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Bayes Factors by Comparison",
                  y = "Bayes Factor", x = "Comparison")


  out <- list(GPBF = res, BFs = results_matrix,
              BF_summary = BF_summary, N = .N,
              bain_res = bain_res, Plot = p)
  return(out)
}
