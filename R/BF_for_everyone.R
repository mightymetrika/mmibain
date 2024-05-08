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
  })

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
  })

  # convert the list of results to a matrix
  results_matrix <- do.call(rbind, results)
  colnames(results_matrix) <- names(results[[1]])

  # compute gPBF
  res <- apply(results_matrix, 2, function(x){
    .N_there <- sum(!is.na(x))
    GP <- prod(x, na.rm = TRUE) ^ (1 / .N_there)
    ER <- abs((GP < 1) - sum(x > 1, na.rm = TRUE)/.N_there)
    SR <- ifelse(GP < 1,
                 sum(x < GP, na.rm = TRUE) / .N_there,
                 sum(x > GP, na.rm = TRUE) / .N_there)
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
    ggplot2::labs(title = "Bayes Factors by Comparison", y = "Bayes Factor", x = "Comparison")


  out <- list(GPBF = res, BFs = results_matrix,
              BF_summary = BF_summary, N = .N,
              bain_res = bain_res, Plot = p)
  return(out)
}
