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

  out <- list(GPBF = res, BFs = results_matrix,
              BF_summary = BF_summary, N = .N,
              bain_res = bain_res)
  return(out)
}
