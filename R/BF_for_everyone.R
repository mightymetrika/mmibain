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
  .n_hyp <- length(gregexpr(";", hypothesis, fixed = TRUE)) + 1

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

  return(results_matrix)
}
