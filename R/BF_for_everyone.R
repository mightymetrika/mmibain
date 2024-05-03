BF_for_everyone <- function(.df, .participant, formula, hypothesis) {
  # split data
  dsets <- split(.df, .df[[.participant]])

  lm_res <- lapply(dsets, function(data){
    # fit linear model for a participant
    lm_model <- do.call(stats::lm, list(formula = stats::as.formula(formula),
                                        data = data))

    # fit bain model
    bain::bain(lm_model, hypothesis = hypothesis)
  })

  return(lm_res)
}
