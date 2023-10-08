mmib_model <- function(formula = NULL, column_names = NULL, model = NULL, data,
                       engine = "lm", standardize = TRUE, ...) {

  # Check inputs
  if (sum(!is.null(formula), !is.null(column_names), !is.null(model)) != 1) {
    stop("Only one of formula, column_names, or model should be provided.")
  }

  if (!is.data.frame(data)) {
    stop("data is not a valid data frame")
  }

  # Fit the model based on the engine selected
  switch(engine,
         lm = {
           if (is.null(formula)) stop("Formula is required for lm engine.")

           # Standardize independent variables if standardize is set to TRUE
           if (standardize) {
             all_vars <- all.vars(formula)
             response_var <- as.character(formula)[2]
             predictor_vars <- setdiff(all_vars, response_var)
             numeric_predictors <- predictor_vars[sapply(data[predictor_vars], is.numeric)]
             data[numeric_predictors] <- lapply(data[numeric_predictors], scale)
           }

           model <- stats::lm(formula, data, ...)
         },
         t_test = {
           if (!is.null(formula)) {
             response_var <- as.character(formula)[2]
             group_var <- as.character(formula)[3]
             model <- stats::t.test(data[[response_var]] ~ data[[group_var]], ...)
           } else if (!is.null(column_names) && length(column_names) == 2) {
             x <- data[[column_names[1]]]
             y <- data[[column_names[2]]]
             model <- stats::t.test(x, y, ...)
           } else {
             stop("For t_test, either provide a formula or two column names.")
           }
         },
         lavaan = {
           if (is.null(model)) stop("Model specification is required for lavaan engine.")
           model <- lavaan::sem(model, data = data, ...)
         },
         stop(paste("Invalid engine:", engine))
  )

  return(model)
}
