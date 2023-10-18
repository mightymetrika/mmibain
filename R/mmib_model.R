#' Fit Statistical Models for MMI Bain Processing
#'
#' This function provides a unified interface to fit different statistical models
#' supported by the 'bain' package.
#'
#' @param formula A symbolic description of the model to be fit. Used specifically
#' for the `lm` and `t_test` engines. Default is NULL.
#' @param column_names A character vector of length 2, representing the column
#' names to be used for the `t_test` engine when `formula` is not provided. Default
#' is NULL.
#' @param model A model specification (usually as a string) for the `lavaan` engine.
#' Default is NULL.
#' @param data A data frame containing the variables in the model.
#' @param engine A character string representing the statistical method to be used.
#' Currently supported methods are: `"lm"`, `"t_test"`, and `"lavaan"`.
#' @param ... Additional arguments to be passed to the underlying statistical
#' function (lm(), t.test(), or lavaan::sem()).
#'
#' @return Returns an object of the type associated with the engine selected (`lm`,
#' `htest`, or `lavaan` object).
#'
#' @details
#' The mmib_model() function provides a simple interface to fit various statistical
#' models, which can be subsequently processed by the bain::bain() function. It
#' ensures that only one of `formula`, `column_names`, or `model` is provided,
#' checks the validity of the provided data, and selects the appropriate statistical
#' method based on the `engine` parameter.
#'
#' @examples
#' data(mtcars)
#'
#' # Fit linear model
#' mod1 <- mmib_model(mpg ~ wt + qsec, data = mtcars, engine = "lm")
#'
#' # Fit t-test
#' mod2 <- mmib_model(formula = vs ~ am, data = mtcars, engine = "t_test")
#'
#'
#' @seealso \code{\link[bain]{bain}} for processing the models fit with `mmib_model`.
#'
#' @export
mmib_model <- function(formula = NULL, column_names = NULL, model = NULL, data,
                       engine = c("lm", "t_test", "lavaan"), ...) {

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
