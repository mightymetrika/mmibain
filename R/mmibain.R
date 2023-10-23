#' mmibain Shiny App
#'
#' This function launches a Shiny app that facilitates a user-friendly interface
#' for setting up and running a Bayesian Informative Hypotheses Evaluation using
#' the `bain` package.
#'
#' @description The user can upload CSV data; choose a model engine (lm, t_test,
#' lavaan); specify the formula, variables, or model; and provide additional
#' arguments. Once the model is fitted, the app allows for setting up hypotheses
#' for evaluation. Upon running the analysis, it displays the results of the
#' Bayesian Informative Hypotheses Evaluation.
#'
#' @details The app's UI consists of a sidebar for user inputs and a main panel
#' for displaying available variables, model terms, and analysis results. The app
#' relies on the `bain` package for analysis.
#'
#' @section UI Components:
#' \itemize{
#'  \item Data upload (CSV format).
#'  \item Engine selection (lm, t_test, lavaan).
#'  \item Model input based on chosen engine.
#'  \item Additional arguments for statistical model function.
#'  \item Action button to fit the model.
#'  \item Hypotheses input.
#'  \item Fraction input for the `bain` fraction parameter.
#'  \item Option to evaluate hypotheses with respect to standardized regression
#'  coefficients.
#'  \item Confidence interval input.
#'  \item Seed input for reproducibility.
#'  \item Action button to run the Bayesian Informative Hypotheses Evaluation.
#' }
#'
#' @return This function returns a running instance of the Shiny app.
#' Interact with the app through the browser or the RStudio Viewer pane.
#'
#' @examples
#' if(interactive()){
#'   mmibain()
#' }
#'
#' @seealso \code{\link[bain]{bain}}
#'
#' @export
#'
#' @references Hoijtink, H., Mulder, J., van Lissa, C., & Gu, X. (2019). A
#' tutorial on testing hypotheses using the Bayes factor. Psychological methods,
#' 24(5), 539–556. https://doi.org/10.1037/met0000201
mmibain <- function(){
  # UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("Set Up Bain Analysis"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput("datafile", "Choose CSV File",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
        shiny::selectInput("engine", "Choose engine:", choices = c("lm", "t_test", "lavaan")),
        shiny::uiOutput("model_input_ui"),
        shiny::textInput("additional_args", "Additional Arguments (key=value format)"),
        shiny::actionButton("fit", "Fit Model"),
        shiny::textAreaInput("hypotheses", "Hypotheses", ""),
        shiny::numericInput("fraction", "Fraction", value = 1),
        shiny::checkboxInput("standardize", "Standardize", value = FALSE),
        shiny::numericInput("ci_value", "Confidence Interval", value = 0.95, min = 0, max = 1),
        shiny::numericInput("seed_value", "Set seed (optional)", value = NA, min = 1, max = .Machine$integer.max),
        shiny::actionButton("run_analysis", "Run Analysis")
      ),
      shiny::mainPanel(
        shiny::htmlOutput("variables_section"),
        shiny::uiOutput("model_terms_header"),
        shiny::verbatimTextOutput("model_terms"),
        shiny::uiOutput("bain_results_header"),
        shiny::verbatimTextOutput("print_bain_output"),
        shiny::verbatimTextOutput("summary_bain_output")
      )
    )
  )

  # Server
  server <- function(input, output, session) {

    # Reactive: Read the uploaded CSV file
    uploaded_data <- shiny::reactive({
      # Check if a file is uploaded
      inFile <- input$datafile
      if (is.null(inFile)) {
        return(NULL)
      }

      # Read the CSV and return it
      utils::read.csv(inFile$datapath, stringsAsFactors = TRUE)
    })

    # Display the entire variables section (header + variable names)
    output$variables_section <- shiny::renderUI({
      if (!is.null(uploaded_data())) {
        list(
          shiny::tags$h2("Available Variables"),
          shiny::verbatimTextOutput("variables")
        )
      }
    })

    # Display the variable names
    output$variables <- shiny::renderPrint({
      names(uploaded_data())
    })

    # Select Engine
    shiny::observe({
      if(input$engine == "lavaan") {
        output$model_input_ui <- shiny::renderUI({
          shiny::textAreaInput("formula_or_model", "Lavaan Model", value = "", rows = 5, resize = "both")
        })
      } else if(input$engine == "lm") {
        output$model_input_ui <- shiny::renderUI({
          shiny::textInput("formula_or_model", "Formula", value = "")
        })
      } else if(input$engine == "t_test") {
        output$model_input_ui <- shiny::renderUI({
          list(
            shiny::textInput("column_name_1", "Column Name 1"),
            shiny::textInput("column_name_2", "Column Name 2"))
          })
        }
      })

    # Logic to fit the model
    model <- NULL
    model_fitted <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(input$fit, {
      shiny::req(uploaded_data(), input$engine)

      # Adjusting how arguments are gathered based on the engine type
      if (input$engine == "t_test") {
        # For t_test engine: gather column names
        column_names <- c(input$column_name_1, input$column_name_2)
        args_list <- list(column_names = column_names, data = uploaded_data(), engine = input$engine)
      } else if (input$engine == "lm") {
        # For lm use formula specification
        args_list <- list(formula = stats::as.formula(input$formula_or_model), data = uploaded_data(), engine = input$engine)
      } else if (input$engine == "lavaan"){
        args_list <- list(model = input$formula_or_model, data = uploaded_data(), engine = input$engine)

      }

      # Add extra arguments if provided
      if (nzchar(input$additional_args)) {
        extra_args <- tryCatch({
          str2list(input$additional_args)
        }, error = function(e) {
          shiny::showNotification(
            paste("Error in additional arguments:", e$message),
            type = "error",
            duration = NULL
          )
          return(NULL)
        })

        if (!is.null(extra_args)) {
          args_list <- c(args_list, extra_args)
        }
      }

      tryCatch({
        model <<- do.call(mmib_model, args_list)
        model_fitted(TRUE)

        # Display terms based on the engine type
        if (input$engine == "lm") {
          terms <- names(stats::coef(model))
        } else if (input$engine == "t_test") {
          terms <- c(input$column_name_1, input$column_name_2)
        } else if (input$engine == "lavaan") {
          terms <- names(lavaan::coef(model))
        }

        output$model_terms <- shiny::renderPrint({ terms })

      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", e$message),
          type = "error",
          duration = NULL
        )
        model_fitted(FALSE)
      })
    })

    output$model_terms_header <- shiny::renderUI({
      if(model_fitted()) {
        shiny::tags$h2("Available Terms for Hypothesis")
      }
    })

    # Logic to run bain analysis on the fitted model

    # Set reactive value to flag when analysis is done
    bain_analysis_done <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(input$run_analysis, {
      shiny::req(model_fitted())

      # If seed value is provided, set it
      if (!is.na(input$seed_value)) {
        set.seed(input$seed_value)
      }

      tryCatch({
        # Run bain analysis
        bain_result <- do.call(bain::bain, args = list(x = model,
                                                       hypothesis = input$hypotheses,
                                                       fraction = input$fraction,
                                                       standardize = input$standardize))

        # Set outputs for the model and bain analysis
        output$print_bain_output <- shiny::renderPrint({ print(bain_result) })
        output$summary_bain_output <- shiny::renderPrint({ summary(bain_result, ci = input$ci_value) })

        # Flag that bain analysis is done
        bain_analysis_done(TRUE)

        }, error = function(e) {
          shiny::showNotification(
            paste("Error:", e$message),
            type = "error",
            duration = NULL
            )

          # Flag that bain analysis is not done
          bain_analysis_done(FALSE)
          })
    })

    output$bain_results_header <- shiny::renderUI({
      if(bain_analysis_done()) {
        shiny::tags$h2("Bayesian Informative Hypotheses Evaluation Results")
      }
    })
  }
  shiny::shinyApp(ui = ui, server = server)
}
