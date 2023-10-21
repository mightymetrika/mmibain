mmibain <- function(){
  # UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("MMIBain Analysis"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # CSV Input
        shiny::fileInput("datafile", "Choose CSV File",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
        # Engine selection
        shiny::selectInput("engine", "Choose engine:", choices = c("lm", "t_test", "lavaan")),
        shiny::uiOutput("model_input_ui"),
        # Additional arguments
        shiny::textInput("additional_args", "Additional Arguments (key=value format)"),
        # 6. Fit model button
        shiny::actionButton("fit", "Fit Model"),
        # 7. Displaying the terms for hypotheses
        # 8. User hypotheses
        shiny::textAreaInput("hypotheses", "Hypotheses", ""),
        # 9. Fraction input
        shiny::numericInput("fraction", "Fraction", value = 1),
        # 10. Standardize option
        shiny::checkboxInput("standardize", "Standardize", value = FALSE),
        # New - Run Analysis button
        shiny::actionButton("run_analysis", "Run Analysis")
      ),
      shiny::mainPanel(
        # Variable names display
        shiny::htmlOutput("variables_section"),
        shiny::uiOutput("model_terms_header"),
        shiny::verbatimTextOutput("model_terms"),
        # 11 & 12. Outputs for model fit and bain analysis
        shiny::verbatimTextOutput("model_output"),
        shiny::verbatimTextOutput("bain_output")
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
    shiny::observeEvent(input$run_analysis, {
      # ... Your logic here to run bain::bain on model_fitted() ...

      # Set outputs
      output$model_output <- shiny::renderText({
        # This is just a placeholder. Replace with actual model output.
        model_fitted()
      })

      output$bain_output <- shiny::renderText({
        # This is just a placeholder. Replace with actual bain output.
        "Your bain analysis results..."
      })
    })

  }

  shiny::shinyApp(ui = ui, server = server)
}
