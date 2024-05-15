BFfe <- function() {
  # UI
  ui <- shiny::fluidPage(
    theme = shinythemes::shinytheme("lumen"),
    shiny::titlePanel("Set Up BF for everyone Analysis"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput("datafile", "Choose CSV File",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
        shiny::uiOutput("model_input_ui"),
        shiny::uiOutput("hypothesis_input_ui"),
        shiny::uiOutput("participant_input_ui"),
        shiny::uiOutput("seed_input_ui"),
        shiny::uiOutput("run_analysis_input_ui"),
        shiny::uiOutput("participant_select_ui")
      ),
      shiny::mainPanel(
        shiny::uiOutput("variables_title"),  # Placeholder for the title
        DT::dataTableOutput("variables_table"),
        shiny::uiOutput("BFfe_summary_header"),
        shiny::verbatimTextOutput("BFfe_summary_output"),
        shiny::uiOutput("GPBF_header"),
        shiny::verbatimTextOutput("GPBF_output"),
        shiny::plotOutput("Plot_output"),
        shiny::uiOutput("participant_results_header"),
        shiny::verbatimTextOutput("participant_bf_results"),
        shiny::verbatimTextOutput("participant_bf_matrix")
      )
    )
  )

  # Server
  server <- function(input, output, session) {
    # Reactive: Read the uploaded CSV file
    uploaded_data <- shiny::reactiveVal()
    shiny::observe({
      inFile <- input$datafile
      if (!is.null(inFile)) {
        data <- utils::read.csv(inFile$datapath, stringsAsFactors = TRUE)
        uploaded_data(data)
      }
    })

    output$variables_title <- shiny::renderUI({
      if (!is.null(uploaded_data()) && nrow(uploaded_data()) > 0) {
        shiny::tags$h2("Available Variables")
      }
    })

    output$variables_table <- DT::renderDataTable({
      shiny::req(uploaded_data())
      data <- uploaded_data()
      df <- data.frame(Variable = names(data), Type = sapply(data, class))
      DT::datatable(df, editable = 'cell', options = list(pageLength = 5),
                    rownames = FALSE)
    })

    shiny::observeEvent(input$variables_table_cell_edit, {
      info <- input$variables_table_cell_edit
      shiny::req(uploaded_data())
      data <- uploaded_data()
      row_number <- info$row
      new_value <- info$value

      if (info$col == 0) {
        tryCatch({
          names(data)[row_number] <- new_value
          uploaded_data(data)
        }, error = function(e) {
          shiny::showNotification(
            paste("Error in changing variable name:", e$message),
            type = "error",
            duration = NULL
          )
        })
      }

      if (info$col == 1) {
        variable_name <- names(data)[row_number]
        tryCatch({
          if (new_value == "factor") {
            data[[variable_name]] <- as.factor(data[[variable_name]])
          } else if (new_value == "numeric") {
            data[[variable_name]] <- as.numeric(data[[variable_name]])
          } else if (new_value == "integer") {
            data[[variable_name]] <- as.integer(data[[variable_name]])
          } else if (new_value == "double") {
            data[[variable_name]] <- as.double(data[[variable_name]])
          } else if (new_value == "character") {
            data[[variable_name]] <- as.character(data[[variable_name]])
          } else {
            stop("New data type must be one of the following: factor, numeric, integer, double, character")
          }
          uploaded_data(data)
        }, error = function(e) {
          shiny::showNotification(
            paste("Error in changing data type:", e$message),
            type = "error",
            duration = NULL
          )
        })
      }
    })

    shiny::observe({
      if (!is.null(uploaded_data())) {
        output$model_input_ui <- shiny::renderUI({
          shiny::textInput("formula_or_model", "Formula", value = "")
        })
      }
    })

    shiny::observe({
      if (!is.null(uploaded_data())) {
        output$hypothesis_input_ui <- shiny::renderUI({
          shiny::textInput("hypothesis_str", "Hypothesis", value = "")
        })
      }
    })

    shiny::observe({
      if (!is.null(uploaded_data())) {
        output$participant_input_ui <- shiny::renderUI({
          shiny::textInput("participant", "Participant Variable")
        })
      }
    })

    shiny::observe({
      if (!is.null(uploaded_data())) {
        output$seed_input_ui <- shiny::renderUI({
          shiny::numericInput("seed_value", "Set seed (optional)",
                              value = NA, min = 1, max = .Machine$integer.max)
        })
      }
    })

    shiny::observe({
      if (!is.null(uploaded_data())) {
        output$run_analysis_input_ui <- shiny::renderUI({
          shiny::actionButton("run_analysis", "Run Analysis")
        })
      }
    })

    BFfe_analysis_done <- shiny::reactiveVal(FALSE)
    BFfe_result <- shiny::reactiveVal()

    shiny::observeEvent(input$run_analysis, {
      shiny::req(uploaded_data(), input$participant, input$formula_or_model, input$hypothesis_str)
      if (!is.na(input$seed_value)) {
        set.seed(input$seed_value)
      }
      tryCatch({
        result <- BF_for_everyone(
          .df = uploaded_data(),
          .participant = input$participant,
          formula = input$formula_or_model,
          hypothesis = input$hypothesis_str
        )
        BFfe_result(result)
        BFfe_analysis_done(TRUE)
        output$BFfe_summary_output <- shiny::renderPrint({ result$BF_summary })
        output$GPBF_output <- shiny::renderPrint({ result$GPBF })
        output$Plot_output <- shiny::renderPlot({ result$Plot })
      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", e$message),
          type = "error",
          duration = NULL
        )
        BFfe_analysis_done(FALSE)
      })
    })

    output$BFfe_summary_header <- shiny::renderUI({
      if (BFfe_analysis_done()) {
        shiny::tags$h2("BFfe Summary")
      }
    })

    output$GPBF_header <- shiny::renderUI({
      if (BFfe_analysis_done()) {
        shiny::tags$h2("GPBF Summary")
      }
    })

    output$participant_select_ui <- shiny::renderUI({
      shiny::req(BFfe_analysis_done())
      participants <- unique(uploaded_data()[[input$participant]])
      shiny::selectInput("selected_participant", "Select Participant",
                         choices = participants, selected = participants[1])
    })

    shiny::observeEvent(input$selected_participant, {
      shiny::req(BFfe_analysis_done())
      result <- BFfe_result()
      participant_id <- input$selected_participant
      participant_index <- which(names(result$bain_res) == participant_id)
      selected_bain_res <- result$bain_res[[participant_index]]
      output$participant_bf_results <- shiny::renderPrint({
        selected_bain_res
      })
      output$participant_bf_matrix <- shiny::renderPrint({
        selected_bain_res$BFmatrix
      })
    })

    output$participant_results_header <- shiny::renderUI({
      if (BFfe_analysis_done() && !is.null(input$selected_participant)) {
        shiny::tags$h2(paste0("BF Results for ", input$participant, ": ",
                              input$selected_participant))
      }
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}

# BFfe <- function(){
#   # UI
#   ui <- shiny::fluidPage(
#     theme = shinythemes::shinytheme("lumen"),
#     shiny::titlePanel("Set Up BF for everyone Analysis"),
#     shiny::sidebarLayout(
#       shiny::sidebarPanel(
#         shiny::fileInput("datafile", "Choose CSV File",
#                          multiple = FALSE,
#                          accept = c("text/csv",
#                                     "text/comma-separated-values,text/plain",
#                                     ".csv")),
#         shiny::uiOutput("model_input_ui"),
#         shiny::uiOutput("hypothesis_input_ui"),
#         shiny::uiOutput("participant_input_ui"),
#         shiny::uiOutput("seed_input_ui"),
#         shiny::uiOutput("run_analysis_input_ui")
#       ),
#       shiny::mainPanel(
#         shiny::uiOutput("variables_title"),  # Placeholder for the title
#         DT::dataTableOutput("variables_table"),
#         shiny::uiOutput("BFfe_summary_header"),
#         shiny::verbatimTextOutput("BFfe_summary_output"),
#         shiny::uiOutput("GPBF_header"),
#         shiny::verbatimTextOutput("GPBF_output"),
#         shiny::plotOutput("Plot_output")
#       )
#     )
#   )
#
#   # Server
#   server <- function(input, output, session) {
#
#     # Reactive: Read the uploaded CSV file
#     uploaded_data <- shiny::reactiveVal()
#
#     shiny::observe({
#       inFile <- input$datafile
#       if (!is.null(inFile)) {
#         data <- utils::read.csv(inFile$datapath, stringsAsFactors = TRUE)
#         uploaded_data(data)
#       }
#     })
#
#     output$variables_title <- shiny::renderUI({
#       if (!is.null(uploaded_data()) && nrow(uploaded_data()) > 0) {
#         shiny::tags$h2("Available Variables")
#       }
#     })
#
#
#     output$variables_table <- DT::renderDataTable({
#       shiny::req(uploaded_data())
#       data <- uploaded_data()
#       df <- data.frame(Variable = names(data), Type = sapply(data, class))
#       DT::datatable(df, editable = 'cell', options = list(pageLength = 5),
#                     rownames = FALSE)
#     })
#
#     shiny::observeEvent(input$variables_table_cell_edit, {
#       info <- input$variables_table_cell_edit
#       shiny::req(uploaded_data())
#       data <- uploaded_data()
#
#       row_number <- info$row
#       new_value <- info$value
#
#       if (info$col == 0){
#         tryCatch({
#           names(data)[row_number] <- new_value
#           # Update the reactive data frame
#           uploaded_data(data)
#         }, error = function(e) {
#           shiny::showNotification(
#             paste("Error in changing variable name:", e$message),
#             type = "error",
#             duration = NULL
#           )
#         })
#       }
#
#       if (info$col == 1) {  # Assuming the 'Type' column is the second column
#         variable_name <- names(data)[row_number]  # Fetch the variable name using row_number
#         tryCatch({
#           if (new_value == "factor") {
#             data[[variable_name]] <- as.factor(data[[variable_name]])
#           } else if (new_value == "numeric") {
#             data[[variable_name]] <- as.numeric(data[[variable_name]])
#           } else if (new_value == "integer") {
#             data[[variable_name]] <- as.integer(data[[variable_name]])
#           } else if (new_value == "double") {
#             data[[variable_name]] <- as.double(data[[variable_name]])
#           } else if (new_value == "character") {
#             data[[variable_name]] <- as.character(data[[variable_name]])
#           } else {
#             stop("New data type must be one of the following: factor, numeric, integer, double, character")
#           }
#           # Update the reactive data frame
#           uploaded_data(data)
#         }, error = function(e) {
#           shiny::showNotification(
#             paste("Error in changing data type:", e$message),
#             type = "error",
#             duration = NULL
#           )
#         })
#       }
#     })
#
#     # Enter formula
#     shiny::observe({
#       if (!is.null(uploaded_data())) {
#         output$model_input_ui <- shiny::renderUI({
#           shiny::textInput("formula_or_model", "Formula", value = "")
#         })
#       }
#     })
#
#     # Enter hypothesis
#     shiny::observe({
#       if (!is.null(uploaded_data())) {
#         output$hypothesis_input_ui <- shiny::renderUI({
#           shiny::textInput("hypothesis_str", "Hypothesis", value = "")
#         })
#       }
#     })
#
#     # Enter participant
#     shiny::observe({
#       if (!is.null(uploaded_data())) {
#         output$participant_input_ui <- shiny::renderUI({
#           shiny::textInput("participant", "Participant Variable")
#         })
#       }
#     })
#
#     # Enter seed
#     shiny::observe({
#       if (!is.null(uploaded_data())) {
#         output$seed_input_ui <- shiny::renderUI({
#           shiny::numericInput("seed_value", "Set seed (optional)",
#                               value = NA, min = 1, max = .Machine$integer.max)
#         })
#       }
#     })
#
#     # Enter analysis button
#     shiny::observe({
#       if (!is.null(uploaded_data())) {
#         output$run_analysis_input_ui <- shiny::renderUI({
#           shiny::actionButton("run_analysis", "Run Analysis")
#         })
#       }
#     })
#
#     # Logic to run BFfe analysis on the fitted model
#
#     # Set reactive value to flag when analysis is done
#     BFfe_analysis_done <- shiny::reactiveVal(FALSE)
#     #BFfe_res <- shiny::reactiveVal()
#
#     shiny::observeEvent(input$run_analysis, {
#       # Ensure the necessary inputs are available
#       shiny::req(uploaded_data(), input$participant, input$formula_or_model, input$hypothesis_str)
#
#
#       # If seed value is provided, set it
#       if (!is.na(input$seed_value)) {
#         set.seed(input$seed_value)
#       }
#
#       tryCatch({
#         # Run BF_for_everyone analysis
#         BFfe_result <- BF_for_everyone(
#           .df = uploaded_data(),
#           .participant = input$participant,
#           formula = input$formula_or_model,
#           hypothesis = input$hypothesis_str
#         )
#
#
#         # Set outputs for the BFfe analysis
#         output$BFfe_summary_output <- shiny::renderPrint({ BFfe_result$BF_summary })
#         output$GPBF_output <- shiny::renderPrint({ BFfe_result$GPBF })
#         output$Plot_output <- shiny::renderPlot({ BFfe_result$Plot })
#
#         # BFfe_res(BFfe_result)
#
#         # Flag that BFfe analysis is done
#         BFfe_analysis_done(TRUE)
#
#       }, error = function(e) {
#         shiny::showNotification(
#           paste("Error:", e$message),
#           type = "error",
#           duration = NULL
#         )
#
#         # Flag that CATs analysis is not done
#         BFfe_analysis_done(FALSE)
#       })
#     })
#
#     output$BFfe_summary_header <- shiny::renderUI({
#       if(BFfe_analysis_done()) {
#         shiny::tags$h2("BFfe Summary")
#       }
#     })
#
#     output$GPBF_header <- shiny::renderUI({
#       if(BFfe_analysis_done()) {
#         shiny::tags$h2("GPBF Summary")
#       }
#     })
#
#   }
#
#   shiny::shinyApp(ui = ui, server = server)
# }
