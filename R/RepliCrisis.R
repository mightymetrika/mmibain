RepliCrisis <- function(){
  ui <- shiny::fluidPage(
    shiny::titlePanel("RepliCrisis"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # Game setup
        shiny::numericInput("sample_size", "Select Sample Size", value = 10, min = 3, max = 500),
        shiny::numericInput("difficulty", "Select Difficulty", value = 3, min = 3, max = 13),
        shiny::numericInput("alpha_level", "Select Alpha Level", value = 0.05, min = 0.01, max = 0.10, step = 0.01),
        shiny::numericInput("seed_value", "Set seed (optional)", value = NA, min = 1, max = .Machine$integer.max),

        # Original study
        shiny::fluidRow(
          shiny::column(12, shiny::actionButton("original_study", "Conduct Original Study")),
          shiny::column(12, shiny::actionButton("show_diagnostics", "Show Diagnostics")),
          shiny::column(12, shiny::actionButton("show_descriptives", "Show Descriptives")),

          # Replication study
          shiny::column(12, shiny::actionButton("replication_study", "Conduct Replication Study"))
        ),
        shiny::numericInput("swap_col1", "Swap Column 1", value = NULL),
        shiny::numericInput("swap_col2", "Swap Column 2", value = NULL),
        shiny::actionButton("swap_cols", "Swap Columns"),
        shiny::numericInput("swap_in_col", "Swap Inside Column", value = NULL),
        shiny::actionButton("swap_inside_col", "Execute Inside Column Swap"),
        shiny::numericInput("swap_in_row_num", "Row Number", value = NULL),
        shiny::numericInput("swap_in_row_col1", "Swap Row Column 1", value = NULL),
        shiny::numericInput("swap_in_row_col2", "Swap Row Column 2", value = NULL),
        shiny::actionButton("swap_inside_row", "Execute Inside Row Swap"),
        shiny::actionButton("run_replication", "Run Replication Analysis")
      ),

      shiny::mainPanel(
        shiny::tableOutput("card_display"),
        shiny::textOutput("hypothesis"),
        shiny::tableOutput("fit_summary"),
        shiny::plotOutput("fit_plot"),
        shiny::textOutput("shapiro_test"),
        shiny::textOutput("levene_test"),
        shiny::tableOutput("descriptives"),
        shiny::textOutput("bain_results_summary"),
        shiny::textOutput("bain_results")
      )
    )
  )

  server <- function(input, output, session) {
    shiny::observeEvent(input$original_study, {
      # If seed value is provided, set it
      if (!is.na(input$seed_value)) {
        set.seed(input$seed_value)
      }

      # Deal cards
      card_grid <- deal_cards_to_rc_grid(deck = mmcards::i_deck(deck = mmcards::shuffle_deck(),
                                                                i_path = "www",
                                                                i_names = c("2_of_clubs", "2_of_diamonds", "2_of_hearts", "2_of_spades",
                                                                            "3_of_clubs", "3_of_diamonds", "3_of_hearts", "3_of_spades",
                                                                            "4_of_clubs", "4_of_diamonds", "4_of_hearts", "4_of_spades",
                                                                            "5_of_clubs", "5_of_diamonds", "5_of_hearts", "5_of_spades",
                                                                            "6_of_clubs", "6_of_diamonds", "6_of_hearts", "6_of_spades",
                                                                            "7_of_clubs", "7_of_diamonds", "7_of_hearts", "7_of_spades",
                                                                            "8_of_clubs", "8_of_diamonds", "8_of_hearts", "8_of_spades",
                                                                            "9_of_clubs", "9_of_diamonds", "9_of_hearts", "9_of_spades",
                                                                            "10_of_clubs", "10_of_diamonds", "10_of_hearts", "10_of_spades",
                                                                            "jack_of_clubs", "jack_of_diamonds", "jack_of_hearts", "jack_of_spades",
                                                                            "queen_of_clubs", "queen_of_diamonds", "queen_of_hearts", "queen_of_spades",
                                                                            "king_of_clubs", "king_of_diamonds", "king_of_hearts", "king_of_spades",
                                                                            "ace_of_clubs", "ace_of_diamonds", "ace_of_hearts", "ace_of_spades"
                                                                )),
                                         n = input$difficulty)

      # It's unclear how you want to display the card grid. If it's a simple matrix, you can use:
      #output$card_display <- shiny::renderTable(card_grid)

      # Generate study data
      study_data <- generate_study_data(card_grid, sample_size = input$sample_size)

      # Process original study
      study_results <- process_original_study(study_data, alpha = input$alpha_level)

      # Display fit summary and hypothesis
      output$fit_summary <- shiny::renderTable(broom::tidy(study_results$fit))
      output$hypothesis <- shiny::renderText(study_results$hypothesis)
    })

    shiny::observeEvent(input$show_diagnostics, {
      # Display diagnostics for the original study: fit plot, Shapiro test, and Levene test

      # Placeholder code
    })

    shiny::observeEvent(input$show_descriptives, {
      # Display descriptive statistics for the original study

      # Placeholder code
    })

    shiny::observeEvent(input$replication_study, {
      # Deal cards for replication study

      # Placeholder code
    })

    shiny::observeEvent(input$swap_cols, {
      # Swap columns as per user choice

      # Placeholder code
    })

    shiny::observeEvent(input$swap_inside_col, {
      # Swap inside a column as per user choice

      # Placeholder code
    })

    shiny::observeEvent(input$swap_inside_row, {
      # Swap inside a row as per user choice

      # Placeholder code
    })

    shiny::observeEvent(input$run_replication, {
      # Run the replication analysis after user finishes making swaps

      # Placeholder code
    })
  }

  shiny::shinyApp(ui, server)
}
