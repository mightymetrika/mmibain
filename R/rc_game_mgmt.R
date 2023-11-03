deal_cards_to_rc_grid <- function(deck = mmcards::i_deck(deck = mmcards::shuffle_deck(),
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
                                                         )), n) {

  # Ensure there are enough cards in the deck to deal
  if(nrow(deck) < 2*n) {
    stop("Not enough cards in the deck to deal")
  }

  # Deal 2*n cards from the deck
  dealt_cards <- vector("list", 2*n)

  for(i in 1:(2*n)) {
    deck <- mmcards::deal_card(deck)
    dealt_cards[[i]] <- deck$dealt_card
  }

  # Reshape the data frame to the desired format
  cards_matrix <- matrix(dealt_cards, nrow = 2, ncol = n, byrow = TRUE)

  # Return results
  return(cards_matrix)
}


generate_study_data <- function(x, sample_size) {

  # Initialize empty vectors to hold column labels and values
  col_labs <- character(0)
  col_vals <- numeric(0)

  # Number of columns in cards_matrix
  num_cols <- ncol(x)

  # For each column in cards_matrix
  for (j in 1:num_cols) {

    # Extract mean and sd values for the current column
    mean_val <- x[[1, j]]$value
    sd_val <- x[[2, j]]$value

    # Generate a sample using rnorm
    generated_vals <- stats::rnorm(n = sample_size, mean = mean_val, sd = sd_val)

    # Append generated values to col_vals
    col_vals <- c(col_vals, generated_vals)

    # Append column labels
    col_labs <- c(col_labs, rep(paste0("Col", j), sample_size))
  }

  # Create a data frame with the results
  df_result <- data.frame(ColLabs = col_labs, ColVals = col_vals)
  df_result$ColLabs <- as.factor(df_result$ColLabs)

  return(df_result)
}

process_original_study <- function(df, alpha = 0.05) {

  # ANOVA without the intercept
  fit <- stats::aov(ColVals ~ ColLabs - 1, data = df)

  # Check for normality of residuals
  shapiro_res <- fit |>
    stats::residuals() |>
    stats::shapiro.test()

  # Check for homogeneity of variances
  levene_res <- car::leveneTest(ColVals ~ ColLabs, data = df)

  # ANOVA summary
  summary_fit <- summary(fit)

  # Check if ANOVA is significant
  pval_anova <- summary_fit[[1]]$`Pr(>F)`[1]

  hypothesis_list <- list()

  if (pval_anova < alpha) {
    # Post-hoc t-tests
    pairwise_t <- stats::pairwise.t.test(df$ColVals, df$ColLabs, p.adjust.method = "bonferroni")

    # Construct hypothesis string
    col_levels <- levels(df$ColLabs)
    col_combinations <- utils::combn(col_levels, 2, simplify = FALSE)

    for (i in seq_along(col_combinations)) {
      comb <- col_combinations[[i]]

      if (which(levels(df$ColLabs) == comb[1]) > which(levels(df$ColLabs) == comb[2])) {
        pval <- pairwise_t$p.value[comb[1], comb[2]]
      } else {
        pval <- pairwise_t$p.value[comb[2], comb[1]]
      }

      if (pval < alpha) {
        if (mean(df$ColVals[df$ColLabs == comb[1]]) > mean(df$ColVals[df$ColLabs == comb[2]])) {
          hypothesis_list[[paste0(comb[1], comb[2])]] <- paste0(comb[1], " > ", comb[2])
        } else {
          hypothesis_list[[paste0(comb[1], comb[2])]] <- paste0(comb[1], " < ", comb[2])
        }
      } else {
        hypothesis_list[[paste0(comb[1], comb[2])]] <- paste0(comb[1], " = ", comb[2])
      }
    }

    # Check if all pairwise t-tests are not significant
    if (all(pairwise_t$p.value >= alpha, na.rm = TRUE)) {
      col_levels <- levels(df$ColLabs)
      combined_hypothesis <- paste(col_levels, collapse = " = ")
    }

  } else {
    col_levels <- levels(df$ColLabs)
    combined_hypothesis <- paste(col_levels, collapse = " = ")
  }

  # Creating a directed graph for simplifying relations
  g <- igraph::graph.empty(n = length(col_levels), directed = TRUE)
  igraph::V(g)$name <- col_levels

  for (i in seq_along(hypothesis_list)) {
    relation <- unlist(hypothesis_list[i])
    comb <- col_combinations[[i]]

    if (grepl(">", relation)) {
      g <- g + igraph::edge(comb[1], comb[2])
    } else if (grepl("<", relation)) {
      g <- g + igraph::edge(comb[2], comb[1])
    } # else: do nothing for "=" relation
  }


  # Simplify the graph by removing direct edges if an indirect path exists
  # between the same nodes
  g <- igraph::simplify(g, remove.multiple = FALSE, remove.loops = FALSE,
                        edge.attr.comb = igraph::igraph_opt("edge.attr.comb"))

  # Extract simplified relations from the graph
  hypothesis_list_simplified <- list()

  igraph::E(g)$name <- NA # clear existing names

  for (e in igraph::E(g)) {
    src <- igraph::get.vertex.attribute(g, "name", index = igraph::tail_of(g, e))
    dest <- igraph::get.vertex.attribute(g, "name", index = igraph::head_of(g, e))
    hypothesis_list_simplified[[paste0(src, dest)]] <- paste0(src, " > ", dest)
  }

  # Combine simplified relations and add the prefix "ColLabs"
  if (length(hypothesis_list_simplified) != 0) {
    combined_hypothesis <- paste(
      sapply(unlist(hypothesis_list_simplified), function(hyp) {
        gsub("(Col\\d+)", "ColLabs\\1", hyp)
      }),
      collapse = " & "
    )
  } else {
    combined_hypothesis <- gsub("(Col\\d+)", "ColLabs\\1", combined_hypothesis)
  }


  # Descriptives
  descriptives <- generate_descriptives(df, "ColLabs")

  return(list(hypothesis = combined_hypothesis,
              pairwise_t = if (pval_anova < alpha) {
                pairwise_t} else {NULL},
              fit_summary = summary_fit,
              descriptives = descriptives,
              fit = fit,
              shapiro_test = shapiro_res,
              levene_test = levene_res))
}

generate_Ho_from_pairwise_t <- function(original_study_results) {

  if (!is.null(original_study_results$pairwise_t)){
    # Extract column and row names from pairwise_t
    colnames <- colnames(original_study_results$pairwise_t$p.value)
    rownames <- rownames(original_study_results$pairwise_t$p.value)

    # Combine column and row names to get all unique variables
    all_variables <- unique(c(colnames, rownames))

    # Prefix the variable names with "ColLabs"
    all_variables <- paste0("ColLabs", all_variables)

    # Generate the null hypothesis
    Ho <- paste(all_variables, collapse = " = ")
  } else {
    Ho <- original_study_results$hypothesis
  }



  return(Ho)
}

process_replication_study <- function(replication_data, original_study_results) {

  # Extract Horiginal from original_study_results
  Horiginal <- original_study_results$hypothesis

  # Generate the null hypothesis
  Ho <- generate_Ho_from_pairwise_t(original_study_results)

  # Prepare the bain hypothesis string
  if (Ho == Horiginal) {
    # If the hypotheses are the same, use only one
    bain_hypothesis <- Horiginal
    message_to_display <- "Notice: The null hypothesis and the original hypothesis were the same, so only the original hypothesis is tested."
  } else {
    bain_hypothesis <- paste0(Ho, " ; ", Horiginal)
    message_to_display <- NULL
  }

  # Set up bain call
  fit_replication <- stats::aov(ColVals ~ ColLabs - 1, data = replication_data)
  bain_results <- do.call(bain::bain, args = list(x = fit_replication,
                                                  hypothesis = bain_hypothesis))

  # Include the potential message in the results
  list(bain_results = bain_results, message = message_to_display)
}


swapper <- function(cards_matrix, swap_cols = NULL, swap_in_col = NULL,
                    swap_in_row = NULL) {

  # Initialize move history
  if (!inherits(cards_matrix, "swapper")) {
    attr(cards_matrix, "swap_in_col_hist") <- 0
    attr(cards_matrix, "swap_in_row1_hist") <- 0
    attr(cards_matrix, "swap_in_row2_hist") <- 0
  }

  # Swap columns
  if (!is.null(swap_cols) && length(swap_cols) == 2) {
    temp <- cards_matrix[, swap_cols[1]]
    cards_matrix[, swap_cols[1]] <- cards_matrix[, swap_cols[2]]
    cards_matrix[, swap_cols[2]] <- temp
  }

  # Swap within column
  if (!is.null(swap_in_col) && swap_in_col > 0) {
    if (attr(cards_matrix, "swap_in_col_hist") >= 1) {
      stop("You can't swap within columns more than once.")
    }
    cards_matrix[, swap_in_col] <- rev(cards_matrix[, swap_in_col])
    attr(cards_matrix, "swap_in_col_hist") <- attr(cards_matrix, "swap_in_col_hist") + 1
  }

  # Swap within row
  if (!is.null(swap_in_row) && length(swap_in_row) == 3) {
    row_num <- swap_in_row[1]
    col1 <- swap_in_row[2]
    col2 <- swap_in_row[3]

    if (row_num == 1 && attr(cards_matrix, "swap_in_row1_hist") >= 1) {
      stop("You can't swap within row 1 more than once.")
    }

    if (row_num == 2 && attr(cards_matrix, "swap_in_row2_hist") >= 1) {
      stop("You can't swap within row 2 more than once.")
    }

    temp <- cards_matrix[row_num, col1]
    cards_matrix[row_num, col1] <- cards_matrix[row_num, col2]
    cards_matrix[row_num, col2] <- temp

    if (row_num == 1) {
      attr(cards_matrix, "swap_in_row1_hist") <- attr(cards_matrix, "swap_in_row1_hist") + 1
    } else if (row_num == 2) {
      attr(cards_matrix, "swap_in_row2_hist") <- attr(cards_matrix, "swap_in_row2_hist") + 1
    }
  }

  # Update the class
  class(cards_matrix) <- c("swapper", class(cards_matrix))

  return(cards_matrix)
}


render_card_grid <- function(new_card_grid) {
  rep_card_images <- unlist(apply(new_card_grid, 1, function(row) sapply(row, function(card) {
    shiny::renderImage({
      list(src = system.file(card$icard, package = "mmibain"), contentType = "image/png", width = 200, height = "auto")
    }, deleteFile = FALSE)
  })))

  rep_matrix_layout <- matrix(rep_card_images, nrow = 2, byrow = TRUE)
  card_ui <- apply(rep_matrix_layout, 1, function(row) {
    shiny::fluidRow(lapply(row, shiny::column, width = floor(12/length(row))))
  })
  return(card_ui)
}

interpret_replication_results <- function(replication_results, bf_threshold = 3,
                                          pmpb_threshold = 0.80) {

  # Retrieve hypothesis names
  hypothesis_names <- row.names(replication_results$bain_results$fit)

  # Check if H2 is present
  h2_present <- "H2" %in% hypothesis_names

  # Retrieve BF.c and PMPb values
  bf_c_values <- replication_results$bain_results$fit$BF.c
  pmpb_values <- replication_results$bain_results$fit$PMPb

  # Function to interpret a single hypothesis with nested condition checking
  interpret_hypothesis <- function(hypothesis, bf_c, pmpb) {
    if (!is.na(bf_c) && bf_c > bf_threshold && !is.na(pmpb) && pmpb > pmpb_threshold) {
      list(message = paste("There is strong evidence for", hypothesis, "with both BF.c and PMPb exceeding their respective thresholds."),
           result = "win")
    } else {
      list(message = paste("The evidence for", hypothesis, "is inconclusive or does not satisfy both thresholds."),
           result = "lose")
    }
  }

  # Interpretation and result initialisation
  interpretation <- ""
  result <- "lose"

  if (h2_present) {
    # Interpret H2
    h2_interpretation <- interpret_hypothesis("H2", bf_c_values[2], pmpb_values[2])
    interpretation <- h2_interpretation$message
    result <- h2_interpretation$result
  } else {
    # Interpret H1 if H2 is not present
    h1_interpretation <- interpret_hypothesis("H1", bf_c_values[1], pmpb_values[1])
    interpretation <- h1_interpretation$message
    result <- h1_interpretation$result
  }

  # Disclaimer with citation
  disclaimer <- paste(
    "Disclaimer: The thresholds for BF.c and PMPb are used here as part of a game for educational purposes.",
    "This approach is not intended to endorse the rigid use of threshold values for hypothesis testing.",
    "It is very unfortunate that threshold values have appeared in the literature to label evidence as 'positive' or 'strong'.",
    "For a comprehensive understanding of Bayesian Factors in the context of informative hypothesis testing,",
    "please refer to 'Hoijtink, H., Mulder, J., van Lissa, C., & Gu, X. (2019). A tutorial on testing hypotheses using the Bayes factor.",
    "Psychological methods, 24(5), 539-556. https://doi.org/10.1037/met0000201'.",
    "Additional references include 'Jeffreys, H. (1961). Theory of Probability. Oxford: Clarendon Press.'",
    "and 'Kass, R.E. & Raftery, A.E. (1995). Bayes Factors. Journal of the American Statistical Association, 90, 773-795.",
    "http://dx.doi.org/10.1080/01621459.1995.10476572'.",
    sep = " "
  )

  # Combine interpretation, result, and disclaimer into a list
  final_output <- list(
    interpretation = interpretation,
    result = result,
    disclaimer = disclaimer
  )

  return(final_output)
}
