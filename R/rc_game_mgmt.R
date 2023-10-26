deal_cards_to_rc_grid <- function(deck = mmcards::shuffle_deck(), n) {

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
  return(list(cards_matrix = cards_matrix, updated_deck = deck))
}


generate_study_data <- function(x, sample_size) {

  # Initialize empty vectors to hold column labels and values
  col_labs <- character(0)
  col_vals <- numeric(0)

  # Extract the cards_matrix from x
  cards_matrix <- x$cards_matrix

  # Number of columns in cards_matrix
  num_cols <- ncol(cards_matrix)

  # For each column in cards_matrix
  for (j in 1:num_cols) {

    # Extract mean and sd values for the current column
    mean_val <- cards_matrix[[1, j]]$value
    sd_val <- cards_matrix[[2, j]]$value

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
  combined_hypothesis <- paste(
    sapply(unlist(hypothesis_list_simplified), function(hyp) {
      gsub("(Col\\d+)", "ColLabs\\1", hyp)
    }),
    collapse = " & "
  )

  # Descriptives
  descriptives <- psych::describeBy(df, df$ColLabs)

  return(list(hypothesis = combined_hypothesis,
              pairwise_t = pairwise_t,
              fit_summary = summary_fit,
              descriptives = descriptives,
              fit = fit,
              shapiro_test = shapiro_res,
              levene_test = levene_res))
}


generate_Ho_from_Horiginal <- function(Horiginal) {
  # Extract unique variables
  vars <- unique(unlist(regmatches(Horiginal, gregexpr("\\b[a-zA-Z]+\\d*\\b", Horiginal))))

  # Concatenate with "="
  Ho <- paste(vars, collapse = " = ")

  return(Ho)
}

process_replication_study <- function(replication_data, original_study_results) {

  # Extract Horiginal from original_study_results
  Horiginal <- original_study_results$hypothesis

  # Generate the null hypothesis
  Ho <- generate_Ho_from_Horiginal(Horiginal)

  # Prepare the bain hypothesis string
  bain_hypothesis <- paste0(Ho, " ; ", Horiginal)

  # Set up bain call
  fit_replication <- stats::aov(ColVals ~ ColLabs - 1, data = replication_data)
  bain_results <- do.call(bain::bain, args = list(x = fit_replication,
                                                 hypothesis = bain_hypothesis))

  return(bain_results)
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
