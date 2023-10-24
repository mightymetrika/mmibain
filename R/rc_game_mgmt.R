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

  # ANOVA summary
  summary_fit <- summary(fit)

  # Check if ANOVA is significant
  pval_anova <- summary_fit[[1]]$`Pr(>F)`[1]

  hypothesis <- ""

  if (pval_anova < alpha) {
    # Post-hoc t-tests
    pairwise_t <- stats::pairwise.t.test(df$ColVals, df$ColLabs, p.adjust.method = "bonferroni")

    # Construct hypothesis string
    col_levels <- levels(df$ColLabs)
    col_combinations <- utils::combn(col_levels, 2, simplify = FALSE)

    hypothesis_list <- vector("list", length(col_combinations))

    for (i in seq_along(col_combinations)) {
      comb <- col_combinations[[i]]

      if (which(levels(df$ColLabs) == comb[1]) > which(levels(df$ColLabs) == comb[2])) {
        pval <- pairwise_t$p.value[comb[1], comb[2]]
      } else {
        pval <- pairwise_t$p.value[comb[2], comb[1]]
      }

      if (pval < 0.05) {
        if (mean(df$ColVals[df$ColLabs == comb[1]]) > mean(df$ColVals[df$ColLabs == comb[2]])) {
          hypothesis_list[[i]] <- paste0(comb[1], " > ", comb[2])
        } else {
          hypothesis_list[[i]] <- paste0(comb[1], " < ", comb[2])
        }
      } else {
        hypothesis_list[[i]] <- paste0(comb[1], " = ", comb[2])
      }
    }
  }

  # After the for loop where hypothesis_list is constructed:

  # Step 1: Create the graph
  g <- igraph::graph.empty(n = length(col_levels), directed = FALSE)
  igraph::V(g)$name <- col_levels

  for (i in seq_along(hypothesis_list)) {
    relation <- hypothesis_list[[i]]
    comb <- col_combinations[[i]]

    if (grepl("=", relation)) {
      g <- g + igraph::edge(comb)
    }
  }

  # Step 2: Find connected components
  comps <- igraph::components(g)
  grouped_cols <- split(igraph::V(g)$name, comps$membership)

  # Step 3: Compare means and order
  means <- sapply(grouped_cols, function(cols) {
    mean(df$ColVals[df$ColLabs %in% cols])
  })

  ordered_groups <- grouped_cols[rev(order(means))]

  hypothesis <- paste(sapply(ordered_groups, function(group) {
    paste0("ColLabs", paste0(group, collapse = "=ColLabs"))
  }), collapse = ">")

  # Descriptives
  descriptives <- psych::describeBy(df, df$ColLabs)

  return(list(hypothesis = hypothesis,
              pairwise_t = pairwise_t,
              fit_summary = summary_fit,
              descriptives = descriptives,
              fit = fit))

}

