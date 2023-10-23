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

  return(df_result)
}
