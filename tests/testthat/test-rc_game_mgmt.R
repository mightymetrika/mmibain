test_that("Orginial Study Workflow", {

  set.seed(100)

  # Run basic work flow
  result <- deal_cards_to_rc_grid(n = 3)
  study_data <- generate_study_data(result, sample_size = 100)
  study_results <- process_original_study(study_data)

  # Test expectations
  expect_equal(length(result), 6)
  expect_s3_class(study_data, "data.frame")
  expect_s3_class(study_results$fit, "aov")
  expect_s3_class(study_results$fit, "lm")
})

test_that("Game Work Flow",{

  set.seed(250)

  # Run basic work flow

  ## Original study
  os_deck <- deal_cards_to_rc_grid(n = 3)
  original_study_data <- generate_study_data(os_deck, sample_size = 100)
  original_study_results <- process_original_study(original_study_data)

  ## Replication study
  rs_deck <- deal_cards_to_rc_grid(n = 3)
  replication_data <- generate_study_data(rs_deck, sample_size = 100)
  replication_results <- process_replication_study(replication_data,
                                                   original_study_results)


  # Test expectations
  expect_s3_class(replication_results$bain_results, "bain")

})



test_that("Card Swap",{

  set.seed(492)

  # Replication study

  ## Swap columns
  rs_deck <- deal_cards_to_rc_grid(n = 3)
  s1 <- swapper(cards_matrix = rs_deck, c(1,2))
  expect_equal(rs_deck[1,1][[1]]$value, s1[1,2][[1]]$value)
  expect_equal(rs_deck[1,2][[1]]$value, s1[1,1][[1]]$value)
  expect_s3_class(s1, "swapper")

  ## Swap within columns
  s2 <- swapper(cards_matrix = s1, swap_in_col = 3)
  expect_equal(s1[1,3][[1]]$value, s2[2,3][[1]]$value)
  expect_equal(s1[2,3][[1]]$value, s2[1,3][[1]]$value)
  expect_equal(attr(s2, "swap_in_col_hist"), 1)
  expect_s3_class(s2, "swapper")

  ## Swap columns 2 and 3 within row 1
  s3 <- swapper(cards_matrix = s2, swap_in_row = c(1, 2, 3))
  expect_equal(s2[1,2][[1]]$value, s3[1,3][[1]]$value)
  expect_equal(s2[1,3][[1]]$value, s3[1,2][[1]]$value)
  expect_equal(attr(s3, "swap_in_col_hist"), 1)
  expect_equal(attr(s3, "swap_in_row1_hist"), 1)
  expect_equal(attr(s3, "swap_in_row2_hist"), 0)

  ## Swap columns 1 and 3 within row 2
  s4 <- swapper(cards_matrix = s3, swap_in_row = c(2, 1, 3))
  expect_equal(s3[2,1][[1]]$value, s4[2,3][[1]]$value)
  expect_equal(s3[2,3][[1]]$value, s4[2,1][[1]]$value)
  expect_equal(attr(s4, "swap_in_col_hist"), 1)
  expect_equal(attr(s4, "swap_in_row1_hist"), 1)
  expect_equal(attr(s4, "swap_in_row2_hist"), 1)

  ## Try a second swap within columns
  expect_error(swapper(cards_matrix = s4, swap_in_col = 2), "You can't swap within columns more than once.")

  ## Try a second swap within row1
  expect_error(swapper(cards_matrix = s4, swap_in_row = c(1, 1, 3)), "You can't swap within row 1 more than once.")

  ## Try a second swap within row2
  expect_error(swapper(cards_matrix = s4, swap_in_row = c(2, 2, 3)), "You can't swap within row 2 more than once.")


})


test_that("i_deck Game Workflow",{

  set.seed(250)

  # Run basic work flow

  ## Original study
  os_deck <- deal_cards_to_rc_grid(deck = mmcards::i_deck(deck = mmcards::shuffle_deck(),
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
                                   n = 3)
  original_study_data <- generate_study_data(os_deck, sample_size = 100)
  original_study_results <- process_original_study(original_study_data)

  ## Replication study
  rs_deck <- deal_cards_to_rc_grid(n = 3)
  replication_data <- generate_study_data(rs_deck, sample_size = 100)
  replication_results <- process_replication_study(replication_data,
                                                   original_study_results)

  # Test expectations
  expect_s3_class(replication_results$bain_results, "bain")

})

test_that("Random Hands Game Flow",{

  # Create function to test random hands
  find_errors <- function(num_iterations, n = 3, sample_size = 10) {
    error_seeds <- numeric(0)

    for (i in 1:num_iterations) {
      seed <- sample.int(10000, 1)  # Randomly selecting a seed value up to 10,000
      set.seed(seed)

      tryCatch({
        ## Original study
        os_deck <- deal_cards_to_rc_grid(n = n)
        original_study_data <- generate_study_data(os_deck, sample_size = sample_size)
        original_study_results <- process_original_study(original_study_data)

        ## Replication study
        rs_deck <- deal_cards_to_rc_grid(n = n)
        replication_data <- generate_study_data(rs_deck, sample_size = sample_size)
        replication_results <- process_replication_study(replication_data,
                                                         original_study_results)
      },
      error = function(e) {
        message("Error encountered with seed: ", seed)
        error_seeds <- c(error_seeds, seed)
      })
    }

    return(error_seeds)
  }

  # Call the function
  num_iterations <- 5

  # Test on n = 3, sample_size = 10
  error_seeds_3_10 <- find_errors(num_iterations)
  expect_equal(length(error_seeds_3_10), 0)

  # Test on n = 13, sample_size = 15
  error_seeds_13_15 <- find_errors(num_iterations, n = 13, sample_size = 15)
  expect_equal(length(error_seeds_13_15), 0)

})
