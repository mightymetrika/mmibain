test_that("basic original study work flow works", {

  set.seed(100)

  # Run basic work flow
  result <- deal_cards_to_rc_grid(n = 3)
  study_data <- generate_study_data(result, sample_size = 100)
  study_results <- process_original_study(study_data)
  #bain_res <- bain::bain(study_results$fit, hypothesis = study_results$hypothesis)

  # Test expectations
  expect_equal(length(result), 2)
  expect_s3_class(result$updated_deck, "UpDeck")
  expect_s3_class(study_data, "data.frame")
  expect_s3_class(study_results$fit, "aov")
  expect_s3_class(study_results$fit, "lm")
})

test_that("basic game work flow works",{

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
  expect_s3_class(replication_results, "bain")

})

test_that("card swap works",{

  set.seed(492)

  # Replication study

  ## Swap columns
  rs_deck <- deal_cards_to_rc_grid(n = 3)
  s1 <- swapper(cards_matrix = rs_deck$cards_matrix, c(1,2))
  expect_equal(rs_deck$cards_matrix[1,1][[1]]$value, s1[1,2][[1]]$value)
  expect_equal(rs_deck$cards_matrix[1,2][[1]]$value, s1[1,1][[1]]$value)
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
