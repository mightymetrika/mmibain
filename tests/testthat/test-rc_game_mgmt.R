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
