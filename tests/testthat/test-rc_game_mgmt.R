# test_that("basic work flow works", {
#   result <- deal_cards_to_rc_grid(n = 3)
#   study_data <- generate_study_data(result, sample_size = 100)
#   study_results <- process_original_study(study_data)
#   bain_res <- bain::bain(study_results$fit, hypothesis = study_results$hypothesis)
#   expect_s3_class(bain_res, "bain")
# })
