test_that("BF_for_everyone works", {

  # Create data
  cars2 <- cars
  cars2[["parts"]] <- rep(1:10, each = 5)

  # Run analysis
  res <- BF_for_everyone(.df = cars2, .participant = "parts",
                           formula = "dist ~ speed", hypothesis = "speed > 0")

  expect_equal(length(res), 6)
})


