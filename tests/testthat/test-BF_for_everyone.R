test_that("BF_for_everyone works", {

  # Create data
  ex_dat <- data.frame(
    participant = rep(1:10, each = 10),
    x = rnorm(100),
    y = rnorm(100)
  )

  # Run analysis
  res <- BF_for_everyone(.df = ex_dat, .participant = "participant",
                         formula = "y ~ x", hypothesis = "x > 0")

  expect_equal(length(res), 6)
})
