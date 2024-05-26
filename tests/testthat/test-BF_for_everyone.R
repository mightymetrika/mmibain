test_that("BF_for_everyone works", {

  res <- BF_for_everyone(.df = Loblolly, .participant = "Seed",
                         formula = "height ~ age", hypothesis = "age > 2.5")

  expect_equal(length(res), 6)
})


