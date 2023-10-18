test_that("mmib_model works with the `lm` engine", {
  # Fit linear model
  mod <- mmib_model(mpg ~ wt + qsec, data = mtcars, engine = "lm")
  mod_comp <- stats::lm(mpg ~ wt + qsec, data = mtcars)

  expect_equal(sum(mod$coefficients == mod_comp$coefficients), 3)
  expect_equal(class(mod), class(mod_comp))

  expect_error(mmib_model(mpg ~ wt + qsec, data = mtcars))
  expect_error(mmib_model(mpg ~ wt + qsec, column_names = "mpg", data = mtcars,
                          engine = "lm"), "Only one of formula, column_names, or model should be provided.")
})

test_that("mmib_model works with the `t_test` engine", {
  # Fit t.test with formula
  mod <- mmib_model(formula = vs ~ am, data = mtcars, engine = "t_test")
  mod_comp <- stats::t.test(vs ~ am, data = mtcars)

  expect_true(mod$statistic == mod_comp$statistic)
  expect_equal(class(mod), class(mod_comp))

  rm(mod, mod_comp)

  # Fit t.test with columns
  mod <- mmib_model(column_names = c("vs", "am"), data = mtcars, engine = "t_test")
  mod_comp <- stats::t.test(mtcars$vs, mtcars$am)

  expect_true(mod$statistic == mod_comp$statistic)
  expect_equal(class(mod), class(mod_comp))

  rm(mod, mod_comp)

  # Fit t.test with reverse columns
  mod <- mmib_model(column_names = c("vs", "am"), data = mtcars, engine = "t_test")
  mod_comp <- stats::t.test(mtcars$am, mtcars$vs)

  expect_true(mod$statistic == -mod_comp$statistic)

})

test_that("mmib_model works with the `lavaan` engine", {

  #  Specify model
  mod <- '
    A =~ mpg + wt + drat + cyl
    B =~ disp + hp + cyl
  '

  # Fit models
  suppressWarnings(fit <- mmib_model(model = mod, data = mtcars, engine = "lavaan",
                                     std.lv = TRUE))
  suppressWarnings(fit_comp <- lavaan::sem(mod, data = mtcars, std.lv = TRUE))


  expect_true(fit@test$standard$stat == fit_comp@test$standard$stat)
})
