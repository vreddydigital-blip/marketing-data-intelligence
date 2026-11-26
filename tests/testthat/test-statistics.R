test_that("Pearson correlation finds the planted perfect relationship", {
  d <- data.frame(x = 1:50, y = 2 * (1:50) + rnorm(50, 0, .01))
  r <- correlation_analysis(d, "x", "y", "pearson")
  expect_gt(r$estimate, 0.99)
  expect_lt(r$p_value, 1e-10)
  expect_true("ci_low" %in% names(r))
})

test_that("chi-square detects the planted association and reports Cramer's V", {
  set.seed(1)
  d <- data.frame(
    ch = c(rep("Search", 100), rep("Email", 100)),
    conv = c(rep("yes", 60), rep("no", 40), rep("yes", 30), rep("no", 70))
  )
  r <- chi_square_test(d, "ch", "conv")
  expect_true(grepl("Cramer", r$effect_size, fixed = TRUE))
  expect_lt(r$p_value, 0.001)
})

test_that("t-test recovers a large planted difference with Cohen's d", {
  set.seed(2)
  d <- data.frame(value = c(rnorm(50, 10), rnorm(50, 15)), grp = rep(c("a", "b"), each = 50))
  r <- t_test_contrast(d, "value", "grp")
  expect_lt(r$p_value, 0.001)
  expect_true(grepl("Cohen", r$effect_size))
  expect_gt(abs(r$estimate), 3)
})

test_that("regression returns coefficients, R-squared, and flags low data", {
  set.seed(3)
  d <- data.frame(x1 = 1:80, x2 = rnorm(80))
  d$y <- 3 * d$x1 + 2 * d$x2 + rnorm(80)
  lr <- linear_regression(d, "y", c("x1", "x2"))
  expect_gt(lr$glance$r.squared, 0.95)
  expect_equal(nrow(lr$coefficients), 3)  # intercept + 2 predictors
  expect_error(linear_regression(d[1:5, ], "y", c("x1", "x2")), "Not enough complete rows")
})
