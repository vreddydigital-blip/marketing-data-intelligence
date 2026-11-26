test_that("safe_div returns exact ratios for known inputs", {
  expect_equal(safe_div(10, 4), 2.5)
  expect_equal(safe_div(c(10, 20), c(4, 8)), c(2.5, 2.5))
})

test_that("safe_div never returns Inf or NaN", {
  expect_true(is.na(safe_div(10, 0)))
  expect_true(is.na(safe_div(NA, 10)))
  expect_true(is.na(safe_div(10, NA)))
  expect_false(is.infinite(safe_div(10, 1)))
  expect_false(is.nan(safe_div(10, 1)))
})
