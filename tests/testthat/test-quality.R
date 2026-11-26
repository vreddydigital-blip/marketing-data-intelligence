test_that("quality score is bounded 0-100 and breakdown sums to the score", {
  d <- make_campaign_df(100)
  q <- compute_quality_report(d)
  expect_gte(q$score, 0); expect_lte(q$score, 100)
  expect_equal(sum(q$breakdown$points), q$score)
})

test_that("duplicate and whitespace issues are detected", {
  d <- make_campaign_df(50)
  d$channel[1:5] <- paste0(" ", d$channel[1:5])
  d <- rbind(d, d[1, ])
  q <- compute_quality_report(d)
  checks <- q$issues$check
  expect_true("whitespace" %in% checks)
  expect_true("duplicate_rows" %in% checks)
  expect_gt(q$metrics$duplicate_rows, 0)
  expect_lt(q$score, 100)
})

test_that("empty dataset yields score 0 without error", {
  q <- compute_quality_report(data.frame())
  expect_equal(q$score, 0)
})
