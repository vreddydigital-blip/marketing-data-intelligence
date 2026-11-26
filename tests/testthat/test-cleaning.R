test_that("duplicate rows are removed, originals untouched", {
  d <- make_campaign_df(30)
  d2 <- rbind(d, d[1:5, ])
  out <- remove_duplicate_rows(d2)
  expect_equal(nrow(out$data), 30)
  expect_equal(out$changed, 5)
})

test_that("numeric and categorical imputation fill all missing values", {
  d <- make_campaign_df(40)
  d$spend[1:4] <- NA
  out <- impute_numeric(d, "spend", "median")
  expect_false(any(is.na(out$data$spend)))
  expect_equal(out$changed, 4)
  expect_equal(out$data$spend[1], stats::median(d$spend[-(1:4)]))

  d$someday <- c(NA, sample(c("x", "y"), 39, replace = TRUE))
  out2 <- impute_categorical(d, "someday", "missing_category")
  expect_true(all(out2$data$someday[1] == "Missing"))
})

test_that("row filters work with structured conditions only", {
  d <- make_campaign_df(60)
  out <- apply_row_filters(d, list(list(column = "spend", operator = ">", value = 250)))
  expect_true(all(out$data$spend > 250))
  expect_equal(out$changed, sum(d$spend <= 250))
})

test_that("date parsing handles ISO and Excel serial dates", {
  expect_equal(parse_date_column(c("2024-03-01", "2024-03-02")), as.Date(c("2024-03-01", "2024-03-02")))
  srl <- c(45352, 45353)  # 2024-03-01/02 in Excel serials
  expect_equal(parse_date_column(srl), as.Date(c("2024-03-01", "2024-03-02")))
})

test_that("outlier detection flags an extreme point", {
  d <- make_campaign_df(100)
  d$spend[1] <- 10 * max(d$spend)
  b <- detect_outliers_iqr(d, "spend")
  expect_gte(b$spend$n_outliers, 1)
  out <- remove_outliers(d, "spend")
  expect_equal(out$changed, b$spend$n_outliers)
})
