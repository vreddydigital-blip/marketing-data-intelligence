test_that("ROAS and CPL insights are generated with evidence for campaign data", {
  d <- make_campaign_df(300, seed = 5)
  m <- detect_marketing_columns(d)
  ks <- kpi_summary(d, m)
  state <- list(df = d, name = "test", quality = compute_quality_report(d),
                kpi_summary = ks, mapping = m, correlations = correlation_matrix(d),
                forecast_summary = NULL, model_metrics = NULL)
  ins <- generate_insights(state)
  expect_true(nrow(ins) > 0)
  expect_true(any(grepl("ROAS", ins$title, fixed = TRUE)))
  expect_true(all(c("evidence", "action", "confidence") %in% names(ins)))
  expect_true(all(ins$confidence |> nzchar()))
})

test_that("empty/insufficient data yields an explicit 'no evidence' message", {
  d <- data.frame(a = 1:3, b = c("x", "y", "z"))
  state <- list(df = d, name = "tiny", quality = compute_quality_report(d),
                kpi_summary = NULL, mapping = detect_marketing_columns(d),
                correlations = NULL, forecast_summary = NULL, model_metrics = NULL)
  ins <- generate_insights(state)
  expect_true(any(grepl("does not contain", ins$explanation[1])))
})

test_that("missing data triggers a duplication/insight warning", {
  d <- make_campaign_df(200)
  d$spend[1:60] <- NA
  state <- list(df = d, name = "gappy", quality = compute_quality_report(d),
                kpi_summary = NULL, mapping = NULL, correlations = NULL,
                forecast_summary = NULL, model_metrics = NULL)
  ins <- generate_insights(state)
  expect_true(any(grepl("missing", ins$title, ignore.case = TRUE)))
})
