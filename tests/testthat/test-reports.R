test_that("Excel report writes all available sheets", {
  d <- make_campaign_df(120)
  m <- detect_marketing_columns(d)
  st <- list(df = d, name = "rep-test", info = describe_dataset(d),
             quality = compute_quality_report(d), kpi_summary = kpi_summary(d, m),
             mapping = m, stats_results = NULL, model_metrics = NULL,
             forecast_table = NULL, insights = NULL)
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  build_excel_report(st, path)
  expect_true(file.exists(path) && file.size(path) > 1000)
  sheets <- openxlsx::getSheetNames(path)
  expect_true(all(c("Summary", "KPIs", "Campaigns") %in% sheets))
})

test_that("Excel report fails loudly without a dataset", {
  expect_error(build_excel_report(list(df = NULL), tempfile(fileext = ".xlsx")), "No dataset")
})

test_that("CSV report writes the current dataset", {
  d <- make_campaign_df(30)
  path <- tempfile(fileext = ".csv"); on.exit(unlink(path), add = TRUE)
  build_csv_report(list(df = d), path)
  expect_equal(nrow(readr::read_csv(path, show_col_types = FALSE)), 30)
})
