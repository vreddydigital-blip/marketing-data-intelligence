test_that("upload module loads a sample dataset into shared state (integration)", {
  dataset <- shiny::reactiveValues(current = NULL, original = NULL, name = NULL,
                                   is_sample = FALSE, info = NULL, quality = NULL,
                                   history = list(), version = 0L)
  shiny::testServer(mod_upload_server, args = list(dataset = dataset), {
    session$setInputs(load_marketing = 1L)
    req_local <- dataset$current
    expect_true(!is.null(req_local))
    expect_gt(nrow(req_local), 1000)
    expect_true(grepl("synthetic", dataset$name, fixed = TRUE))
    expect_true(!is.null(dataset$quality))
  })
})

test_that("data quality module renders issues for a flawed dataset (integration)", {
  dataset <- shiny::reactiveValues(current = NULL, original = NULL, name = NULL,
                                   is_sample = TRUE, info = NULL, quality = NULL,
                                   history = list(), version = 0L)
  d <- read_data_file(file.path(app_root_dir(), "sample_data", "marketing_campaigns.csv"),
                      "marketing_campaigns.csv")
  shiny::isolate(set_dataset(dataset, d, "test", is_sample = TRUE))
  expect_gt(shiny::isolate(dataset$quality$score), 0)
  expect_lte(shiny::isolate(dataset$quality$score), 100)
  expect_true(nrow(shiny::isolate(dataset$quality$issues)) >= 1)
})

test_that("cleaning module applies dedupe and reset-to-original (integration)", {
  dataset <- shiny::reactiveValues(current = NULL, original = NULL, name = NULL,
                                   is_sample = TRUE, info = NULL, quality = NULL,
                                   history = list(), version = 0L)
  shiny::isolate(set_dataset(dataset, read_data_file(file.path(app_root_dir(), "sample_data", "marketing_campaigns.csv"),
                                      "marketing_campaigns.csv"), "t", is_sample = TRUE))
  n0 <- nrow(shiny::isolate(dataset$current))
  out <- remove_duplicate_rows(shiny::isolate(dataset$current))
  shiny::isolate(apply_cleaning(dataset, out, "Remove duplicate rows"))
  expect_lt(nrow(shiny::isolate(dataset$current)), n0)
  expect_identical(dim(shiny::isolate(dataset$original))[1], n0)
  shiny::testServer(mod_cleaning_server, args = list(dataset = dataset), {
    session$setInputs(reset = 1L)
    expect_equal(nrow(shiny::isolate(dataset$current)), n0)
  })
})
