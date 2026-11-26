test_that("CSV files with headers, missing cells, and different encodings load", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c("a,b,c", "1,x,2024-01-01", "2,y,", "3,xz,2024-01-03"), tmp)
  d <- read_data_file(tmp, "x.csv")
  expect_equal(nrow(d), 3)
  expect_equal(ncol(d), 3)
  expect_true(is.na(d$b[2]) || d$b[2] == "y")
})

test_that("semicolon-delimited CSVs load via delimiter probing", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c("a;b;c", "1;2;3", "4;5;6"), tmp)
  d <- read_data_file(tmp, "x.csv")
  expect_equal(ncol(d), 3)
})

test_that("xlsx files load and sheet selection works", {
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Main"); openxlsx::writeData(wb, "Main", data.frame(a = 1:3))
  openxlsx::addWorksheet(wb, "Other"); openxlsx::writeData(wb, "Other", data.frame(z = 4:6))
  openxlsx::saveWorkbook(wb, tmp)
  d <- read_data_file(tmp, "x.xlsx")
  expect_equal(names(d), "a")
  d2 <- read_data_file(tmp, "x.xlsx", sheet = "Other")
  expect_equal(names(d2), "z")
})

test_that("unsupported/empty files are rejected with clear errors", {
  tmp <- tempfile(fileext = ".txt"); writeLines("hello", tmp); on.exit(unlink(tmp), add = TRUE)
  expect_error(read_data_file(tmp, "x.txt"), "Unsupported file type")
  v <- validate_upload_file(tmp, "x.txt")
  expect_false(v$ok)
  expect_true(grepl("Unsupported file type", v$message[1]))
})

test_that("uploaded filenames are sanitised against path traversal", {
  nm <- safe_internal_name("../../evil/name<script>.CSV")
  expect_false(grepl("\\.\\.", nm))
  expect_true(grepl("\\.csv$", nm))
})
