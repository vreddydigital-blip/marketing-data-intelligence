test_that("problem type detection", {
  d <- make_campaign_df(60)
  expect_equal(detect_problem_type(d, "revenue"), "regression")
  expect_equal(detect_problem_type(d, "channel"), "classification_multiclass")
  d2 <- d; d2$flag <- rep(c("Y", "N"), times = c(30, 30))
  expect_equal(detect_problem_type(d2, "flag"), "classification_binary")
})

test_that("regression model trains and reports RMSE/MAE/R2 on held-out data", {
  set.seed(11)
  n <- 300
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), channel = sample(c("a", "b"), n, TRUE))
  d$y <- 5 * d$x1 - 3 * d$x2 + ifelse(d$channel == "b", 2, 0) + rnorm(n, sd = .3)
  eng <- ml_engines$regression[[1]]
  res <- train_single_model(d, "y", c("x1", "x2", "channel"), "regression", eng, seed = 42)
  m <- as.data.frame(res$metrics_test)
  expect_setequal(m$.metric, c("rmse", "mae", "rsq"))
  expect_true(all(m$.estimate >= 0))
  expect_true(m$.estimate[m$.metric == "rsq"] > 0.8)
  expect_equal(res$n_train + res$n_test, n)
})

test_that("model workflow never fits preprocessing on test data (no leakage)", {
  set.seed(12)
  n <- 200
  d <- data.frame(cat = sample(c("a", "b", "c"), n, TRUE), num = rnorm(n))
  d$y <- rnorm(n)
  eng <- ml_engines$regression[[1]]
  res <- train_single_model(d, "y", c("cat", "num"), "regression", eng, seed = 7)
  wf <- res$fitted_workflow
  # the recipe was fitted on training rows only: the preprocessed training
  # design matrix (mold) must have exactly the training-set row count
  fitted_recipe <- workflows::extract_recipe(wf, estimated = TRUE)
  expect_true(inherits(fitted_recipe, "recipe"))
  mold <- hardhat::extract_mold(wf)
  expect_equal(nrow(mold$predictors), res$n_train)
  expect_equal(nrow(mold$outcomes), res$n_train)
})

test_that("small or degenerate problems are blocked with clear messages", {
  d <- make_campaign_df(10)
  warns <- validate_ml_problem(d, "revenue", "spend", "regression")
  expect_true(any(grepl("at least", warns)))
  d2 <- make_campaign_df(100); d2$const <- 5
  warns2 <- validate_ml_problem(d2, "const", "spend", "regression")
  expect_true(any(grepl("constant", warns2)))
})

test_that("feature importance is extractable for ranger", {
  set.seed(13)
  n <- 300
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  d$y <- d$x1 * 3 + d$x2 + rnorm(n, .2)
  eng <- ml_engines$regression[[3]]  # ranger
  res <- train_single_model(d, "y", c("x1", "x2"), "regression", eng, seed = 42)
  imp <- regression_feature_importance(res)
  expect_true(!is.null(imp))
  expect_setequal(imp$feature, c("x1", "x2"))
  expect_true(all(imp$importance >= 0))
})

test_that("k-means clustering returns profiles and silhouette in range", {
  set.seed(14)
  n <- 120
  d <- data.frame(a = c(rnorm(60, 0, .3), rnorm(60, 5, .3)), b = c(rnorm(60, 5, .3), rnorm(60, 0, .3)))
  res <- run_clustering(d, c("a", "b"), k = 2, seed = 1)
  expect_equal(ncol(res$profiles), 3)
  expect_true(is.na(res$silhouette) || (res$silhouette >= -1 && res$silhouette <= 1))
})
