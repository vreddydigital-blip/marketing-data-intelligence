# Machine learning service built on tidymodels. Preprocessing (imputation,
# encoding, scaling) lives inside a recipe fitted on training data only, so
# test data can never leak into preprocessing. Seeds are fixed for
# reproducibility. Models are never trained when data is too small/unsuitable.

DEFAULT_SEED <- 42

MIN_ROWS_REGRESSION <- 30
MIN_ROWS_CLASSIFICATION <- 30
MAX_CARDINALITY_FACTOR <- 50L

detect_problem_type <- function(df, target) {
  x <- df[[target]]
  if (is.numeric(x)) return("regression")
  uq <- unique(x[!is.na(x)])
  if (length(uq) == 2) return("classification_binary")
  if (length(uq) > 2) return("classification_multiclass")
  stop("Target variable has no discernible values.", call. = FALSE)
}

validate_ml_problem <- function(df, target, features, problem_type) {
  problems <- character(0)
  if (nrow(df) < if (grepl("classification", problem_type)) MIN_ROWS_CLASSIFICATION else MIN_ROWS_REGRESSION) {
    problems <- c(problems, sprintf("Only %d rows available; at least %d are required for reliable modelling.",
                                    nrow(df), MIN_ROWS_REGRESSION))
  }
  if (!length(features)) problems <- c(problems, "No predictor variables selected.")
  if (!length(unique(df[[target]][!is.na(df[[target]])])) >= 2 && grepl("classification", problem_type)) {
    problems <- c(problems, "Target variable has fewer than 2 distinct classes.")
  }
  if (is.numeric(df[[target]]) && stats::var(df[[target]], na.rm = TRUE) == 0) {
    problems <- c(problems, "Target variable is constant; there is nothing to learn.")
  }
  high_card <- features[vapply(features, function(f) {
    is.character(df[[f]]) && length(unique(df[[f]][!is.na(df[[f]])])) > MAX_CARDINALITY_FACTOR * nrow(df) / 10
  }, logical(1))]
  no_signal_high_card <- high_card[vapply(high_card, function(f) {
    length(unique(df[[f]][!is.na(df[[f]])])) > 0.9 * nrow(df)
  }, logical(1))]
  if (length(no_signal_high_card)) {
    problems <- c(problems, sprintf("Candidate ID-like column(s) with almost no repeated values will not generalise: %s",
                                    paste(no_signal_high_card, collapse = ", ")))
  }
  problems
}

build_ml_recipe <- function(df_train, target, features, problem_type, normalize = FALSE) {
  form <- stats::reformulate(features, response = target)
  rec <- recipes::recipe(form, data = df_train) |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_impute_median(recipes::all_numeric_predictors()) |>
    recipes::step_impute_mode(recipes::all_nominal_predictors()) |>
    recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = FALSE)
  if (normalize) {
    rec <- rec |> recipes::step_normalize(recipes::all_numeric_predictors())
  }
  # classification: keep class labels usable
  if (grepl("classification", problem_type)) {
    rec <- rec |> recipes::step_string2factor(recipes::all_outcomes())
  }
  rec
}

ml_engines <- list(
  regression = list(
    list(name = "Linear regression", spec = function() parsnip::linear_reg() |> parsnip::set_engine("lm"), normalized = FALSE),
    list(name = "Ridge/Lasso (glmnet)", spec = function() parsnip::linear_reg(penalty = 0.1, mixture = 0.5) |> parsnip::set_engine("glmnet"), normalized = TRUE),
    list(name = "Random forest (ranger)", spec = function() parsnip::rand_forest(trees = 300) |> parsnip::set_engine("ranger", importance = "permutation", num.threads = 1) |> parsnip::set_mode("regression"), normalized = FALSE)
  ),
  classification_binary = list(
    list(name = "Logistic regression", spec = function() parsnip::logistic_reg() |> parsnip::set_engine("glm"), normalized = TRUE),
    list(name = "Regularised logistic (glmnet)", spec = function() parsnip::logistic_reg(penalty = 0.1, mixture = 0.5) |> parsnip::set_engine("glmnet"), normalized = TRUE),
    list(name = "Random forest (ranger)", spec = function() parsnip::rand_forest(trees = 300) |> parsnip::set_engine("ranger", importance = "permutation", num.threads = 1) |> parsnip::set_mode("classification"), normalized = FALSE)
  )
)
ml_engines$classification_multiclass <- ml_engines$classification_binary[2:3]

train_single_model <- function(df, target, features, problem_type, engine, seed = DEFAULT_SEED,
                               n_cv = if (nrow(df) < 500) 3 else 5) {
  set.seed(seed)
  split <- rsample::initial_split(df, prop = 0.8, strata = if (grepl("classification", problem_type)) target else NULL)
  df_train <- rsample::training(split)
  df_test <- rsample::testing(split)
  rec <- build_ml_recipe(df_train, target, features, problem_type, normalize = engine$normalized)
  spec <- engine$spec()
  wf <- workflows::workflow() |> workflows::add_recipe(rec) |> workflows::add_model(spec)
  fitted <- suppressWarnings(tryCatch(
    workflows::fit(wf, df_train),
    error = function(e) stop(paste0("Model '", engine$name, "' failed to train: ", conditionMessage(e)), call. = FALSE)
  ))
  pred <- predict(fitted, df_test)
  test_eval <- df_test
  if (problem_type == "regression") {
    test_eval$.pred <- pred$.pred
    mtr <- yardstick::metrics(test_eval, truth = !!rlang::sym(target), estimate = .pred)
  } else {
    pred_class <- predict(fitted, df_test, type = "class")
    test_eval$.pred_class <- pred_class$.pred_class
    mtr <- yardstick::metrics(test_eval, truth = !!rlang::sym(target), estimate = .pred_class)
    cm <- yardstick::conf_mat(test_eval, truth = !!rlang::sym(target), estimate = .pred_class)
    if (identical(problem_type, "classification_binary")) {
      prob_cols <- tryCatch(predict(fitted, df_test, type = "prob"), error = function(e) NULL)
      if (!is.null(prob_cols) && ncol(prob_cols) >= 2) {
        first_level <- levels(factor(df_train[[target]]))[1]
        prob_col <- grep(first_level, names(prob_cols), fixed = TRUE, value = TRUE)
        if (length(prob_col)) {
          test_eval$.pred_prob <- prob_cols[[prob_col[1]]]
          auc_row <- tryCatch(
            yardstick::roc_auc(test_eval, truth = !!rlang::sym(target), .pred_prob, event_level = "first"),
            error = function(e) NULL)
          if (!is.null(auc_row)) mtr <- rbind(mtr, auc_row)
        }
      }
    }
  }
  # cross-validated performance on the training data (for comparison)
  cv_res <- tryCatch({
    folds <- rsample::vfold_cv(df_train, v = n_cv, strata = if (grepl("classification", problem_type)) target else NULL)
    res <- tune::fit_resamples(wf, folds)
    tune::collect_metrics(res)
  }, error = function(e) NULL)

  list(engine = engine, split = split, fitted_workflow = fitted,
       metrics_test = if (problem_type == "regression") mtr else mtr,
       cv_metrics = cv_res,
       confusion = if (!grepl("classification", problem_type)) NULL else cm,
       n_train = nrow(df_train), n_test = nrow(df_test), seed = seed)
}

bare_metrics_frame <- function() {
  data.frame(metric = character(0), estimate = numeric(0), stringsAsFactors = FALSE)
}

regression_feature_importance <- function(model_result) {
  fit <- purrr::pluck(model_result, "fitted_workflow", "fit", "fit", "fit")
  if (is.null(fit)) return(NULL)
  if (inherits(fit, "ranger")) {
    imp <- fit$variable.importance
    data.frame(feature = names(imp), importance = as.numeric(imp), stringsAsFactors = FALSE) |>
      dplyr::arrange(dplyr::desc(importance))
  } else if (inherits(fit, "glmnet")) {
    co <- stats::coef(fit, s = 0.1)
    df <- data.frame(feature = rownames(co), importance = abs(as.numeric(co)), stringsAsFactors = FALSE)
    df |>
      dplyr::filter(feature != "(Intercept)", importance > 0) |>
      dplyr::arrange(dplyr::desc(importance))
  } else if (inherits(fit, "lm")) {
    co <- stats::coef(summary(fit))
    df <- data.frame(feature = rownames(co), importance = abs(co[, 3]), stringsAsFactors = FALSE)
    df |>
      dplyr::filter(feature != "(Intercept)") |>
      dplyr::arrange(dplyr::desc(importance)) |>
      `colnames<-`(c("feature", "importance"))
  } else NULL
}

# ---- Clustering -------------------------------------------------------------

run_clustering <- function(df, features, k = 3, seed = DEFAULT_SEED, max_rows = 5000) {
  features <- intersect(features, names(df))
  if (!length(features)) stop("Select at least one feature for clustering.", call. = FALSE)
  dat <- df[, features, drop = FALSE]
  dat <- dat[stats::complete.cases(dat), , drop = FALSE]
  if (nrow(dat) < 10) stop("At least 10 complete rows are required for clustering.", call. = FALSE)
  sampled <- FALSE
  if (nrow(dat) > max_rows) {
    set.seed(seed)
    dat <- dat[sample.int(nrow(dat), max_rows), , drop = FALSE]
    sampled <- TRUE
  }
  numcols <- vapply(dat, is.numeric, logical(1))
  if (!any(numcols)) stop("Clustering needs at least one numeric feature.", call. = FALSE)
  X <- scale(dat[, numcols, drop = FALSE])
  if (any(names(numcols)[numcols] %in% names(dat)[!numcols])) {
    fct <- dat[, !numcols, drop = FALSE]
    dummy <- as.data.frame(stats::model.matrix(~ . - 1, data = fct))
    X <- cbind(as.data.frame(X), dummy)
  }
  set.seed(seed)
  km <- stats::kmeans(X, centers = min(k, nrow(dat) - 1L), nstart = 10, iter.max = 100)
  sil <- tryCatch(
    mean(cluster::silhouette(km$cluster, stats::dist(X))[, 3]),
    error = function(e) NA_real_)
  profiles <- as.data.frame(cbind(cluster = km$cluster, dat[, numcols, drop = FALSE])) |>
    dplyr::group_by(cluster) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), \(z) round(mean(z, na.rm = TRUE), 2)), .groups = "drop") |>
    as.data.frame()
  distances <- stats::dist(X)
  hc <- tryCatch(stats::hclust(distances, method = "ward.D2"), error = function(e) NULL)
  list(kmeans = km, silhouette = sil, profiles = profiles, X = X, data = dat,
       hierarchical = hc, sampled = sampled, seed = seed)
}
