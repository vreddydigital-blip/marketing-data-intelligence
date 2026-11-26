# Machine Learning Documentation

## Workflow

1. Select target → the app detects the problem type (numeric target =
   regression; 2 levels = binary classification; >2 = multiclass).
2. Select predictors (ID-like columns with ~no repeated values are blocked
   with an explicit warning).
3. Data is split 80/20 (`rsample::initial_split`, stratified on the target for
   classification; seed configurable, default 42).
4. A `recipes` recipe is created **from the training data only**: zero-variance
   filter → median imputation (numeric) → mode imputation (categorical) →
   dummy encoding → normalisation (for regularised/linear models).
5. Model trains inside a `workflows` object; predictions are made on the test
   set; test metrics and optional cross-validated metrics are reported.

## Models

| Problem | Models |
| --- | --- |
| Regression | Linear regression (lm), Ridge/Lasso (glmnet, penalty 0.1), Random forest (ranger, 300 trees) |
| Binary classification | Logistic regression (glm), Regularised logistic (glmnet), Random forest |
| Multiclass classification | Regularised logistic, Random forest |
| Clustering | k-means (scaled features, labelled profiles, silhouette, PCA projection); hierarchical clustering (Ward.D2) built for exploration |

## Metrics

- Regression: RMSE, MAE, R² on held-out test rows.
- Classification: accuracy, precision, recall, F1 (yardstick), ROC AUC for
  binary problems, confusion matrix.
- Clustering: cluster sizes, per-cluster numeric profiles, mean silhouette.

## Safeguards

- Preprocessing is fitted **only on training data** (enforced structurally by
  the recipe; a test asserts the fitted recipe template has exactly n_train rows).
- Training never runs automatically; it is button-triggered, so reactive data
  changes cannot trigger surprise model fits.
- Data below `MIN_ROWS_REGRESSION` / `MIN_ROWS_CLASSIFICATION` (30) is refused
  before training; constant targets and 1-class classification are refused.
- Seeds are set explicitly on split, CV folds, model fitting, resampling and
  clustering, making runs reproducible.
- Predictions are presented as estimates; no guaranteed outcomes are claimed.
- Regularised models use a fixed penalty (0.1); tuning is a documented next step.

## Feature importance

- Random forest: permutation importance (ranger).
- glmnet: absolute coefficients at s = 0.1.
- Linear models: absolute t-statistics.
