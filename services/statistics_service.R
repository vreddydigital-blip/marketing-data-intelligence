# Statistical tests with hypothesis statements, effect sizes, assumption
# checks, and plain-language interpretation. Results are data frames for
# display and reporting.

TEST_HYPOTHESES <- list(
  corr_pearson = list(H0 = "There is no linear correlation between the two variables (rho = 0).",
                      H1 = "There is a linear correlation between the two variables (rho != 0)."),
  corr_spearman = list(H0 = "The two variables are not monotonically correlated.",
                       H1 = "The two variables are monotonically correlated."),
  chi_square = list(H0 = "The two categorical variables are independent.",
                    H1 = "The two categorical variables are associated."),
  t_test = list(H0 = "The means of the two groups are equal.",
                H1 = "The means of the two groups are not equal."),
  t_test_paired = list(H0 = "The mean of the paired differences is zero.",
                       H1 = "The mean of the paired differences is not zero."),
  mann_whitney = list(H0 = "The distributions of the two groups are equal.",
                      H1 = "The distributions of the two groups differ."),
  anova = list(H0 = "All group means are equal.",
               H1 = "At least one group mean differs."),
  kruskal = list(H0 = "All group distributions are identical.",
                 H1 = "At least one group distribution differs."),
  regression = list(H0 = "All predictor coefficients are zero (the model has no explanatory power).",
                    H1 = "At least one predictor coefficient is non-zero.")
)

correlation_analysis <- function(df, col_a, col_b, method = c("pearson", "spearman")) {
  method <- match.arg(method)
  x <- as.numeric(df[[col_a]]); y <- as.numeric(df[[col_b]])
  ok <- !is.na(x) & !is.na(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 3) stop("At least 3 complete pairs are required for correlation analysis.", call. = FALSE)
  res <- stats::cor.test(x, y, method = method)
  tidy <- broom::tidy(res)
  data.frame(
    test = sprintf("%s correlation", method),
    statistic = tidy$statistic,
    estimate = tidy$estimate,
    ci_low = tidy$conf.low, ci_high = tidy$conf.high,
    p_value = tidy$p.value,
    n = length(x),
    effect_size = sprintf("r = %.3f (|r| >= 0.1 small, >= 0.3 moderate, >= 0.5 strong)", tidy$estimate),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

chi_square_test <- function(df, col_a, col_b) {
  tab <- table(df[[col_a]], df[[col_b]])
  if (any(dim(tab) < 2)) stop("Chi-square requires at least 2 levels in each variable.", call. = FALSE)
  res <- suppressWarnings(stats::chisq.test(tab))
  n <- sum(tab)
  k <- min(dim(tab))
  cramers_v <- sqrt(as.numeric(res$statistic) / (n * (k - 1)))
  low_expected <- mean(res$expected < 5)
  data.frame(
    test = "Chi-square test of independence",
    statistic = unname(res$statistic), parameter = unname(res$parameter),
    p_value = res$p.value,
    effect_size = sprintf("Cramer's V = %.3f (0.1 small, 0.3 moderate, 0.5 large)", cramers_v),
    n = n,
    warning = if (low_expected > 0.2) "More than 20% of expected cell counts are below 5; interpret with caution (consider collapsing categories)."
              else if (any(res$expected < 5)) "Some expected cell counts are below 5; interpret with caution." else ""
  )
}

t_test_contrast <- function(df, value_col, group_col, paired = FALSE) {
  x <- df[[value_col]]
  g <- as.character(df[[group_col]])
  ok <- !is.na(x) & !is.na(g)
  x <- x[ok]; g <- g[ok]
  if (length(unique(g)) < 2) stop("The grouping variable needs at least 2 groups with complete values.", call. = FALSE)
  if (paired) {
    # treat each group's sorted values as paired observations of equal length
    g1 <- sort(x[g == unique(g)[1]]); g2 <- sort(x[g == unique(g)[2]])
    m <- min(length(g1), length(g2))
    g1 <- g1[seq_len(m)]; g2 <- g2[seq_len(m)]
    if (m < 3) stop("Paired t-test needs at least 3 complete pairs.", call. = FALSE)
    res <- stats::t.test(g1, g2, paired = TRUE, conf.level = 0.95)
    d_eff <- effect_cohen_d(g1, g2, paired = TRUE)
  } else {
    g1 <- x[g == unique(g)[1]]; g2 <- x[g == unique(g)[2]]
    if (length(g1) < 2 || length(g2) < 2) stop("Each group needs at least 2 complete values.", call. = FALSE)
    var_eq <- stats::var.test(g1, g2)$p.value >= 0.05
    res <- stats::t.test(g1, g2, var.equal = var_eq, conf.level = 0.95)
    d_eff <- effect_cohen_d(g1, g2)
  }
  tidy <- broom::tidy(res)
  data.frame(
    test = if (paired) "Paired t-test" else sprintf("Independent samples t-test (Welch=%s)", !var_eq && !paired),
    statistic = tidy$statistic, parameter = tidy$parameter,
    estimate = tidy$estimate1 - tidy$estimate2,
    ci_low = tidy$conf.low, ci_high = tidy$conf.high,
    p_value = tidy$p.value,
    group1 = unique(g)[1], group2 = unique(g)[2],
    effect_size = sprintf("Cohen's d = %.3f (0.2 small, 0.5 moderate, 0.8 large)", d_eff),
    stringsAsFactors = FALSE
  )
}

effect_cohen_d <- function(g1, g2, paired = FALSE) {
  if (paired) {
    d <- g1 - g2
    d <- d[!is.na(d)]
    if (length(d) < 2 || stats::sd(d) == 0) return(NA_real_)
    mean(d) / stats::sd(d)
  } else {
    n1 <- length(g1); n2 <- length(g2)
    s1 <- stats::sd(g1); s2 <- stats::sd(g2)
    if (is.na(s1) || is.na(s2)) return(NA_real_)
    pooled <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
    if (pooled == 0) return(NA_real_)
    (mean(g1) - mean(g2)) / pooled
  }
}

mann_whitney_test <- function(df, value_col, group_col) {
  x <- df[[value_col]]; g <- as.character(df[[group_col]])
  ok <- !is.na(x) & !is.na(g)
  x <- x[ok]; g <- g[ok]
  grp <- unique(g)
  if (length(grp) != 2) stop("Mann-Whitney requires exactly 2 groups.", call. = FALSE)
  res <- suppressWarnings(stats::wilcox.test(x ~ g, conf.int = TRUE))
  tidy <- broom::tidy(res)
  rbc <- rank_biserial(x, g)
  data.frame(
    test = "Mann-Whitney U (Wilcoxon rank-sum)",
    statistic = tidy$statistic, parameter = NA_real_,
    estimate = tidy$estimate,
    ci_low = tidy$conf.low, ci_high = tidy$conf.high,
    p_value = tidy$p.value,
    effect_size = sprintf("Rank-biserial r = %.3f (|r| 0.1 small, 0.3 moderate, 0.5 large)", rbc),
    stringsAsFactors = FALSE
  )
}

rank_biserial <- function(x, g) {
  n1 <- sum(g == unique(g)[1]); n2 <- sum(g == unique(g)[2])
  u <- as.numeric(stats::wilcox.test(x ~ g)$statistic)
  1 - 2 * u / (n1 * n2)
}

anova_test <- function(df, value_col, group_col) {
  x <- df[[value_col]]; g <- as.character(df[[group_col]])
  ok <- !is.na(x) & !is.na(g)
  dat <- data.frame(x = x[ok], g = g[ok])
  fit <- stats::aov(x ~ g, data = dat)
  sm <- broom::tidy(fit)
  between <- sm[1, ]
  eta_sq <- between$sumsq / sum(sm$sumsq)
  # assumption check: Levene via car
  lev <- tryCatch(car::leveneTest(x, as.factor(g)), error = function(e) NULL)
  lev_warning <- if (!is.null(lev) && !is.na(lev$`Pr(>F)`[1]) && lev$`Pr(>F)`[1] < 0.05) {
    "Levene's test indicates unequal variances across groups; consider Kruskal-Wallis."
  } else ""
  data.frame(
    test = "One-way ANOVA (F-test)",
    statistic = between$statistic, parameter = NA_real_,
    estimate = NA_real_, ci_low = NA_real_, ci_high = NA_real_,
    p_value = between$p.value,
    effect_size = sprintf("Eta-squared = %.3f (0.01 small, 0.06 moderate, 0.14 large)", eta_sq),
    warning = lev_warning
  )
}

kruskal_test <- function(df, value_col, group_col) {
  x <- df[[value_col]]; g <- as.character(df[[group_col]])
  ok <- !is.na(x) & !is.na(g)
  res <- suppressWarnings(stats::kruskal.test(x ~ g[ok]))
  eps_sq <- as.numeric(res$statistic) / (length(x[ok]) - 1)
  data.frame(
    test = "Kruskal-Wallis rank-sum test",
    statistic = unname(res$statistic), parameter = unname(res$parameter),
    estimate = NA_real_, ci_low = NA_real_, ci_high = NA_real_,
    p_value = res$p.value,
    effect_size = sprintf("Epsilon-squared = %.3f", eps_sq),
    stringsAsFactors = FALSE
  )
}

linear_regression <- function(df, target, predictors) {
  predictors <- intersect(predictors, names(df))
  predictors <- setdiff(predictors, target)
  if (!length(predictors)) stop("Select at least one predictor variable.", call. = FALSE)
  dat <- df[, c(target, predictors)]
  dat <- dat[stats::complete.cases(dat), , drop = FALSE]
  if (nrow(dat) < length(predictors) + 10) {
    stop(sprintf("Not enough complete rows (%d) for %d predictors; need at least predictors + 10.",
                 nrow(dat), length(predictors)), call. = FALSE)
  }
  form <- stats::reformulate(predictors, response = target)
  fit <- suppressWarnings(stats::lm(form, data = dat))
  glance <- broom::glance(fit)
  coef_tab <- broom::tidy(fit, conf.int = TRUE)
  # VIF for numeric multicollinearity warning
  vif_warning <- ""
  if (length(predictors) > 1) {
    v <- tryCatch(car::vif(fit), error = function(e) NULL)
    if (!is.null(v) && max(v, na.rm = TRUE) > 5) {
      vif_warning <- sprintf("Some predictors have variance inflation factor above 5 (max %.1f); multicollinearity may inflate uncertainty.",
                             max(v, na.rm = TRUE))
    }
  }
  shapiro_note <- ""
  if (nrow(dat) <= 5000) {
    sw <- tryCatch(stats::shapiro.test(stats::residuals(fit)), error = function(e) NULL)
    if (!is.null(sw) && sw$p.value < 0.05) {
      shapiro_note <- "Residual normality is in doubt (Shapiro-Wilk p < 0.05); robustness of p-values is limited."
    }
  } else {
    shapiro_note <- "Dataset too large for Shapiro-Wilk; residuals not formally tested for normality."
  }
  list(
    fit = fit,
    glance = glance,
    coefficients = coef_tab,
    diagnostics = list(vif_warning = vif_warning, shapiro_note = shapiro_note, n_obs = nrow(dat))
  )
}
