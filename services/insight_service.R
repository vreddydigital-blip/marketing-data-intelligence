# Deterministic, rule-based insight engine. Every insight includes a title,
# an explanation, supporting evidence computed from the data, a recommended
# action, and a confidence/limitation note. No paid AI API is required; the
# engine interface (list of insight objects) is designed so an LLM provider
# can be plugged in later behind the same contract.

insight_frame <- function(rows) {
  cols <- c("title", "explanation", "metric", "evidence", "action", "confidence")
  rows <- lapply(rows, function(r) {
    out <- lapply(cols, function(c1) {
      v <- r[[c1]]
      if (is.null(v) || length(v) == 0) NA_character_ else as.character(v)[1]
    })
    stats::setNames(out, cols)
  })
  do.call(rbind, lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
}

empty_insight <- function(dataset_name) {
  insight_frame(list(list(
    title = "Not enough evidence in this dataset yet",
    explanation = paste0("The current dataset '", dataset_name, "' does not contain the columns required for",
                         " campaign, channel, time-series, or correlation analysis, so no specific insights can be generated."),
    metric = NA_character_, evidence = NA_character_, action = NA_character_,
    confidence = "No statistical evidence available."
  )))
}

#' @param state A list with named elements: df (current data frame), quality,
#'   kpi_summary (list or NULL), correlations (matrix or NULL),
#'   forecast_summary (list or NULL), model_metrics (data.frame or NULL).
generate_insights <- function(state, max_insights = 12) {
  out <- list()
  add <- function(title, explanation, metric, evidence, action, confidence) {
    out[[length(out) + 1L]] <<- list(
      title = title, explanation = explanation, metric = metric,
      evidence = evidence, action = action, confidence = confidence)
  }

  df <- state$df
  if (is.null(df) || nrow(df) == 0) return(empty_insight("(none)"))
  name <- state$name %||% "dataset"

  # --- data quality insights -------------------------------------------------
  q <- state$quality
  if (!is.null(q)) {
    miss_pct <- 100 * (sum(is.na(df)) / (nrow(df) * ncol(df)))
    col_miss <- vapply(df, function(x) mean(is.na(x)), numeric(1))
    worst_col <- names(which.max(col_miss))
    worst_share <- 100 * max(col_miss)
    if (worst_share > 20) {
      add("High missingness in individual columns",
          sprintf("Column '%s' in '%s' is %.0f%% missing (%.1f%% of all cells overall).", worst_col, name,
                  worst_share, miss_pct),
          metric = "Missing values in worst column",
          evidence = sprintf("'%s' has %.0f%% missing values", worst_col, worst_share),
          action = "Review the Data Quality tab and repair columns with high missingness before drawing conclusions.",
          confidence = "Deterministic count from the loaded data.")
    } else if (miss_pct > 5) {
      add("Material missing data present",
          sprintf("%.1f%% of all cells in '%s' are missing. Marketing KPIs computed on rows with missing drivers will be based on smaller samples.",
                  miss_pct, name),
          metric = "Missing cell share",
          evidence = sprintf("%.0f of %d cells", sum(is.na(df)), nrow(df) * ncol(df)),
          action = "Review the Data Quality tab and repair columns with high missingness before drawing conclusions.",
          confidence = "Deterministic count from the loaded data.")
    }
    if (!is.null(q$metrics) && q$metrics$duplicate_rows > 0) {
      add("Duplicate rows detected",
          sprintf("%d fully duplicated rows exist; they inflate totals like spend and conversions if not removed.",
                  q$metrics$duplicate_rows),
          metric = "Duplicate rows",
          evidence = sprintf("%d rows", q$metrics$duplicate_rows),
          action = "Use the Data Cleaning tab: 'Remove duplicate rows'.",
          confidence = "Deterministic duplicate check on all columns.")
    }
  }

  # --- marketing KPI insights ------------------------------------------------
  ks <- state$kpi_summary
  mapping <- state$mapping
  if (!is.null(ks) && !is.null(mapping)) {
    grp_cols <- c("campaign", "channel", "source")
    for (g in grp_cols) {
      if (is.null(mapping[[g]])) next
      perf <- grouped_performance(df, mapping, mapping[[g]])
      if (is.null(perf) || nrow(perf) < 2) next
      if (!is.null(perf$roas) && !all(is.na(perf$roas)) && sum(!is.na(perf$spend)) > 1) {
        i <- which.max(perf$roas)
        j <- which.min(perf$roas)
        if (!is.na(perf$roas[i]) && perf$roas[i] > 0) {
          spend_total <- sum(perf$spend, na.rm = TRUE)
          if (spend_total > 0) {
            add(sprintf("'%s' is the most spend-efficient %s by ROAS",
                        as.character(perf[[g]][i]), g),
                sprintf("%s returned $%.2f of revenue per $1 of spend, versus $%.2f for the weakest %s '%s'. Both are measured over the same periods.",
                        as.character(perf[[g]][i]), perf$roas[i], perf$roas[j], g, as.character(perf[[g]][j])),
                metric = "ROAS = revenue / spend",
                evidence = sprintf("%s ROAS %.2f vs %.2f (weakest), total spend %.0f",
                                   as.character(perf[[g]][i]), perf$roas[i], perf$roas[j], spend_total),
                action = sprintf("Consider shifting incremental budget toward '%s' while monitoring saturation.", as.character(perf[[g]][i])),
                confidence = paste("Descriptive comparison; does not adjust for audience quality, seasonality, or chance.",
                                   "Correlation is not causation."))
          }
        }
      }
      if (!is.na(perf$spend[1])) {
        if (all(!is.na(perf$cpa))) {
          i <- which.min(perf$cpa)
          if (!is.na(perf$cpa[i])) {
            add(sprintf("'%s' converts at the lowest cost (%s, %s = %s)",
                        as.character(perf[[g]][i]), g, "CPA", format(perf$cpa[i] %||% NA, digits = 3)),
                sprintf("Cost per conversion for '%s' is $%.2f, the lowest of %d %s(s).",
                        as.character(perf[[g]][i]), perf$cpa[i], nrow(perf), g),
                metric = "CPA = spend / conversions",
                evidence = sprintf("CPA range %.2f - %.2f across %s(s)",
                                   min(perf$cpa, na.rm = TRUE), max(perf$cpa, na.rm = TRUE), g),
                action = sprintf("Benchmark other %s(s) against '%s' and test its creative/offer.", g, as.character(perf[[g]][i])),
                confidence = "Descriptive comparison based on current data volume.")
          }
        }
      }
    }
    # funnel leak
    if (!is.null(ks$kpis$ctr) && !is.na(ks$kpis$ctr)) {
      cr <- ks$kpis$conversion_rate
      if (!is.null(cr) && !is.na(cr) && cr < 2) {
        add("Click-to-conversion rate below 2%",
            sprintf("Across all traffic, %.2f%% of clicks convert. Industry medians for most channels sit above this, so a significant share of paid clicks is not converting.",
                    cr),
            metric = "Conversion Rate = conversions / clicks x 100",
            evidence = sprintf("%.0f clicks - %.0f conversions", ks$totals$clicks, ks$totals$conversions),
            action = "Inspect landing pages and offer relevance for the weakest channels before increasing spend.",
            confidence = "Aggregate observation; benchmark ranges vary by industry and season.")
      }
    }
  }

  # --- correlation insights ----------------------------------------------------
  corr <- state$correlations
  if (!is.null(corr) && ncol(corr) >= 2) {
    m <- as.matrix(corr)
    diag(m) <- NA
    if (any(!is.na(m))) {
      max_pair <- which(abs(m) == max(abs(m), na.rm = TRUE), arr.ind = TRUE)[1, ]
      r <- m[max_pair[1], max_pair[2]]
      if (abs(r) >= 0.5 && max_pair[1] != max_pair[2]) {
        add(sprintf("Strong %s correlation between '%s' and '%s'",
                    if (r > 0) "positive" else "negative", colnames(corr)[max_pair[1]], colnames(corr)[max_pair[2]]),
            sprintf("Pearson r = %.2f. This is an association in this dataset only; it does not prove that one drives the other.",
                    r),
            metric = "Pearson r",
            evidence = sprintf("r = %.3f (n = %d rows)", r, nrow(df)),
            action = "Test the relationship experimentally (e.g. holdout channels, A/B tests) before treating it as causal.",
            confidence = "Correlation does not imply causation.")
      }
    }
  }

  # --- time-series / jump insights --------------------------------------------
  if (!is.null(mapping) && !is.null(mapping$date)) {
    for (metric_role in c("spend", "revenue", "clicks")) {
      col <- mapping[[metric_role]]
      if (is.null(col)) next
      series <- tryCatch(daily_series(df, mapping, freq = "day"), error = function(e) NULL)
      if (is.null(series) || nrow(series) < 10) break
      vals <- series[[metric_role]]
      jumps <- tryCatch(spend_jumps(series$period, vals), error = function(e) NULL)
      if (!is.null(jumps) && nrow(jumps) > 0) {
        worst <- jumps[which.max(abs(jumps$change_pct)), ]
        add(sprintf("Sudden %.0f%% %s in %s on %s", abs(worst$change_pct),
                    if (worst$change_pct > 0) "increase" else "decrease", metric_role, worst$period),
            sprintf("%s moved by %+.1f%% from the previous day, exceeding the +/-25%% observation threshold.",
                    metric_role, worst$change_pct),
            metric = metric_role,
            evidence = sprintf("Day-over-day change on %s: %+.1f%%", worst$period, worst$change_pct),
            action = "Verify tracking setup, campaign flights, or external events around that date.",
            confidence = "Threshold-based detection; may reflect normal volatility.")
        break
      }
    }
    fs <- state$forecast_summary
    if (!is.null(fs) && !is.null(fs$direction)) {
      add(sprintf("Forecast: %s trend for %s", fs$direction, fs$metric),
          sprintf("The %s model projects %s for '%s' over the next %d periods (based on %d historical periods).",
                  fs$best_model, fs$direction_text, fs$metric, fs$horizon, fs$n),
          metric = fs$metric,
          evidence = sprintf("Best holdout RMSE: %s (%s model)", format(fs$best_rmse, digits = 4), fs$best_model),
          action = "Use the forecast with its prediction intervals for planning, not as a guarantee.",
          confidence = "Forecast from historical patterns; actuals may differ, especially on short history.")
    }
  }

  # --- model insights -----------------------------------------------------------
  mm <- state$model_metrics
  if (!is.null(mm) && nrow(mm) > 0) {
    add(sprintf("Best model: %s", mm$model[1]),
        sprintf("On the held-out test data, '%s' scored %s. Model performance is an estimate; it can degrade on new data.",
                mm$model[1], mm$summary[1]),
        metric = "Held-out performance",
        evidence = paste(vapply(seq_len(min(2, nrow(mm))), function(i) sprintf("%s: %s", mm$model[i], mm$summary[i]), character(1)), collapse = "; "),
        action = "Before operational use, re-validate on more recent data and monitor drift.",
        confidence = "Single train/test split with fixed seed 42; modest sample sizes produce uncertainty.")
  }

  if (!length(out)) return(empty_insight(name))
  insight_frame(out[seq_len(min(length(out), max_insights))])
}
