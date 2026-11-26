# Marketing Data Intelligence

A local-first marketing analytics platform built in R with Shiny. Upload a
marketing, sales, or customer dataset (CSV/XLS/XLSX) and the application walks
you through data quality, cleaning, EDA, marketing KPIs, statistical tests,
predictive models, forecasting, rule-based insights, and report exports —
without any external API, account, or paid service.

**All sample data shipped with the app is synthetic and clearly labelled.**

## Feature map

| Tab | What it does |
| --- | --- |
| Overview | Dataset snapshot and guided workflow |
| Data Upload | Validated CSV/XLS/XLSX import, sheet selection, preview (100 rows), column rename, dataset info |
| Data Quality | 12+ checks and a transparent 0–100 internal diagnostic score with breakdown |
| Data Cleaning | Previewable operations: dedupe, drop/rename columns, impute (mean/median/mode/missing), type conversion, category standardisation, whitespace trim, safe row filters, outlier removal (1.5×IQR), log/sqrt/z-score transforms; reset-to-original |
| EDA | Numeric/categorical summaries (mean, median, sd, quartiles, IQR, skewness, kurtosis), missingness table + chart |
| Visualizations | Chart builder: histogram, density, box, bar, scatter, correlation heatmap, time series (ggplot2 + plotly) |
| Marketing Analytics | Auto column-role detection with manual mapping; CTR, CPC, CPM, conversion rate, CPL, CPA, ROAS, revenue/lead via safe division; campaign/channel comparisons, trends, funnel, efficiency ranking, budget shares, filters |
| Statistical Analysis | Pearson/Spearman, chi-square (Cramér's V), independent/paired t-tests (Cohen's d), Mann-Whitney, ANOVA (η²), Kruskal-Wallis, linear/multiple regression with VIF + residual checks; hypotheses stated for every test |
| Machine Learning | Regression / binary & multiclass classification / k-means + hierarchical; tidymodels recipes fitted on training data only (no leakage); RMSE/MAE/R², accuracy/precision/recall/F1/ROC AUC, confusion matrix, feature importance, silhouette |
| Forecasting | Naive, seasonal-naive, ETS, ARIMA; day/week/month aggregation; chronological holdout backtest; prediction intervals |
| AI-Style Insights | Deterministic rule engine with title, explanation, evidence, action, and limitation per insight; LLM-ready interface |
| Reports | Excel workbook, CSV, HTML (R Markdown); PDF when LaTeX available (clear error otherwise) |
| Settings | Light/dark theme, upload limits, SQLite session persistence |

## Quick start (Windows)

See [docs/INSTALL_WINDOWS.md](docs/INSTALL_WINDOWS.md). Summary:

```bat
Rscript run_app.R
```

then open http://127.0.0.1:3838.

## Deploying to shinyapps.io

```bat
REM set credentials first (never commit them)
set SHINYAPPS_NAME=<your-account>
set SHINYAPPS_TOKEN=<token>
set SHINYAPPS_SECRET=<secret>
Rscript deploy.R
```

See [docs/DEPLOYMENT.md](docs/DEPLOYMENT.md) for free-tier limits and a
Docker/ShinyProxy path.

## Waking the hosted app (shinyapps.io)

Free-tier apps sleep after ~15 minutes of inactivity. To warm the app on
demand before you use it:

```bat
Rscript tools/wake_app.R
```

The script sends one request (which starts the instance), polls until the
application page is served, and reports how long the wake-up took. Do not run
it in a loop as a keep-alive pinger - see docs/DEPLOYMENT.md for why.

## Tests

```bat
Rscript tests/testthat.R
```

Covers safe division, KPI maths (including zero-denominator behaviour), data
quality scoring, cleaning operations, file import (CSV/semicolon/Excel),
statistical tests, ML training with a no-leakage assertion, forecasting with a
chronological holdout, insight generation, report generation, SQLite, and
Shiny module integration flows.

## Troubleshooting

- **shiny >= 1.14**: when R/ autoloading is active (the default), shiny no longer
  evaluates `global.R` itself. This app handles both behaviours: `R/zzz_mdi_bootstrap.R`
  is auto-loaded and sources `global.R` exactly once (older shiny versions also work).
- **HTML report fails to render**: rmarkdown needs Pandoc; on Windows the Pandoc
  bundled with RStudio is used automatically. PDF reports additionally require
  LaTeX (TinyTeX); if it is absent the app shows an explicit error instead of
  failing silently — HTML and Excel remain fully functional.
- **Package versions**: `renv.lock` pins the tested stack (R 4.4.3, shiny 1.14.0,
  bslib 0.12.0, tidymodels 1.4.1, forecast 9.0.2, …). Note bslib >= 0.9 only
  accepts `bs_theme(version = 5, preset = ...)`, not `bootswatch =` together with `preset =`.

## Documentation

- [docs/INSTALL_WINDOWS.md](docs/INSTALL_WINDOWS.md) — installation & troubleshooting
- [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) — module map and data flow
- [docs/DATA_PROCESSING.md](docs/DATA_PROCESSING.md) — quality score formula and cleaning operations
- [docs/MACHINE_LEARNING.md](docs/MACHINE_LEARNING.md) — models, metrics, safeguards
- [docs/DEPLOYMENT.md](docs/DEPLOYMENT.md) — cloud deployment options (ShinyApps.io, ShinyProxy)
- [docs/SECURITY.md](docs/SECURITY.md) — file handling, privacy, secrets

## License

Provided for the project owner's use. All sample datasets are synthetic.
