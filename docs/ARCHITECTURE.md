# Application Architecture

## Principles

- **R-only backend**: Shiny server; no Python/Node; no JS required from the app author.
- **Thin UI, fat services**: all business logic lives in `services/` and `utils/`;
  Shiny modules (`R/mod_*.R`) only orchestrate inputs/outputs and call services.
- **One shared data state**: `app.R` creates a single `reactiveValues` "dataset"
  object (original, current, info, quality, history, version). Modules read
  `dataset$current`; cleaning ops replace it through `apply_cleaning()`, which
  refreshes derived info/quality and appends to the session history.
  The **uploaded original is immutable**; "reset to original" restores it.

## Data flow

```
Upload/Excel/CSV --> validators --> file_service::read_data_file
      |                                    |
      v                                    v
set_dataset()  --> describe_dataset() --> compute_quality_report()
      |
      v
dataset$current ----+--> EDA / Visualizations / Statistics
                    |--> analytics_service (mapping + KPIs) --> Marketing tab, Insights
                    |--> modeling_service (tidymodels)       --> ML tab
                    |--> forecasting_service                 --> Forecast tab
                    |
                    v
        cleaning ops --> apply_cleaning() --> refreshed quality/history
                    |
                    v
        report_service (Excel/CSV/HTML) + insight_service
```

## Module inventory (R/)

| File | Purpose |
| --- | --- |
| mod_overview.R | Landing snapshot + workflow steps |
| mod_upload.R | Validation, read, preview, rename |
| mod_data_quality.R | Score + issues tables |
| mod_cleaning.R | Preview-in-modal cleaning operations |
| mod_eda.R | Descriptive tables + missingness visuals |
| mod_visualizations.R | Chart builder (ggplot2/plotly) |
| mod_marketing_analytics.R | Mapping, KPI cards, filters, comparisons |
| mod_statistics.R | Test launcher + result rendering |
| mod_machine_learning.R | Supervised + clustering UI; button-triggered training |
| mod_forecasting.R | Time series setup + backtest + forecast |
| mod_insights.R | Rule-based insight cards |
| mod_reports.R | Download handlers |
| mod_settings.R | Theme, limits, SQLite session persistence |

`modules/` mirrors this structure as domain documentation folders; the
executable module code is in `R/` (single source of truth).

## Services (services/)

- `file_service.R` — import (CSV encodings/delimiters, Excel sheets), dataset description
- `quality_service.R` — quality checks and the documented 0–100 score
- `cleaning_service.R` — pure data-frame transformations
- `eda_service.R` — descriptive stats, correlation matrix, missingness
- `analytics_service.R` — marketing role detection + KPIs with safe division
- `statistics_service.R` — hypothesis tests, effect sizes, diagnostics
- `modeling_service.R` — tidymodels workflows, validation guards, clustering
- `forecasting_service.R` — aggregation, chronological backtests, forecasts
- `insight_service.R` — deterministic rule engine (LLM-swappable)
- `report_service.R` — Excel/CSV/HTML/PDF report builders

## Database (database/)

SQLite by default through `database.R` (DBI + RSQLite):
`datasets`, `dataset_metadata`, `cleaning_operations`, `analysis_history`,
`model_runs`, `report_history`, `app_settings`. All queries parameterised; the
connection factory is the single swap point for PostgreSQL.

## Performance notes

- Reactive state keys off `dataset$version` to avoid recomputation loops.
- ML/forecast/clustering run only on explicit button events, never on data change.
- Quality/EDA computations are column-vectorised; `data.table` is available
  where batch aggregation needs it.
- Uploaded files land in the OS temp dir and are read once into memory.
