# Module placement note

The executable Shiny modules for this application live in the project root's
`R/` directory (`R/mod_upload.R`, `R/mod_data_quality.R`, …) because that is the
location Shiny auto-loads at startup (see `docs/ARCHITECTURE.md`).

The subdirectories referenced by the original project layout
(`data_import`, `data_quality`, `data_cleaning`, `exploratory_analysis`,
`marketing_analytics`, `statistical_analysis`, `machine_learning`,
`forecasting`, `insights`, `reporting`) are intended for future refactoring of
complex domain logic out of the flat service layer. Domain logic that already
existed at the time of this note lives in:

- `data_import` -> `services/file_service.R`
- `data_quality` -> `services/quality_service.R`
- `data_cleaning` -> `services/cleaning_service.R`
- `exploratory_analysis` -> `services/eda_service.R`
- `marketing_analytics` -> `services/analytics_service.R`
- `statistical_analysis` -> `services/statistics_service.R`
- `machine_learning` -> `services/modeling_service.R`
- `forecasting` -> `services/forecasting_service.R`
- `insights` -> `services/insight_service.R`
- `reporting` -> `services/report_service.R`
