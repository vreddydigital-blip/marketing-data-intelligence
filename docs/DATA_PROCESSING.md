# Data Processing Documentation

## Import

- CSV: readr with UTF-8 first, Latin-1 fallback, then semicolon delimiter probing.
- XLS/XLSX: readxl with sheet selection (the first sheet is the default).
- Extensions alone are not trusted: the reader validates structure and refuses
  empty/one-column/non-tabular content with explicit messages.
- Column names are trimmed; duplicate names, empty columns, and date columns
  are detected and shown in the dataset information panel.

## Data Quality Score (internal diagnostic)

**This score is an internal diagnostic produced by this application. It is not
an industry-standard benchmark.**

| Component | Max | Formula |
| --- | --- | --- |
| Completeness | 40 | 40 × (1 − share of missing cells) |
| Uniqueness | 20 | 20 × (1 − share of duplicated rows) |
| Type consistency | 20 | 20 × (1 − risky cells / total cells), risky = numbers stored as text |
| Validity | 20 | 20 × (1 − invalid cells / total cells); invalid = negative values in detected business metrics + whitespace-contaminated cells |

Checks emitted as issues: missing values, duplicate rows, duplicate column
names, empty columns, constant columns, negative business metrics, leading /
trailing whitespace, numbers stored as text.

## Cleaning operations

Every operation is previewed (rows/columns before → after) and applied only on
confirmation. The original upload never changes; "Reset to original" restores
it. Each applied op is appended to the session history (and the SQLite log
when a session is saved).

| Operation | Behaviour |
| --- | --- |
| Remove duplicate rows | full-row `distinct()` |
| Remove columns | multi-select |
| Rename column | single mapping with preview |
| Replace missing values | fixed value per selected column |
| Impute numeric | mean or median of observed values |
| Impute categorical | mode or explicit "Missing" category |
| Convert types | numeric (strips currency symbols), character, date (multi-format + Excel serials), factor |
| Standardise categories | lower / title_case / upper / snake_case |
| Trim whitespace | selected or all text columns |
| Filter rows | structured column/operator/value conditions (no raw code evaluation) |
| Remove outliers | 1.5 × IQR rule per selected numeric column |
| Transform | log (log1p-safe), sqrt (clipped at 0), z-score |

## KPI rules

KPIs are computed from **summed numerators and denominators** so aggregate
ratios are correct weighted averages rather than means of row ratios. Row-level
KPIs use `safe_div`, which returns `NA` when the denominator is 0 or missing —
never `Inf`. KPIs whose inputs are absent are hidden from the dashboard, and
an explanatory note lists the formulas that could not be computed.
