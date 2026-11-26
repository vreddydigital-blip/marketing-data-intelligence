# Security and Privacy Notes

## File handling

- Only `.csv`, `.xls`, `.xlsx` are accepted; both extension and structure are
  validated, and the size limit is enforced (default 50 MB, configurable).
- Uploaded content is parsed as **data only**. Nothing from an upload is
  evaluated, executed, or injected into code paths.
- Row filters are structured (column/operator/value), never raw expressions.
- Internal filenames are generated server-side via `safe_internal_name()`
  (alphanumeric + timestamp), so client filenames can never traverse paths.
- Uploads stay in OS temporary directories; no server filesystem paths are
  exposed in the UI.

## Data retention

- Uploaded data is held in memory for the session only. The SQLite database
  stores metadata, history, and model metrics **unless** the user explicitly
  ticks "store the current dataset" when saving a session.
- Reports are written to `data/reports/` and can be deleted with the folder.

## Text handling

- All user-controlled strings rendered in the UI pass through `esc()`
  (htmltools::htmlEscape) — including dataset names, labels, insight text.
- DT tables render as HTML by default; application-generated strings are
  escaped before display.

## Secrets and credentials

- No API keys exist in the initial version; config comes from environment
  variables (`.Renviron`, see `.Renviron.example`), which is git-ignored.
- No authentication yet. When added, use an established package (e.g.
  shinymanager or a reverse-proxy/SSO in front of the app) — never a custom
  password scheme. Passwords must be salted-hashed (e.g. sodium/bcrypt).
- Logging records events (row/column counts, error messages) — never dataset
  contents, values, or credentials. Stack traces go to the log, not the UI.

## Database

- All SQL is parameterised (DBI `?` placeholders). No string interpolation
  into queries.
- SQLite in a local file; when PostgreSQL is adopted later, credentials come
  from environment variables and TLS is terminated by the infrastructure.
