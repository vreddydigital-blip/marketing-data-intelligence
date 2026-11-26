-- Marketing Data Intelligence - SQLite schema.
-- Designed so the abstraction layer can later point at PostgreSQL.

CREATE TABLE IF NOT EXISTS app_settings (
  key         TEXT PRIMARY KEY,
  value       TEXT NOT NULL,
  updated_at  TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE TABLE IF NOT EXISTS datasets (
  id            INTEGER PRIMARY KEY AUTOINCREMENT,
  name          TEXT NOT NULL,
  display_name  TEXT NOT NULL,
  n_rows        INTEGER NOT NULL,
  n_cols        INTEGER NOT NULL,
  size_bytes    INTEGER,
  is_sample     INTEGER NOT NULL DEFAULT 0,
  stored_path   TEXT,
  created_at    TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE TABLE IF NOT EXISTS dataset_metadata (
  dataset_id  INTEGER NOT NULL REFERENCES datasets(id),
  key         TEXT NOT NULL,
  value       TEXT NOT NULL,
  PRIMARY KEY (dataset_id, key)
);

CREATE TABLE IF NOT EXISTS cleaning_operations (
  id          INTEGER PRIMARY KEY AUTOINCREMENT,
  dataset_id  INTEGER REFERENCES datasets(id),
  operation   TEXT NOT NULL,
  details     TEXT,
  created_at  TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE TABLE IF NOT EXISTS analysis_history (
  id          INTEGER PRIMARY KEY AUTOINCREMENT,
  dataset_id  INTEGER REFERENCES datasets(id),
  module      TEXT NOT NULL,
  summary     TEXT NOT NULL,
  created_at  TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE TABLE IF NOT EXISTS model_runs (
  id            INTEGER PRIMARY KEY AUTOINCREMENT,
  dataset_id    INTEGER REFERENCES datasets(id),
  problem_type  TEXT NOT NULL,
  target        TEXT NOT NULL,
  model_name    TEXT NOT NULL,
  metrics_json  TEXT NOT NULL,
  seed          INTEGER,
  n_train       INTEGER,
  n_test        INTEGER,
  created_at    TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE TABLE IF NOT EXISTS report_history (
  id           INTEGER PRIMARY KEY AUTOINCREMENT,
  dataset_id   INTEGER REFERENCES datasets(id),
  report_type  TEXT NOT NULL,
  file_name    TEXT NOT NULL,
  created_at   TEXT NOT NULL DEFAULT (datetime('now'))
);
