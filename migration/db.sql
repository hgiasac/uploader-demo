CREATE TABLE IF NOT EXISTS files (
  name TEXT PRIMARY KEY,
  type TEXT NOT NULL DEFAULT '',
  size BIGINT NOT NULL DEFAULT 0,
  uri TEXT NOT NULL,
  is_link INT NOT NULL DEFAULT 0,
  created_at REAL DEFAULT (datetime('now'))

);
