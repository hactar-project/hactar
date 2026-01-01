-- Create the errors table
CREATE TABLE IF NOT EXISTS errors (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at TEXT DEFAULT (strftime('%Y-%m-%d %H:%M:%f', 'now')),
    code TEXT NOT NULL,
    stack TEXT,
    slug TEXT NOT NULL,
    title TEXT NOT NULL,
    message TEXT NOT NULL,
    cause TEXT NOT NULL,
    solution TEXT NOT NULL,
    tags TEXT
);

-- Create the virtual table for vector search using sqlite-vec
CREATE VIRTUAL TABLE IF NOT EXISTS vec_errors USING vec0(
  embedding FLOAT[768]
);

-- Add index for code
CREATE INDEX IF NOT EXISTS idx_errors_code ON errors (code);
CREATE INDEX IF NOT EXISTS idx_errors_stack ON errors (stack);
CREATE INDEX IF NOT EXISTS idx_errors_slug ON errors (slug);
