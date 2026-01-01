-- The vec extension is loaded by the application code, this is for manual setup.
-- SELECT load_extension('vec0');

-- Create the main documents table
CREATE TABLE IF NOT EXISTS documents (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at TEXT DEFAULT (strftime('%Y-%m-%d %H:%M:%f', 'now')),
    tags TEXT,
    source_id TEXT UNIQUE,
    covers TEXT,
    links_to TEXT,
    slug TEXT,
    source TEXT NOT NULL,
    meta TEXT,
    content TEXT NOT NULL,
    title TEXT NOT NULL,
    type TEXT
);

-- Create the virtual table for vector search using sqlite-vec
CREATE VIRTUAL TABLE IF NOT EXISTS vec_documents USING vec0(
  embedding FLOAT[768]
);

-- Add indexes for faster searching on the main table
CREATE INDEX IF NOT EXISTS idx_documents_source ON documents (source);
CREATE INDEX IF NOT EXISTS idx_documents_type ON documents (type);
CREATE INDEX IF NOT EXISTS idx_documents_slug ON documents (slug);
CREATE INDEX IF NOT EXISTS idx_documents_source_id ON documents (source_id);

