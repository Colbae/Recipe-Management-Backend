CREATE TABLE IF NOT EXISTS pantry (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id INTEGER NOT NULL REFERENCES user(id),
    ingredients TEXT NOT NULL
);

CREATE INDEX IF NOT EXISTS pantries_by_user ON pantry(user_id);
