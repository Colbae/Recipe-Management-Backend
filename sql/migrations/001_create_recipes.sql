CREATE TABLE IF NOT EXISTS recipes (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    title TEXT NOT NULL,
    servings INTEGER NOT NULL CHECK (servings > 0),
    ingredients TEXT NOT NULL,
    instructions TEXT NOT NULL
);
