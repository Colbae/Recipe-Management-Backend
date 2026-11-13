CREATE TABLE IF NOT EXISTS recipes (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    title TEXT NOT NULL CHECK (
        length(trim(title)) > 0
        AND length(title) <= 255
    ),
    servings INTEGER NOT NULL CHECK (servings > 0),
    ingredients TEXT NOT NULL,
    instructions TEXT NOT NULL
);