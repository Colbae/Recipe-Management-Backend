CREATE TABLE IF NOT EXISTS user (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL CHECK (length(trim(name)) > 0 AND length(name) <= 255)
);


ALTER TABLE menu_plan ADD COLUMN user_id INTEGER NOT NULL REFERENCES user(id);
ALTER TABLE recipes ADD COLUMN author_id INTEGER NOT NULL REFERENCES user(id);

CREATE INDEX menu_plans_by_user ON menu_plan(user_id);
CREATE INDEX recipes_by_author ON recipes(author_id);
