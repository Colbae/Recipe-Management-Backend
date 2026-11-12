CREATE TABLE IF NOT EXISTS menu_plan (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    start_date TEXT NOT NULL
        CHECK (date(start_date, '+0 days') IS NOT NULL
               AND date(start_date, '+0 days') = start_date),
    end_date TEXT NOT NULL
        CHECK (date(end_date, '+0 days') IS NOT NULL
               AND date(end_date, '+0 days') = end_date),
    CHECK (start_date <= end_date)
);

CREATE TABLE IF NOT EXISTS menu_plan_to_recipe (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    menu_plan_id INTEGER NOT NULL REFERENCES menu_plan(id) ON DELETE CASCADE,
    recipe_id INTEGER NOT NULL REFERENCES recipes(id) ON DELETE CASCADE,
    day TEXT NOT NULL
        CHECK (date(day, '+0 days') IS NOT NULL AND date(day, '+0 days') = day)
);

CREATE INDEX IF NOT EXISTS menu_plan_entries_by_day
    ON menu_plan_to_recipe(menu_plan_id, day);
CREATE INDEX IF NOT EXISTS menu_plan_entries_by_recipe
    ON menu_plan_to_recipe(recipe_id);
