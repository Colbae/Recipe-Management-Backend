INSERT INTO menu_plan_to_recipe (menu_plan_id, recipe_id, day)
SELECT id, ?, ?
FROM menu_plan
WHERE id = ? AND user_id = ?;
