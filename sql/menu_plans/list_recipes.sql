SELECT
    m.id AS entry_id,
    m.day,
    r.id,
    r.title,
    r.servings,
    r.ingredients,
    r.instructions
FROM menu_plan_to_recipe AS m
JOIN recipes AS r ON r.id = m.recipe_id
WHERE m.menu_plan_id = ?
ORDER BY m.day, m.id;