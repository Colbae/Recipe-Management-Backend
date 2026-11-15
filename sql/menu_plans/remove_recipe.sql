DELETE FROM menu_plan_to_recipe
WHERE id = ?
  AND menu_plan_id = ?
  AND menu_plan_id IN (
    SELECT id FROM menu_plan WHERE user_id = ?
  );
