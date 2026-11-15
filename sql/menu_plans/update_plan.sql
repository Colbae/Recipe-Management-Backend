UPDATE menu_plan
SET start_date = ?, end_date = ?
WHERE id = ? AND user_id = ?
  AND NOT EXISTS (
    SELECT 1 FROM menu_plan_to_recipe
    WHERE menu_plan_id = menu_plan.id
      AND (day < ? OR day > ?)
  );
