SELECT id, user_id, start_date, end_date
FROM menu_plan
WHERE user_id = ?
ORDER BY id;
