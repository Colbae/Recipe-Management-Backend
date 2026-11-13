SELECT id, author_id, title, servings, ingredients, instructions
FROM recipes
WHERE id > ?
ORDER BY id
LIMIT ?;
