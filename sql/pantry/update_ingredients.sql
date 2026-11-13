-- Replace the full JSON list after Haskell computes additions or subtractions.
UPDATE pantry
SET ingredients = ?
WHERE id = ? AND user_id = ?;
