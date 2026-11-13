INSERT INTO user (id, name) VALUES (1, 'Alice'), (2, 'Bob')
ON CONFLICT(id) DO NOTHING;

SELECT id, name FROM user ORDER BY id;
