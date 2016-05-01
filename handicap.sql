CREATE OR REPLACE VIEW handicap AS
SELECT s.id AS sid, c.date AS date, u.id AS uid, u.name AS name, sum(o.score) - sum(h.par) AS result
FROM user AS u, layout AS l, hole AS h, serie AS s, competition AS c, round AS r, score AS o
WHERE s.id = c.serie_id
AND r.competition_id = c.id
AND o.round_id = r.id
AND r.state = 'Finished'
AND c.state = 'Finished'
AND h.layout_id = l.id
AND o.hole_id = h.id
AND u.id = r.user_id
GROUP BY r.id, l.id
ORDER BY sid, result;
