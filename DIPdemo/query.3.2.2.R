## Scenario 3.2.2

library(ibmdbR)
con = idaConnect('BLUDB','','')
idaInit(con)

sql = "
WITH RPL (ID, Number3, level, root) AS
 (  SELECT ROOT.ID, ROOT.Number3, 0, ROOT.ID
    FROM dip.eds1 ROOT
    WHERE ROOT.ID IN (10000045349,10000056692)
  UNION ALL
    SELECT CHILD.ID, CHILD.Number3, PARENT.level+1, PARENT.ROOT
    FROM RPL PARENT, dip.eds1 CHILD
    WHERE  PARENT.ID = CHILD.Number3
)
SELECT COALESCE(a.level,b.level) as level, a.count as \"ID=10000045349\", b.count as \"ID=10000056692\" 
FROM
 (  SELECT  root, level, count(*) as count
     FROM RPL
     WHERE root = 10000045349
     GROUP BY root, level) a
 FULL OUTER JOIN
 (  SELECT  root, level, count(*) as count
     FROM RPL
     WHERE root = 10000056692
     GROUP BY root, level) b
 ON a.level = b.level;
 ;"

levelRes = idaQuery(sql)

sql = "
WITH RPL (ID, Number3, level, root) AS
  (   SELECT ROOT.ID, ROOT.Number3, 0, ROOT.ID
      FROM dip.eds1 ROOT
      WHERE ROOT.ID IN (10000045349,10000056692)
      UNION ALL
      SELECT CHILD.ID, CHILD.Number3, PARENT.level+1, PARENT.ROOT
      FROM RPL PARENT, dip.eds1 CHILD
      WHERE  PARENT.ID = CHILD.Number3
)
SELECT root, id, level
FROM RPL
ORDER BY root, id; 
"

materialsRes = idaQuery(sql)

sql = "
      WITH RPL (ID, Number3, level, root, Attribute1) AS
       (   SELECT ROOT.ID, ROOT.Number3, 0, ROOT.ID, ROOT.Attribute1
            FROM dip.eds1 ROOT
            WHERE ROOT.ID IN (10000045349,10000056692)
          UNION ALL
            SELECT CHILD.ID, CHILD.Number3, PARENT.level+1, PARENT.ROOT, CHILD.Attribute1
            FROM RPL PARENT, dip.eds1 CHILD
            WHERE  PARENT.ID = CHILD.Number3
       )
    SELECT COALESCE(a.Attribute1,b.Attribute1) as Attribut1, a.count as \"ID=10000045349\", b.count as \"ID=10000056692\"
    FROM
    (  SELECT  root, Attribute1, count(*) as count
        FROM RPL
        WHERE root = 10000045349
        GROUP BY root, Attribute1) a
    FULL OUTER JOIN
    (  SELECT  root, Attribute1, count(*) as count
        FROM RPL
        WHERE root = 10000056692
        GROUP BY root, Attribute1) b
    ON a.Attribute1 = b.Attribute1
    ORDER BY 1;
"

attRes = idaQuery(sql)
