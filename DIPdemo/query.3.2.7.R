## Scenario 3.2.7

library(ibmdbR)
con = idaConnect('BLUDB','','')
idaInit(con)

sql = "
CREATE TABLE tdip.SubsetA (ID bigint) ORGANIZE BY row;
"
res = idaQuery(sql)

sql = "
INSERT INTO tdip.SubsetA
WITH RPL (ID, Number3) AS
(  SELECT ROOT.ID, ROOT.Number3
FROM dip.eds1 ROOT
WHERE ROOT.Number1 in ('EK00000001', 'EK00000002', 'EK00000003', 'EK00000004', 'EK00000005', 'EK00000006', 'EK00000007',
                       'EK00000008', 'EK00000009', 'EK00000010', 'EK00000011', 'EK00000012', 'EK00000013', 'EK00000014',
                       'EK00000015', 'EK00000016', 'EK00000017', 'EK00000018', 'EK00000019', 'EK00000020', 'EK00000021',
                       'EK00000022', 'EK00000023', 'EK00000024', 'EK00000025', 'EK00000026', 'EK00000027', 'EK00000028',
                       'EK00000029', 'EK00000030')
UNION ALL
SELECT PARENT.ID, PARENT.Number3
FROM RPL CHILD, dip.eds1 PARENT
WHERE  PARENT.ID = CHILD.Number3
)
SELECT  ID as ROOT
FROM RPL
WHERE Number3 is NULL;
"

res = idaQuery(sql)

subsetAres = ida.data.frame("tdip.SubsetA")
View(subsetAres)

sql = "
CREATE VIEW tdip.join1 (ID, date1, Attribute1) as
SELECT b.ID, a.date1, b.Attribute1
FROM dip.eds2 a, dip.eds1 b, tdip.SubsetA s
WHERE s.ID = b.ID AND a.Number1 = b. Number1;
"

res = idaQuery(sql)

join1res = ida.data.frame("tdip.join1")
View(join1res)

sql = "
CREATE VIEW tdip.NonBuyers1 (ID) as
SELECT ID
FROM tdip.join1 a
WHERE NOT EXISTS (SELECT 1 FROM tdip.join1 b
                  WHERE a.id = b.id AND year(b.date1)=2017);
"

res = idaQuery(sql)

nonBuyers1res = ida.data.frame("tdip.NonBuyers1")
View(nonBuyers1res)

sql = "
WITH RPL (ID, Number3) AS
(   SELECT ROOT.ID, ROOT.Number3
FROM dip.eds1 ROOT
WHERE ROOT.ID IN (SELECT * from tdip.NonBuyers1)
UNION ALL
SELECT CHILD.ID, CHILD.Number3
FROM RPL PARENT, dip.eds1 CHILD
WHERE  PARENT.ID = CHILD.Number3
)
SELECT ID
FROM RPL
WHERE Number3 is NULL;
"

nonBuyerMachinesRes = idaQuery(sql)
View(nonBuyerMachinesRes)

idaDeleteTable("tdip.SubsetA")
idaDropView("tdip.join1")
idaDropView("tdip.NonBuyers1")
