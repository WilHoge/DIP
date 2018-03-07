## Scenario 3.2.7

library(ibmdbR)
con = idaConnect("BLUDB;DATABASE=bludb;HOSTNAME=dashdb-enterprise-yp-dal13-30.services.dal.bluemix.net;PORT=50000;PROTOCOL=TCPIP;", uid = "wilfriedhoge", pwd = "dip44TRUMPF@", conType = "odbc")
idaInit(con)

## (3.2.7.2) Find in EDS1 all trees that contain at least one node containing one of the following attribute values
##           Number1: between EK00000001 – EK00000030

sql = "
CREATE TABLE tdip.SubsetA (ID bigint);
"
idaQuery(sql)

sql = "
INSERT INTO tdip.SubsetA
SELECT distinct a.top
FROM dip.eds1_hier1 a, dip.eds1 b
WHERE a.ID = b.ID AND
      b.Number1 in ('EK00000001', 'EK00000002', 'EK00000003', 'EK00000004', 'EK00000005', 'EK00000006', 'EK00000007',
                    'EK00000008', 'EK00000009', 'EK00000010', 'EK00000011', 'EK00000012', 'EK00000013', 'EK00000014',
                    'EK00000015', 'EK00000016', 'EK00000017', 'EK00000018', 'EK00000019', 'EK00000020', 'EK00000021',
                    'EK00000022', 'EK00000023', 'EK00000024', 'EK00000025', 'EK00000026', 'EK00000027', 'EK00000028',
                    'EK00000029', 'EK00000030')
;
"

idaQuery(sql)

## Found are trees now in table SubsetA
subsetAres = idaQuery("select * from tdip.SubsetA")
View(subsetAres)

## (3.2.7.3) Match the subset of trees “Subset A” to the order positions in EDS2 
##           via a join of equality of EDS1 (Number2) and EDS2 (Number2)

sql = "
CREATE TABLE tdip.join1 (ID BIGINT, date1 DATE, Attribute1 INT, Number1 CHAR(10), Number2 INT)
"

idaQuery(sql)

sql = "
INSERT INTO tdip.join1
SELECT distinct eds2.ID, eds2.date1, eds2.Attribute1, eds2.Number1, eds2.Number2
FROM dip.eds2 eds2, dip.eds1_hier1 eds1_hier, tdip.SubsetA subseta
WHERE subseta.ID = eds1_hier.top AND 
      eds1_hier.Number2 = eds2.Number2;
"

idaQuery(sql)

## Matching positions are now in View join1
join1res = idaQuery("select * from tdip.join1")
View(join1res)


## (3.2.7.4) Given the result “Join1” of the previous step we want to know all EDS2 (Attribute1) values 
##           where we have no matching data for a certain time window for EDS2 (Date1). 
##           Check for the time window [2017-01-01 to 2017-12-31].

sql = "
CREATE VIEW tdip.NonBuyers1 (Attribute1) as
SELECT distinct Attribute1
FROM tdip.join1 a
WHERE NOT EXISTS (SELECT 1 FROM tdip.join1 b
                  WHERE a.id = b.id AND year(b.date1)=2017);
"

idaQuery(sql)

## Found customers not buying in 2017 are in View NonBuyers1
nonBuyers1res = idaQuery("select * from tdip.NonBuyers1")
View(nonBuyers1res)

## Get all known trees of EDS1 that can be related to the Values of NonBuyers1

sql = "
SELECT distinct eds1_hier.top as ID
FROM dip.eds2 eds2, dip.eds1_hier1 eds1_hier, tdip.NonBuyers1 nonbuyers1
WHERE nonbuyers1.Attribute1 = eds2.Attribute1 AND
eds1_hier.Number2 = eds2.Number2;
"

nonBuyerMachinesRes = idaQuery(sql)

## Found trees
View(nonBuyerMachinesRes)

idaDeleteTable("tdip.SubsetA")
idaDeleteTable("tdip.join1")
idaDropView("tdip.NonBuyers1")
