## Scenario 3.2.8

library(ibmdbR)
con = idaConnect("BLUDB;DATABASE=bludb;HOSTNAME=dashdb-enterprise-yp-dal13-30.services.dal.bluemix.net;PORT=50000;PROTOCOL=TCPIP;", uid = "wilfriedhoge", pwd = "dip44TRUMPF@", conType = "odbc")
idaInit(con)

sql = "
SELECT DISTINCT top
FROM dip.eds1_star
WHERE Attribute1 = 'F' AND
      Attribute2 = 315;
"

res1 = idaQuery(sql)

sql = "
SELECT DISTINCT top
FROM dip.eds1_star
WHERE Attribute1 IN ('A','E','I','O','U') AND
      Attribute2 >= 518 AND Attribute2 <= 522;
"

res2 = idaQuery(sql)

