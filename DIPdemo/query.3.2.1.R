## Scenario 3.2.1

library(ibmdbR)
con = idaConnect("BLUDB;DATABASE=bludb;HOSTNAME=dashdb-enterprise-yp-dal13-30.services.dal.bluemix.net;PORT=50000;PROTOCOL=TCPIP;", uid = "wilfriedhoge", pwd = "dip44TRUMPF@", conType = "odbc")
idaInit(con)

sql = "
      WITH RPL (ID, Number3, Number1, Path) AS
      (   SELECT ROOT.ID, ROOT.Number3, ROOT.Number1, CAST(ID AS VARCHAR(1000))
          FROM dip.eds1 ROOT
          WHERE ROOT.Number1 = 'L000000786'
        UNION ALL
          SELECT PARENT.ID, PARENT.Number3, PARENT.Number1, RTRIM(CHILD.Path) || ' > ' || CAST(PARENT.ID AS CHAR(15))
          FROM RPL CHILD, dip.eds1 PARENT
          WHERE  PARENT.ID = CHILD.Number3
      )
    SELECT  ID as ROOT, Path
    FROM RPL
    WHERE Number3 is NULL;
"

res = idaQuery(sql)
