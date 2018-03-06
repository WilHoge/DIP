library(ibmdbR)
con = idaConnect("BLUDB;DATABASE=bludb;HOSTNAME=dashdb-enterprise-yp-dal13-30.services.dal.bluemix.net;PORT=50000;PROTOCOL=TCPIP;", uid = "wilfriedhoge", pwd = "dip44TRUMPF@", conType = "odbc")
idaInit(con)

res = idaQuery("select * from tdip.tab_zaehler")
df = as.data.frame(res)

write.csv(res, "Tab_Zaehler.csv")
