library(arulesViz)
library(ibmdbR)

con = idaConnect("BLUDB;DATABASE=bludb;HOSTNAME=dashdb-enterprise-yp-dal13-30.services.dal.bluemix.net;PORT=50000;PROTOCOL=TCPIP;", uid = "wilfriedhoge", pwd = "dip44TRUMPF@", conType = "odbc")
idaInit(con)

idaQuery("CREATE TABLE TDIP.EDS2_ASO (cluster char(20), Number1 CHAR(10))")
idaQuery("INSERT INTO TDIP.EDS2_ASO SELECT ATTRIBUTE1||CAST(DATE1 as CHAR(10)) AS CLUSTER, NUMBER1 FROM DIP.EDS2 WHERE YEAR(DATE1)=2017")

eds2a = ida.data.frame('TDIP.EDS2_ASO')[,c('CLUSTER','NUMBER1')]

res = idaArule(eds2a,tid="CLUSTER",item="NUMBER1",minsupport=0.0001,minconf=0.9,maxlen=3)

#Now we can inspect the rules on command line
inspect(head(sort(res, by = "lift"), n=10))

plot(head(sort(res, by = "lift"), n=10), method = "graph", control=list(cex=.8))

idaQuery("DROP TABLE TDIP.EDS2_ASO")
idaDropModel(idaGetModelname(res))

idaClose(con)
