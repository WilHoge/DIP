library(ibmdbR)
con = idaConnect('BLUDB','','')
idaInit(con)

res = idaQuery("select * from tdip.tab_zaehler")
df = as.data.frame(res)

write.csv(res, "Tab_Zaehler.csv")
