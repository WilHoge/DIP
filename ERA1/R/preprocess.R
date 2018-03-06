#### Load data from CSV (made via Excel import)
# dat <- read.csv("../data/Zaehler.csv", sep=";", header=TRUE, stringsAsFactors=FALSE)

#### Load data from .accdb
#accdb <- odbcConnectAccess2007(accdb_input_file)
# sqlTables(accdb)
#query <- "SELECT * FROM Tab_Zaehler"
#dat <- sqlQuery(accdb, query, as.is = TRUE)
#odbcClose(accdb)

#### Begin preprocessing

library(ibmdbR)
con = idaConnect("BLUDB;DATABASE=bludb;HOSTNAME=dashdb-enterprise-yp-dal13-30.services.dal.bluemix.net;PORT=50000;PROTOCOL=TCPIP;", uid = "wilfriedhoge", pwd = "dip44TRUMPF@", conType = "odbc")
idaInit(con)

idf.1 <- ida.data.frame('DIP.EDS4')
dat = as.data.frame(idf.1)

colnames(dat)=c("ID","ID1","EquipNr","EquipTyp","IB1","IB2","Einsatz_DATUM","SIS_Erfassungsdatum",
                "DATUMRUECKMELDUNG","SIS_NC","SIS_Resonator","SIS_Strahl","Einsatz_TYP","SIS_NC_Aufgewertet",
                "SIS_Resonator_Aufgewertet","SIS_Strahl_Aufgewertet","Erfassungsdatum","IB1_Aufgewertet",
                "IB2_Aufgewertet","Stunden_ab_IB1","Aufwertungsbeschreibung","Datum_Erstellung_Datensatz",
                "korrigiert_SIS_NC_Aufgewertet","korrigiert_SIS_Resonator_Aufgewertet",
                "korrigiert_SIS_Strahl_Aufgewertet","manual_SIS_NC_Aufgewertet","manual_SIS_Resonator_Aufgewertet",
                "manual_SIS_Strahl_Aufgewertet","Bereich_SIS_NC_Aufgewertet","Offset_SIS_NC_Aufgewertet",
                "Bereich_SIS_Resonator_Aufgewertet","Offset_SIS_Resonator_Aufgewertet","Bereich_SIS_Strahl_Aufgewertet",
                "Offset_SIS_Strahl_Aufgewertet","SIS_NC_Aufgewertet_hat","SIS_Resonator_Aufgewertet_hat",
                "SIS_Strahl_Aufgewertet_hat","SIS_NC_Aufgewertet_resid_abs","SIS_NC_Aufgewertet_resid_rel",
                "SIS_Resonator_Aufgewertet_resid_abs","SIS_Resonator_Aufgewertet_resid_rel","SIS_Strahl_Aufgewertet_resid_abs",
                "SIS_Strahl_Aufgewertet_resid_rel","SIS_NC_Aufgewertet_Steigung","SIS_Resonator_Aufgewertet_Steigung",
                "SIS_Strahl_Aufgewertet_Steigung","NC_zu_SA","NC_zu_RA" ,"RA_zu_SA","quality_score","Differenztage" ,
                "Differenztage_SIS_NC_Aufgewertet","Differenztage_SIS_Resonator_Aufgewertet" ,"Differenztage_SIS_Strahl_Aufgewertet" ,
                "SIS_NC_Steigung","SIS_Resonator_Steigung","SIS_Strahl_Steigung","NC_zu_SA_vorher","NC_zu_RA_vorher",
                "RA_zu_SA_vorher","Automatische_Bereinigung" ,"Letzte_Aenderung","Deaktiviert" ,"Deaktiviert_")


## Replace -99 with NA
for(z in ZAEHLER){
  dat[[z]] <- minus99_to_NA(dat[[z]])
}

dat$Erfassungsdatum <- ymd_hms(substring(dat$Erfassungsdatum,1,19))

## Sort data by date
dat <- dat[order(dat$EquipNr, dat$Erfassungsdatum), ]

## Convert counters from integer to numeric, so that median() works
for(col in c("SIS_NC", "SIS_Resonator", "SIS_Strahl",
            "SIS_NC_Aufgewertet", "SIS_Resonator_Aufgewertet",
            "SIS_Strahl_Aufgewertet")){
  dat[[col]] <- as.numeric(dat[[col]])
}

dat$Deaktiviert <- 0
dat <- deactivate_duplicate_timestamps(dat)
dat <- deactivate_if_Erfassungsdatum_is_NA(dat)

# info columns whether this counter was corrected by this procedure:
dat$korrigiert_SIS_NC_Aufgewertet <- FALSE
dat$korrigiert_SIS_Resonator_Aufgewertet <- FALSE
dat$korrigiert_SIS_Strahl_Aufgewertet <- FALSE

# info columns on whether (and through the numeric value, why) a manual check for this counter is necessary
dat$manual_SIS_NC_Aufgewertet <- 0
dat$manual_SIS_Resonator_Aufgewertet <- 0
dat$manual_SIS_Strahl_Aufgewertet <- 0

# Delete all counters whose hours are higher than Stunden_ab_IB1
for(col in ZAEHLER){
  rows_with_implausible_values <- which(dat[[col]] > dat$Stunden_ab_IB1)
  dat[rows_with_implausible_values, col] <- NA
}

# Aufwertungsbeschreibung: Replace NA with empty string
dat$Aufwertungsbeschreibung[is.na(dat$Aufwertungsbeschreibung)] <- ""

if("Abgenommen" %in% colnames(dat)){
  dat$Abgenommen[is.na(dat$Abgenommen)] <- FALSE  # new values with empty entry become FALSE
} else {
  dat$Abgenommen <- FALSE  # Abgenommen: Final, corrected value
}

#########################################
# Separate aktiviert and deaktiviert data

dat_aktiviert <- filter(dat, Deaktiviert==0)
dat_deaktiviert <- filter(dat, Deaktiviert==1)
rm(dat)

# create some descriptives so that correct_machine(verbose=TRUE) can output percentages
all_machines <- sort(unique(dat_aktiviert$EquipNr))
