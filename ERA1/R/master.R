setwd("Y:/2017-04-12_Trumpf_EXA_HarveyNash/R")
accdb_input_file <- "../data/TechnischeZuverlaessigkeit_Test-EXA2.accdb"

source("config.R")  # runs for ca. 30sec
source("preprocess.R")  # loads data from accdb and preprocesses it into R

################################################################
#### The correction procedure starts here.
#### This should be run only once for the existing database.

machine_DFs <- split(dat_aktiviert, dat_aktiviert$EquipNr)
#machine_DFs <- machine_DFs[c(1,100,200,1000)]  # debug line. This way, the code takes just a few seconds.
machine_DFs_corrected <- lapply(machine_DFs, correct_machine, verbose=TRUE)
machine_DFs_corrected <- lapply(machine_DFs_corrected, add_descriptive_columns, verbose=TRUE)
dat_corrected <- bind_rows(machine_DFs_corrected)
dat_corrected <- postprocess(dat_corrected)
dat_deaktiviert <- postprocess(dat_deaktiviert)

#write.csv2(dat_corrected, file="dat_corrected.csv")
update_table_in_accdb(dat_corrected, accdb_path=accdb_input_file, table="Tab_Zaehler", verbose=TRUE)  # ca. 5min
update_table_in_accdb(dat_deaktiviert, accdb_path=accdb_input_file, table="Tab_Zaehler", verbose=TRUE)

saveRDS(dat_corrected, file="../data/dat_corrected.rds")
saveRDS(dat_deaktiviert, file="../data/dat_deaktiviert.rds")

################################################################
#### The following code checks *new* data, given the machine has been corrected before
#### This should be run every month, when new data (with column Abgenommen==0) has been added to the database.

# #### Load and split up sample data into old (oldest 98%) and new (newest 2%) data:
# accdb_input_file <- "../data/TechnischeZuverlaessigkeit_100percent.accdb"
# source("config.R")  # runs for ca. 30sec
# source("preprocess.R")  # loads data from accdb and preprocesses it into R
# dim(dat_aktiviert)
# 
# cutoff <- quantile(ymd_hms(dat_aktiviert$SIS_Erfassungsdatum), 0.98, na.rm=TRUE)
# dat_old <- filter(dat_aktiviert, ymd_hms(SIS_Erfassungsdatum) <= cutoff)
# dat_new <- filter(dat_aktiviert, ymd_hms(SIS_Erfassungsdatum) > cutoff)
# 
# #### Correct the old data
# machine_DFs <- split(dat_old, dat_old$EquipNr)
# machine_DFs_corrected <- lapply(machine_DFs, correct_machine, verbose=TRUE)
# machine_DFs_corrected <- lapply(machine_DFs_corrected, add_descriptive_columns, verbose=TRUE)
# dat_old_corrected <- bind_rows(machine_DFs_corrected)
# 
# dat_test <- rbind.fill(dat_old_corrected, dat_new)
# saveRDS(dat_test, file="../data/dat_test.rds")
dat_test <- readRDS("../data/dat_test.rds")
#### ^- This is the complete data set after 1 month of new data has accumulated.
#### Columns like "Abgenommen" and "Bereich" are partially empty, i.e. NA, now.

## TODO check if dat_test structure is equal to dat_aktiviert from above.
## TODO then, add preprocessing and correction procedure here and 
##  check for equality of fully processed data and 98/2-percent data.

################################################################
#### Create all correction plots

if(!file.exists("../img/gesamt")) dir.create("../img/gesamt", recursive=TRUE)
if(!file.exists("../img/gesamt/offset")) dir.create("../img/gesamt/offset", recursive=TRUE)
if(!file.exists("../img/gesamt/vertauscher")) dir.create("../img/gesamt/vertauscher", recursive=TRUE)
if(!file.exists("../img/gesamt/monotone")) dir.create("../img/gesamt/monotone", recursive=TRUE)
if(!file.exists("../img/gesamt/nothing")) dir.create("../img/gesamt/nothing", recursive=TRUE)
if(!file.exists("../img/gesamt/manual")) dir.create("../img/gesamt/manual", recursive=TRUE)

for(id in unique(dat_aktiviert$EquipNr)){
  cat(id, "\n")
  subdat_before <- filter(dat_aktiviert, EquipNr==id)
  plot_correction(subdat_before)
}

################################################################
#### Sandbox

dat_corrected <- readRDS("../data/dat_corrected.rds")

bad_machines <- 
dat_corrected %>%
  filter(manual_SIS_NC_Aufgewertet == 0) %>%
  filter(SIS_NC_Aufgewertet_Steigung < 0) %>%
  filter(mean(Bereich_SIS_NC_Aufgewertet) > 1) %>%   # this filters out all machines where an offset happened
  select(EquipNr) %>%
  unlist %>% unname %>% unique

length(bad_machines)  

ID <- bad_machines[2]
subdat <- machine_DFs[[ID]]
subdat_c <- correct_machine(subdat)

plot_base_w_smooth(subdat)  
plot_base_w_smooth(subdat_c)

subdat_c$Bereich_SIS_NC_Aufgewertet
subdat_c[[ZAEHLER[1]]]

