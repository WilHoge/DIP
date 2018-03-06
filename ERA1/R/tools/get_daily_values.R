source("functions/offset.R")  # I need get_weighted_avg_slope() from this file
ZAEHLER <- c("SIS_NC_Aufgewertet", "SIS_Resonator_Aufgewertet", "SIS_Strahl_Aufgewertet")

dat <- readRDS("../data/dat_corrected.rds")

IDs <- c("L0131E0534",
         "L1150E0957",
         "S1631C0221")
for(id in IDs){
  cat(id, "\n")
  machine <- filter(dat, EquipNr==id)
  DV <- get_daily_values(machine)
  write.csv2(DV, paste0("../img/interpolation/", id, ".csv"), row.names=FALSE)
  png(paste0("../img/interpolation/", id, ".png"))
  plot_interpolation(DV)
  dev.off()
}


#### Function definition:

get_daily_values <- function(dat, end_date=Sys.Date()){
  
  IB2 <- as.Date(ymd_hms(dat$IB2_Aufgewertet)[1])
  
  DV_start_date <- min(IB2, as.Date(dat$Erfassungsdatum))
  DV_end_date <- max(end_date, as.Date(dat$Erfassungsdatum))
  
  # If end date is before the last measured value, create data frame until the last value in Erfassungsdatum
  DV <- data.frame(Erfassungsdatum = seq(DV_start_date, DV_end_date, by=1),
                   SIS_NC_Aufgewertet = NA,
                   SIS_Resonator_Aufgewertet = NA,
                   SIS_Strahl_Aufgewertet = NA)
  
  ## enter all available data
  DV_rows <- match(as.Date(dat$Erfassungsdatum), DV$Erfassungsdatum)
  DV[DV_rows, ZAEHLER] <- dat[,ZAEHLER]
  
  
  for(z in ZAEHLER){

    if(all(is.na(dat[[z]]))){
      # ignore counters where all values are NA
      DV[[paste0(z, "_interpoliert")]] <- NA
      next
    }
    
    DV[1, z] <- ifelse(is.na(DV[1, z]), 0, DV[1, z])  # set IB2 (first date) to 0, but only if there is no measurement
    y <- DV[[z]]
    
    if(is.na(last(y))){  # extrapolate only if the last value is NA (i.e. not an actual measurement)
      slope <- get_weighted_avg_slope(DV, z)
      last_value_row <- last(which(!is.na(y)))
      
      diff_days <- as.numeric(DV$Erfassungsdatum[nrow(DV)] - DV$Erfassungsdatum[last_value_row])
      ## extrapolate value in last row:
      y[nrow(DV)] <- round(y[last_value_row] + slope * 24 * diff_days)
    }
    
    # linearly interpolate all daily values:
    DV[[paste0(z, "_interpoliert")]] <- round(approx(x = DV$Erfassungsdatum, y = y, xout = DV$Erfassungsdatum)$y)
  }
  return(DV)
}

plot_interpolation <- function(DV){
  y_lims <- c(0,
              max(DV[,paste0(ZAEHLER, "_interpoliert")], na.rm=TRUE)
              )
  plot(DV$Erfassungsdatum, DV[[ZAEHLER[1]]], xlab="", ylab="", ylim=y_lims)
  points(DV$Erfassungsdatum, DV[[ZAEHLER[2]]], col=2)
  points(DV$Erfassungsdatum, DV[[ZAEHLER[3]]], col=3)
  
  lines(DV$Erfassungsdatum, DV$SIS_NC_Aufgewertet_interpoliert, lty=2)
  lines(DV$Erfassungsdatum, DV$SIS_Resonator_Aufgewertet_interpoliert, lty=2, col=2)
  lines(DV$Erfassungsdatum, DV$SIS_Strahl_Aufgewertet_interpoliert, lty=2, col=3)
}
