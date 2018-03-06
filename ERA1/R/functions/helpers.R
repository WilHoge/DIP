correct_machine <- function(subdat, verbose=FALSE){
    if(verbose){
      info_string <- paste0("correct_machine(): ", subdat$EquipNr[1], 
                            " (", which(subdat$EquipNr[1] == all_machines), "/", length(all_machines), ", ",
                            round(which(subdat$EquipNr[1] == all_machines)/length(all_machines)*100), "%)")
      cat(info_string, "\n")
    }
  
    QS <- compute_quality_score(subdat)
    if(!is.na(QS) && QS < config$QS_threshold_for_manual_flag){
        # flag them to say this machine needs someone to look at the counter values
        for(col in ZAEHLER){
          subdat[[paste0("manual_", col)]] <- MANUAL_BAD_QUALITY_SCORE
          subdat[[paste0("Offset_", col)]] <- 0
          subdat[[paste0("Bereich_", col)]] <- 1
        }
        # In this case, subdat$Abgenommen stays FALSE
        return(subdat)
    }

    subdat <- add_offset_bereiche(subdat)
    subdat <- swap_vertauscher(subdat)
    subdat <- swap_resonator_below_strahl(subdat)
    subdat <- remove_too_low_slopes(subdat)
    subdat <- remove_too_high_slopes(subdat)
    
    subdat$Abgenommen <- TRUE
    return(subdat)
}

minus99_to_NA <- function(vec){
    vec[vec == -99] <- NA
    return(vec)
}

NA_to_minus99 <- function(vec){
    vec[is.na(vec)] <- -99
    vec[is.nan(vec)] <- -99
    return(vec)
}

deactivate_duplicate_timestamps <- function(dat){
  multiple_entries <- dat %>%
    group_by(EquipNr, Erfassungsdatum) %>%
    summarise(count=n()) %>%
    filter(count > 1)
  
  for(i in seq_len(nrow(multiple_entries))){
    dat_rows <- which(dat$EquipNr==multiple_entries$EquipNr[i] & 
                        dat$Erfassungsdatum == multiple_entries$Erfassungsdatum[i])
    nonmissing <- !is.na(dat[dat_rows, "SIS_NC"])
    keep <- which(nonmissing)[1]
    if((length(keep) != 1) || is.na(keep))  # keep == NA if Erfassungsdatum==NA 
      keep <- 1
    dat[dat_rows[-keep], "Deaktiviert"] <- 1
  }
  return(dat)
}

deactivate_if_Erfassungsdatum_is_NA <- function(dat){
  dat$Deaktiviert[is.na(dat$Erfassungsdatum)] <- 1
  return(dat)
}

get_slopes <- function(subdat, col){
    y <- subdat[[col]][!is.na(subdat[[col]])]
    x <- subdat[["Erfassungsdatum"]][!is.na(subdat[[col]])]
    slopes <- diff(y) / as.numeric(diff(x))  # diff(x) as 'difftime' is now measured in days
    return(slopes / 24)  # return slopes in terms of hours instead of days
}

add_slopes <- function(subdat, columns){
    for(zaehler in columns){
        slopes <- get_slopes(subdat, zaehler)
        slope_col <- paste0(zaehler, "_Steigung")
        subdat[[slope_col]] <- NA
        subdat[[slope_col]][!is.na(subdat[[zaehler]])] <- c(slopes, NA)  # fill the last non-NA field with a slope of "NA"
    }
    return(subdat)
}

add_slope_proportions <- function(subdat, slopes=FALSE){
  if(slopes){  # Proportions of slopes
    subdat$NC_zu_SA <- subdat$SIS_NC_Aufgewertet_Steigung / subdat$SIS_Strahl_Aufgewertet_Steigung
    subdat$NC_zu_RA <- subdat$SIS_NC_Aufgewertet_Steigung / subdat$SIS_Resonator_Aufgewertet_Steigung
    subdat$RA_zu_SA <- subdat$SIS_Resonator_Aufgewertet_Steigung / subdat$SIS_Strahl_Aufgewertet_Steigung
  } else {  # Proportions of absolute values
    subdat$NC_zu_SA <- subdat$SIS_NC_Aufgewertet / subdat$SIS_Strahl_Aufgewertet
    subdat$NC_zu_RA <- subdat$SIS_NC_Aufgewertet / subdat$SIS_Resonator_Aufgewertet
    subdat$RA_zu_SA <- subdat$SIS_Resonator_Aufgewertet / subdat$SIS_Strahl_Aufgewertet
  }
  
  subdat$NC_zu_SA[is.infinite(subdat$NC_zu_SA)] <- -99
  subdat$NC_zu_RA[is.infinite(subdat$NC_zu_RA)] <- -99
  subdat$RA_zu_SA[is.infinite(subdat$RA_zu_SA)] <- -99
  return(subdat)
}

has_column <- function(df, column){
    return(column %in% names(df))
}

edit_aufwertungsbeschreibung <- function(subdat, row, swapped=NULL, removed=NULL){
    zaehler <- c("NC", "RA", "SA")

    ## As of now, use only one of these per function call, not both at the same time
    if(!is.null(removed)){
        stopifnot(length(removed)==1)
        edit_string <- paste0("-", zaehler[removed])
    } else if(!is.null(swapped)){
        stopifnot(length(swapped)==2)
        edit_string <- paste0(zaehler[swapped[1]], "<>", zaehler[swapped[2]])
    }

    subdat$Aufwertungsbeschreibung[row] <- paste0(subdat$Aufwertungsbeschreibung[row], edit_string, ", ")
    return(subdat)
}

postprocess <- function(dat){
    # Round numeric columns to 2 digits
    dat <- dat %>% mutate_if(is.numeric, funs(round(., 2)))
    dat$Letzte_Aenderung <- Sys.time()  # time of last edit
    dat$Aufwertungsbeschreibung_manuell <- NA
    return(dat)
}

add_descriptive_columns <- function(subdat, verbose=FALSE){
  if(verbose){
    info_string <- paste0("add_descriptive_columns(): ", subdat$EquipNr[1], 
                          " (", which(subdat$EquipNr[1] == all_machines), "/", length(all_machines), ", ",
                          round(which(subdat$EquipNr[1] == all_machines)/length(all_machines)*100), "%)")
    cat(info_string, "\n")
  }
  
    ## Smooths
    subdat <- add_yhats(subdat)
    ## Relative Residuen
    ## Absolute Residuen
    subdat <- add_residuals(subdat, absolute=TRUE, relative=TRUE)
    ## Steigung
    subdat <- add_slopes(subdat, columns=ZAEHLER)
    ## Quotient d. Steigung
    subdat <- add_slope_proportions(subdat)
    ## Guetepruefung [0,1]
    subdat$quality_score <- compute_quality_score(subdat)
    
    ## Differenztage
    subdat <- add_differenztage(subdat)
    
    ## "before"-Werte für Steigung und Quoten:
    subdat <- add_slopes(subdat, columns=c("SIS_NC", "SIS_Resonator", "SIS_Strahl"))
    subdat$NC_zu_SA_vorher <- subdat$SIS_NC_Steigung / subdat$SIS_Strahl_Steigung
    subdat$NC_zu_RA_vorher <- subdat$SIS_NC_Steigung / subdat$SIS_Resonator_Steigung
    subdat$RA_zu_SA_vorher <- subdat$SIS_Resonator_Steigung / subdat$SIS_Strahl_Steigung
    subdat$NC_zu_SA_vorher[is.infinite(subdat$NC_zu_SA_vorher)] <- -99
    subdat$NC_zu_RA_vorher[is.infinite(subdat$NC_zu_RA_vorher)] <- -99
    subdat$RA_zu_SA_vorher[is.infinite(subdat$RA_zu_SA_vorher)] <- -99
    return(subdat)
}

add_differenztage <- function(subdat){
  # gesamte Differenztage:
  subdat$Differenztage <- c(as.numeric(diff(ymd(subdat$Erfassungsdatum))), NA)
  
  # Differenztage pro Zaehler:
  for(col in ZAEHLER){
     nonNA_idx <- !is.na(subdat[[col]])
     subdat[[paste0("Differenztage_", col)]] <- 0
     if(any(nonNA_idx)){
       subdat[nonNA_idx, paste0("Differenztage_", col)] <- c(as.numeric(diff(ymd(subdat$Erfassungsdatum[nonNA_idx]))), NA)     
     }
  }
  return(subdat)
}

add_before_slopes <- function(subdat){
  for(zaehler in c("SIS_NC", "SIS_Resonator", "SIS_Strahl")){
    slopes <- get_slopes(subdat, zaehler)
    slope_col <- paste0(zaehler, "_Steigung")
    subdat[[slope_col]] <- NA
    subdat[[slope_col]][!is.na(subdat[[zaehler]])] <- c(slopes, NA)  # fill the last non-NA field with a slope of "NA"
  }
  return(subdat)
}

get_weighted_avg_slope <- function(subdat, col){

  subdat <- add_slopes(subdat, columns=col)
  subdat <- add_differenztage(subdat)
  
  idx <- !is.na(subdat[[col]])
  slopes <- subdat[[paste0(col, "_Steigung")]][idx]
  w <- subdat[[paste0("Differenztage_", col)]][idx]

  return(weighted.mean(slopes, w, na.rm=TRUE))  # na.rm because the very last weight (and slope) are NA
}
