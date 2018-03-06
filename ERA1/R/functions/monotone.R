remove_too_low_slopes <- function(subdat){
    
    ## Makes the data points monotonically increasing by iteratively removing the points furthest awy from the Theil-Sen regression line
    subdat <- add_offsets_if_exist(subdat)
    
    for(col in ZAEHLER){
        slopes <- get_slopes(subdat, col)
        # cat(paste(subdat$EquipNr[1], ", ", col, "\n"))
        if(any(slopes < config$min_slope[col]) && sum(!is.na(subdat[[col]])) < config$min_obs_for_monotone_correction){
          # If there are negative slopes but only few observations for this counter,
          # flag it for manual inspection and skip
          subdat[[paste0("manual_", col)]] <- MANUAL_TOO_FEW_OBSERVATIONS
          next
        } 

        while(any(slopes<config$min_slope[col])){
            if(sum(!is.na(subdat[[col]])) < config$min_obs_for_monotone_correction){
              subdat[[paste0("manual_", col)]] <- MANUAL_TOO_FEW_OBSERVATIONS
              break
            }
            idx <- which(!is.na(subdat[[col]]))[which.min(slopes)]
            candidates <- c(idx, idx+min(which(!is.na(subdat[-(1:idx), col]))))  # two points to be removed to remove negative slope (ignoring possible NAs)

            resids <- rep(NA, nrow(subdat))
            nonNA <- !is.na(subdat[[col]])

            mod <- cobs(as.numeric(subdat$Erfassungsdatum), subdat[[col]], constraint="increase",
                        degree=1, lambda=1000, knots=as.numeric(subdat$Erfassungsdatum)[!is.na(subdat[[col]])])
            resids[nonNA] <- residuals(mod)
            larger_resid <- candidates[which.max(abs(resids[candidates]))]
            subdat[larger_resid, col] <- NA # remove data point with larger residual
            subdat[larger_resid, paste0("korrigiert_", col)] <- TRUE
            subdat <- edit_aufwertungsbeschreibung(subdat, row=larger_resid, removed=match(col, ZAEHLER))
            slopes <- get_slopes(subdat, col)
        }
    }
    subdat <- add_offsets_if_exist(subdat, subtract=TRUE)
    return(subdat)
}

remove_too_high_slopes <- function(subdat){
  subdat <- add_offsets_if_exist(subdat)
  
  for(col in ZAEHLER){
    slopes <- get_slopes(subdat, col)
    # cat(paste(subdat$EquipNr[1], ", ", col, "\n"))
    if(any(slopes > config$max_slope[col]) && sum(!is.na(subdat[[col]])) < config$min_obs_for_monotone_correction){
      # If there are negative slopes but only few observations for this counter,
      # flag it for manual inspection and skip
      subdat[[paste0("manual_", col)]] <- MANUAL_TOO_FEW_OBSERVATIONS
      next
    } 
    
    while(any(slopes > config$max_slope[col])){
      if(sum(!is.na(subdat[[col]])) < config$min_obs_for_monotone_correction){
        subdat[[paste0("manual_", col)]] <- MANUAL_TOO_FEW_OBSERVATIONS
        break
      }
      idx <- which(!is.na(subdat[[col]]))[which.max(slopes)]
      candidates <- c(idx, idx+min(which(!is.na(subdat[-(1:idx), col]))))  # two points to be removed to remove negative slope (ignoring possible NAs)
      
      resids <- rep(NA, nrow(subdat))
      nonNA <- !is.na(subdat[[col]])
      
      mod <- cobs(as.numeric(subdat$Erfassungsdatum), subdat[[col]], constraint="increase",
                  degree=1, lambda=1000, knots=as.numeric(subdat$Erfassungsdatum)[!is.na(subdat[[col]])])
      resids[nonNA] <- residuals(mod)
      larger_resid <- candidates[which.max(abs(resids[candidates]))]
      subdat[larger_resid, col] <- NA # remove data point with larger residual
      subdat[larger_resid, paste0("korrigiert_", col)] <- TRUE
      subdat <- edit_aufwertungsbeschreibung(subdat, row=larger_resid, removed=match(col, ZAEHLER))
      slopes <- get_slopes(subdat, col)
    }
  }
  subdat <- add_offsets_if_exist(subdat, subtract=TRUE)
  return(subdat)
}

