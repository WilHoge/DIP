swap_vertauscher <- function(subdat){
    if(sum(
        sum(!is.na(subdat$SIS_NC_Aufgewertet)) >= 5,
        sum(!is.na(subdat$SIS_Resonator_Aufgewertet)) >= 5,
        sum(!is.na(subdat$SIS_Strahl_Aufgewertet)) >= 5
    ) < 2){
        ## if less than two columns have at least 5 measurements
        return(subdat)
    }

    ## Calculate model only if more than 4 measurements
    subdat <- add_yhats(subdat)
    
    for(r in seq_len(nrow(subdat))){  # Check for each date of this machine
        
        y <- c(subdat$SIS_NC_Aufgewertet[r], subdat$SIS_Resonator_Aufgewertet[r], subdat$SIS_Strahl_Aufgewertet[r])  # actual values
        yhat <- c(subdat$SIS_NC_Aufgewertet_hat[r], subdat$SIS_Resonator_Aufgewertet_hat[r], subdat$SIS_Strahl_Aufgewertet_hat[r])  # robust predictions

        for(i in 1:3){  # NC, Re, St
            if(is.na(y[i]) || is.na(yhat[i])){
                ## If this counter has no data, skip it
                next
            }
            if(abs(y[i] - yhat[i]) != min(abs(y[i] - yhat), na.rm=TRUE)){  # if the point lies closer to a different predicted value
                swap_candidate <- which.min(abs(y[i] - yhat))
                
                new_residual <- abs(y[i] - yhat[swap_candidate])
                old_residual <- abs(y[i] - yhat[i])

                if(new_residual > 0.1 * old_residual){  # TODO parmetrize 0.1
                    ## only consider points with large relative residual (new resid <10% of old resid)
                    next
                }

                if(is.na(y[swap_candidate]) || which.min(abs(y[swap_candidate] - yhat)) == i){  # if the other point is NA or would also best be swapped
                    swap_tmp <- subdat[r, ZAEHLER[i]]
                    subdat[r, ZAEHLER[i]] <- subdat[r, ZAEHLER[swap_candidate]]
                    subdat[r, ZAEHLER[swap_candidate]] <- swap_tmp

                    subdat[r, paste0("korrigiert_", ZAEHLER[i])] <- TRUE
                    subdat[r, paste0("korrigiert_", ZAEHLER[swap_candidate])] <- TRUE
                    
                    subdat <- edit_aufwertungsbeschreibung(subdat, row=r, swap=c(i, swap_candidate))
                  
                    break
                }
            }
        }
    }

    ## Remove yhat's again (because these were before removing swaps)
    subdat <- select(subdat, -SIS_NC_Aufgewertet_hat, -SIS_Resonator_Aufgewertet_hat, -SIS_Strahl_Aufgewertet_hat)
    
    return(subdat)
}

swap_resonator_below_strahl <- function(subdat){
  rows_to_swap <- which(subdat$SIS_Resonator_Aufgewertet < subdat$SIS_Strahl_Aufgewertet)
  for(row in rows_to_swap){
    temp <- subdat$SIS_Strahl_Aufgewertet[row]
    subdat$SIS_Strahl_Aufgewertet[row] <- subdat$SIS_Resonator_Aufgewertet[row]
    subdat$SIS_Resonator_Aufgewertet[row] <- temp
    subdat[row, "korrigiert_SIS_Strahl_Aufgewertet"] <- TRUE
    subdat[row, "korrigiert_SIS_Resonator_Aufgewertet"] <- TRUE
    subdat <- edit_aufwertungsbeschreibung(subdat, row=row, swap=c(2, 3))
  }
  return(subdat) 
}
