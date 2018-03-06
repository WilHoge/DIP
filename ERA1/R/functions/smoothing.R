add_yhats <- function(subdat){
  subdat <- add_offsets_if_exist(subdat)  # before smoothing, temporarily add offsets to the counters
  
  mods <- get_models(subdat)
  for(col in ZAEHLER){
    subdat[[paste0(col, "_hat")]] <- NA
    if(!isTRUE(is.na(mods[[col]]))){
      non_na_x <- !is.na(subdat$Stunden_ab_IB1)
      subdat[[paste0(col, "_hat")]][non_na_x] <- predict(mods[[col]], z=as.numeric(subdat$Erfassungsdatum)[non_na_x])[,2]  # cobs specific predict
    }
  }
  
  subdat <- add_offsets_if_exist(subdat, subtract=TRUE, smooths_too=TRUE)  # remove the offsets, but from the counters AND smooths
  return(subdat)
}

get_models <- function(subdat){
    mods <- list()
    for(col in ZAEHLER){
        enough_observations <- sum(!is.na(subdat[[col]])) >= config$smoothing_min_obs # only compute the smoothing model if we have more than 3 measurements
        if(enough_observations){  
            mods[[col]] <- cobs(as.numeric(subdat$Erfassungsdatum), subdat[[col]], constraint="increase", 
                                degree=config$smoothing_degree, lambda=config$smoothing_penalty, 
                                knots=as.numeric(subdat$Erfassungsdatum)[!is.na(subdat[[col]])])
        } else {
            mods[[col]] <- NA
        }
    }
    return(mods)
}


add_residuals <- function(subdat, absolute=TRUE, relative=TRUE){
    if(!all(paste0(ZAEHLER, "_hat") %in% colnames(subdat))){  # add smooths if smooths are not yet added
        subdat <- add_yhats(subdat)
    }

    for(zaehler in ZAEHLER){
        smooth_col <- paste0(zaehler, "_hat")
        absolute_col <- paste0(zaehler, "_resid_abs")
        relative_col <- paste0(zaehler, "_resid_rel")
        
        if(absolute){
            subdat[[absolute_col]] <- subdat[[zaehler]] - subdat[[smooth_col]]
        }
        if(relative){
            subdat[[relative_col]] <- (subdat[[zaehler]] - subdat[[smooth_col]]) / subdat[[zaehler]]
        }
    }
    return(subdat)
}
