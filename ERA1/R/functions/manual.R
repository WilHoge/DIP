fix_trivial_errors <- function(subdat){
    subdat <- add_offset_bereiche(subdat)
    subdat <- swap_vertauscher(subdat)  # TODO soll warnung geben, wenn spalte $bereich nicht existiert, und sie hinzufuegen
    return(subdat)
}

compute_quality_score <- function(subdat){

    if(all(is.na(subdat$Erfassungsdatum))){
      QS = 0
      return(QS)
    }
  
    subdat <- fix_trivial_errors(subdat)  # i.e. compute quality score *after* fixing offsets and swaps

    ## - max_IQR not so good, since relative errors are big when y near zero
    ## - diff_slopes not so good since giant slope with small dx is no problem
    ## - the area between interpolated y's and the smooth did not work so well
    ## - the median absolute deviation (or the 70%-quantile of the absolute residuals) work well

    subdat <- add_yhats(subdat)
    ADs <- list()  # absolute differences
    for(col in ZAEHLER){
        ADs[[col]] <- abs(subdat[[col]] - subdat[[paste0(col, "_hat")]])
    }
    if(all(is.na(Reduce(c, ADs)))){  # if too few observations, what do?
        return(NA)
    }
    # median absolute difference:
    MAD <- Reduce(c, ADs) %>% na.omit %>% quantile(0.7) %>% unname()
    
    ## The MAD is larger for worse machines. 
    ## Transform it into a quality score between 0 and 10000:
    
    QS <- 100 / max(0.01, MAD)
    
    return(QS)
}
