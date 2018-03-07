add_offset_bereiche <- function(subdat){
    ## Diese Funktion kann momentan nur einen Offset pro Zaehler erkennen,
    ## d.h. keine Bereiche 1, 2, und 3 (etc.) erkennen.
    for(col in ZAEHLER){
        bereichsvariable <- paste0("Bereich_", col)
        offsetvariable <- paste0("Offset_", col)  # numerisch
        subdat[[bereichsvariable]] <- 1
        subdat[[offsetvariable]] <- 0
        
        res <- identify_offset(subdat, col)
        if(res!=FALSE){
            subdat[[bereichsvariable]][(res+1):nrow(subdat)] <- 2
            subdat[[offsetvariable]][(res+1):nrow(subdat)] <- get_offset(subdat, col, res)
        }
    }
    return(subdat)
}

identify_offset <- function(subdat, col, check_next=5){
    subdat$row_id <- 1:nrow(subdat)  # including possible NAs
    subdat <- subdat[!is.na(subdat[[col]]), ]
    if(nrow(subdat) < config$min_obs_for_offset) return(FALSE)

    slopes <- get_slopes(subdat, col)
    neg_slopes <- which(slopes < 0)

    candidates <- neg_slopes[order(diff(subdat[[col]])[neg_slopes])]  # sort by largest *absolute* negative diff

    diff_factors <- exp(diff(log(subdat[[col]])))
    diffs <- diff(subdat[[col]])
    large_relative_drop <- diff_factors[candidates] < (1-config$offset_relative_drop)
    large_absolute_drop <- diffs[candidates] < -config$offset_absolute_drop

    candidates <- candidates[large_relative_drop & large_absolute_drop]

    for(l_obs in candidates){
        
        if(l_obs < 4 || l_obs > (length(slopes)-4)) next  # if the left or right side has too little data
      
        l_idx <- 1:l_obs
        r_idx <- (l_obs+1):nrow(subdat)
        subdat_l <- subdat[l_idx, ]
        subdat_r <- subdat[r_idx, ]

        ## the next four statements check whether the slopes of the
        ## left and right part are similar (within 1.5 times each other).
        ## If not, (A0060A0595, after observation 4, was such a case), 
        ## it's probably not an offset.
        avg_slope_l <- get_weighted_avg_slope(subdat_l, col)
        avg_slope_r <- get_weighted_avg_slope(subdat_r, col)
        slope_quote <- avg_slope_l / avg_slope_r
        factor_threshold <- 1.5
        if(slope_quote < 1/factor_threshold || slope_quote > factor_threshold){
          next
        }
        
        form <- as.formula(paste0(col, " ~ Erfassungsdatum"))
        lm_l <- lm(form, data=subdat_l[1:(nrow(subdat_l)-1),])  # drop last obs in case this is the outlier

        if(is.nan(summary(lm_l)$r.squared)) next  # if left data is constant (e.g. A0035A0187, NC)

        if(summary(lm_l)$r.squared < 0.9) next  # drop case if the left part is not linear enough

        lm_l_yhat_r <- predict(lm_l, newdata=subdat_r)  # right predictions with left LM
        y_r_raw <- subdat_r[[col]]  # true right y's
        y_r_corrected <- correct_offset(subdat, col, l_obs)[[col]][r_idx]

        #### Method: Is the SSQ of the next n corrected right points less than 1% of the SSQ of the left points?
        check_next_i <- min(check_next, length(lm_l_yhat_r))
        resid_raw <- lm_l_yhat_r[1:check_next_i] - y_r_raw[1:check_next_i]
        resid_corrected <- lm_l_yhat_r[1:check_next_i] - y_r_corrected[1:check_next_i]
        ssq_leave_it <- sum(resid_raw)^2
        ssq_correct_it <- sum(resid_corrected)^2  # which smaller?
        if(ssq_correct_it < config$offset_if_SSQ_ratio_below * ssq_leave_it){
            return(subdat$row_id[l_obs])
        }
    }
    return(FALSE)  # no offset found
}

get_offset <- function(subdat, col, pos){
    subdat$row_idx <- 1:nrow(subdat)

    l_idx <- 1:pos
    r_idx <- (pos+1):nrow(subdat)
    subdat_l <- subdat[l_idx, ]
    subdat_r <- subdat[r_idx, ]

    lm_left <- lm(subdat_l[[col]] ~ subdat_l$Erfassungsdatum)
    first_nonNA_index <- which(!is.na(subdat_r[[col]]))[1]
    y_interpolated <- coef(lm_left)[1] + coef(lm_left)[2] * as.numeric(subdat_r$Erfassungsdatum[first_nonNA_index])
    add_intercept <- round(y_interpolated - subdat_r[[col]][first_nonNA_index])

    return(add_intercept)
}

correct_offset <- function(subdat, col, pos){
    subdat$row_idx <- 1:nrow(subdat)

    l_idx <- 1:pos
    r_idx <- (pos+1):nrow(subdat)
    subdat_l <- subdat[l_idx, ]
    subdat_r <- subdat[r_idx, ]

    lm_left <- lm(subdat_l[[col]] ~ subdat_l$Erfassungsdatum)
    first_nonNA_index <- which(!is.na(subdat_r[[col]]))[1]
    y_interpolated <- coef(lm_left)[1] + coef(lm_left)[2] * as.numeric(subdat_r$Erfassungsdatum[first_nonNA_index])
    add_intercept <- round(y_interpolated - subdat_r[[col]][first_nonNA_index])

    y_corrected <- subdat_r[[col]] + add_intercept
    subdat[subdat_r$row_idx, col] <- y_corrected
    return(subdat)
}

add_offsets_if_exist <- function(subdat, subtract=FALSE, smooths_too=FALSE){
    for(col in ZAEHLER){
        offset_col <- paste0("Offset_", col)
        if(has_column(subdat, offset_col)){
            if(any(is.na(subdat[[offset_col]]))){
              stop("Offsets must not be NA. add_offset_bereiche() or correct_machine() should have set them to zero!")
            }
            plusminus <- ifelse(subtract, -1, +1)

            subdat[[col]] <- subdat[[col]] + plusminus * subdat[[offset_col]]
            if(smooths_too){
                subdat[[paste0(col, "_hat")]] <- subdat[[paste0(col, "_hat")]] + plusminus * subdat[[offset_col]]
            }
        }
    }
    return(subdat)
}
