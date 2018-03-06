plot_base_w_smooth <- function(subdat, legend=FALSE, plot.smooth=TRUE, ...){
    xlim <- range(subdat$Stunden_ab_IB1)
    if(any(is.na(xlim))){
        warning("NAs in Stunden_ab_IB1!")
        return(NA)
    }
    
    values <- Reduce(c, subdat[,ZAEHLER])
    if(all(is.na(values))) return(NA)  # e.g. A0435A0001 has all NA values in all counters
    ylim <- range(values, na.rm=TRUE)
    plot(SIS_NC_Aufgewertet~Stunden_ab_IB1, data=na.omit(subdat[,c("Stunden_ab_IB1", "SIS_NC_Aufgewertet")]), type="o", lty=2, ylim=ylim, xlim=xlim, ylab="Hours", ...)
    lines(SIS_Resonator_Aufgewertet~Stunden_ab_IB1, data=na.omit(subdat[,c("Stunden_ab_IB1", "SIS_Resonator_Aufgewertet")]), type="o", col=2, lty=2)
    lines(SIS_Strahl_Aufgewertet~Stunden_ab_IB1, data=na.omit(subdat[,c("Stunden_ab_IB1", "SIS_Strahl_Aufgewertet")]), type="o", col=3, lty=2)

    if(plot.smooth){
        mods <- get_models(subdat)
        for(i in 1:3){
            if(isTRUE(is.na(mods[[i]]))) next
            x <- as.numeric(subdat$Erfassungsdatum)  # model goes with Erfassungsdatum
            yhat <- predict(mods[[i]], z=x)[,2]
            x_plot <- subdat$Stunden_ab_IB1  # plot goes with Stunden_ab_IB1
            lines(x_plot, yhat, col=i, lwd=2)
        }
    }
    if(legend){
      legend("topleft", lty=2, col=1:3, legend=c("NC","Resonator","Strahl"))
    }
}

machineplot <- function(subdat, title=NA){
    par(oma=c(0,0,0,0))
    layout(matrix(c(1,1,1,2,2,3,3)))
    par(mar=c(2,4,2,2))  # bottom, left, top, right
    res <- plot_base_w_smooth(subdat, main=title)
    if(isTRUE(is.na(res))) return(NA)
    par(mar=c(2,4,2,2))
    plot_slopes(subdat)
    par(mar=c(4,4,2,2))
    plot_proportions(subdat, props="absolute", do_not_interpolate=TRUE)
}

slope_fct <- function(subdat, col, x_new){
    x <- subdat$Stunden_ab_IB1[!is.na(subdat[[col]])]
    y <- get_slopes(subdat, col)
    if(length(x)==0 || length(y)==0){
        x <- c(0,0)
        y <- 0
    }
    slope_fct <- stepfun(x, c(0, y, 0))
    return(slope_fct(x_new))
}

plot_slopes <- function(subdat){
    slope_functions <- list()
    x_new <- seq(min(subdat$Stunden_ab_IB1), max(subdat$Stunden_ab_IB1))
    for(col in ZAEHLER){
        slope_functions[[col]] <- slope_fct(subdat, col, x_new)
    }
    y_lim <- lapply(1:3, function(i) slope_functions[[i]]) %>% Reduce(c, .) %>% range()
    if(y_lim[1] < -0.5)  y_lim[1] <- -0.5
    plot(x_new, slope_functions[["SIS_NC_Aufgewertet"]], type="l", ylim=y_lim,
         xlab="Stunden_ab_IB1", ylab="Slopes")
    lines(x_new, slope_functions[["SIS_Resonator_Aufgewertet"]], col=2)
    lines(x_new, slope_functions[["SIS_Strahl_Aufgewertet"]], col=3)
    abline(h=0, lty=2)
}

plot_proportions <- function(subdat, props=c("slopes","absolute"), do_not_interpolate=TRUE){
    props <- match.arg(props)
    x_new <- seq(min(subdat$Stunden_ab_IB1), max(subdat$Stunden_ab_IB1))

    if(props=="slopes"){
        slope_functions <- list()
        for(col in ZAEHLER){
            slope_functions[[col]] <- slope_fct(subdat, col, x_new)
        }

        NC_to_Re <- slope_functions[["SIS_NC_Aufgewertet"]] / slope_functions[["SIS_Resonator_Aufgewertet"]]
        NC_to_St <- slope_functions[["SIS_NC_Aufgewertet"]] / slope_functions[["SIS_Strahl_Aufgewertet"]]
        Re_to_St <- slope_functions[["SIS_Resonator_Aufgewertet"]] / slope_functions[["SIS_Strahl_Aufgewertet"]]

        y_lab <- "Proportion of slopes"
        finite_y_vals <- c(NC_to_Re, NC_to_St, Re_to_St)[!is.na(c(NC_to_Re, NC_to_St, Re_to_St)) & c(NC_to_Re, NC_to_St, Re_to_St) < Inf & c(NC_to_Re, NC_to_St, Re_to_St) > -Inf]
        if(length(finite_y_vals)==0){  # if each row in subdat has only one counter available
          ylim <- c(0,0)
        } else {
          ylim <- range(finite_y_vals)
        }
        
        plot(x_new[!is.na(NC_to_Re)], NC_to_Re[!is.na(NC_to_Re)], type="l", ylim=ylim, col=4,
             xlab="Stunden_ab_IB1", ylab=y_lab)
        lines(x_new[!is.na(NC_to_St)], NC_to_St[!is.na(NC_to_St)], type="l", col=5)
        lines(x_new[!is.na(Re_to_St)], Re_to_St[!is.na(Re_to_St)], type="l", col=6)
        abline(h=0, lty=2)
        legend("topleft", col=4:6, lty=1, legend=c("NC/Re","NC/St","Re/St"), bg="white")
        
    } else if(props=="absolute"){
        if(!do_not_interpolate){
          interpolated <- list()
          for(col in ZAEHLER){
              x <- subdat$Stunden_ab_IB1[!is.na(subdat[[col]])]
              y <- subdat[[col]][!is.na(subdat[[col]])]
              if(length(x) <= 1 || length(y) <= 1){
                  x <- range(x_new)
                  y <- c(0,0)
              }
              interpolated[[col]] <- approx(x,
                                            y,
                                           xout=x_new)
          }
          NC_to_Re <- interpolated[["SIS_NC_Aufgewertet"]]$y / interpolated[["SIS_Resonator_Aufgewertet"]]$y
          NC_to_St <- interpolated[["SIS_NC_Aufgewertet"]]$y / interpolated[["SIS_Strahl_Aufgewertet"]]$y
          Re_to_St <- interpolated[["SIS_Resonator_Aufgewertet"]]$y / interpolated[["SIS_Strahl_Aufgewertet"]]$y
        } else if(do_not_interpolate){
          x_new <- subdat$Stunden_ab_IB1
          subdat <- interpolate_for_proportions_plot(subdat)
          NC_to_Re <- subdat$SIS_NC_Aufgewertet / subdat$SIS_Resonator_Aufgewertet
          NC_to_St <- subdat$SIS_NC_Aufgewertet / subdat$SIS_Strahl_Aufgewertet
          Re_to_St <- subdat$SIS_Resonator_Aufgewertet / subdat$SIS_Strahl_Aufgewertet
        }
        
        y_lab <- "Proportion of y values"
        finite_y_vals <- c(NC_to_Re, NC_to_St, Re_to_St)[!is.na(c(NC_to_Re, NC_to_St, Re_to_St)) & c(NC_to_Re, NC_to_St, Re_to_St) < Inf & c(NC_to_Re, NC_to_St, Re_to_St) > -Inf]
        if(length(finite_y_vals)==0){  # if each row in subdat has only one counter available
          ylim <- c(0,0)
        } else {
          ylim <- range(finite_y_vals)
        }
        
        plot(x_new[!is.na(NC_to_Re)], NC_to_Re[!is.na(NC_to_Re)], type="o", ylim=ylim, col=4,
             xlab="Stunden_ab_IB1", ylab=y_lab)
        lines(x_new[!is.na(NC_to_St)], NC_to_St[!is.na(NC_to_St)], type="o", col=5)
        lines(x_new[!is.na(Re_to_St)], Re_to_St[!is.na(Re_to_St)], type="o", col=6)
        abline(h=0, lty=2)
        legend("topleft", col=4:6, lty=1, legend=c("NC/Re","NC/St","Re/St"), bg="white")
        
    }
}

interpolate_for_proportions_plot <- function(subdat){
  ## for each counter, if there is a NA *within* the data, linearly interpolate it.
  ## for NAs at the head or tail of a counter, ignore.
  ## e.g., c(NA, 3, 5, 7) will be left alone. But c(1, 2, NA, 4) will be imputed to 1,2,3,4
  
  for(col in ZAEHLER){
    subdat[[col]] <- round(approx(x=subdat$Stunden_ab_IB1[!is.na(subdat[[col]])],
                           y=subdat[[col]][!is.na(subdat[[col]])],
                           xout=subdat$Stunden_ab_IB1)$y)
  }
  subdat
}

plot_correction <- function(subdat_before){
  id <- subdat_before$EquipNr[1]
  
  if(nrow(subdat_before)==1) return(FALSE)
  
  QS <- compute_quality_score(subdat_before)
  if(!is.na(QS) && QS < config$QS_threshold_for_manual_flag){
    png(paste0("../img/gesamt/", id, ".png"), width=1.5*480, height=1.5*480)
    plot_base_w_smooth(subdat_before, plot.smooth=FALSE, main=paste0("Original (QS: ", round(QS, 2), ")"))
    dev.off()
    file.copy(paste0("../img/gesamt/", id, ".png"), paste0("../img/gesamt/manual/", id, ".png"))
    return(FALSE)
  }
  
  subdat_offsets <- add_offset_bereiche(subdat_before)
  subdat_vertauscher <- swap_vertauscher(subdat_offsets)
  subdat_vertauscher <- swap_resonator_below_strahl(subdat_vertauscher)
  subdat_monotone <- remove_too_low_slopes(subdat_vertauscher)
  subdat_monotone <- remove_too_high_slopes(subdat_monotone)
  
  offset_found <- any(subdat_offsets$Offset_SIS_NC_Aufgewertet != 0) || any(subdat_offsets$Offset_SIS_Resonator_Aufgewertet != 0) || any(subdat_offsets$Offset_SIS_Strahl_Aufgewertet != 0)
  vertauscher_corrected <- !identical(subdat_vertauscher[,ZAEHLER], subdat_offsets[,ZAEHLER])
  negatives_corrected <- !identical(subdat_monotone[,ZAEHLER], subdat_vertauscher[,ZAEHLER])
  
  png(paste0("../img/gesamt/", id, ".png"), width=1.5*480, height=1.5*480)
  n_plots <- 1 + offset_found + vertauscher_corrected + negatives_corrected
  
  layout(matrix(1:n_plots, ncol=1))
  
  plot_base_w_smooth(subdat_before, plot.smooth=FALSE, main=paste0("Original (QS: ", round(QS, 2), ")"))
  if(offset_found){
    plot_base_w_smooth(add_offsets_if_exist(subdat_offsets), plot.smooth=FALSE, main="Offsets corrected")
  }
  if(vertauscher_corrected){
    plot_base_w_smooth(add_offsets_if_exist(subdat_vertauscher), plot.smooth=FALSE, main="Vertauscher corrected")
  }
  if(negatives_corrected){
    plot_base_w_smooth(add_offsets_if_exist(subdat_monotone), plot.smooth=FALSE, main="Negative slopes corrected")
  }
  dev.off()
  
  if(offset_found) file.copy(paste0("../img/gesamt/", id, ".png"), paste0("../img/gesamt/offset/", id, ".png"))
  if(vertauscher_corrected) file.copy(paste0("../img/gesamt/", id, ".png"), paste0("../img/gesamt/vertauscher/", id, ".png"))
  if(negatives_corrected) file.copy(paste0("../img/gesamt/", id, ".png"), paste0("../img/gesamt/monotone/", id, ".png"))
  if(!offset_found && !vertauscher_corrected && !negatives_corrected) file.copy(paste0("../img/gesamt/", id, ".png"), paste0("../img/gesamt/nothing/", id, ".png"))
}
