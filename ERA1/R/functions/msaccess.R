update_table_in_accdb <- function(dat_corrected, accdb_path=NA, table=NA, verbose=FALSE){
  accdb <- odbcConnectAccess2007(accdb_path)
  
  # replace NAs with -99 again for the database
  for(zaehler in ZAEHLER){  
    dat_corrected[[zaehler]] <- NA_to_minus99(dat_corrected[[zaehler]])
  }
  
  # Create all missing columns in DB:
  columns_in_db <- colnames(sqlQuery(accdb, paste("SELECT TOP 1 * FROM ", table)))
  columns_in_dat <- colnames(dat_corrected)
  add_these_columns <- columns_in_dat[which(!(columns_in_dat %in% columns_in_db))]
  for(col in add_these_columns){
    query <- paste("ALTER TABLE", table, "ADD COLUMN", col, "NUMERIC")
    res <- sqlQuery(accdb, query)
  }
  
  write_these_columns <- c(
    "Deaktiviert",                             "Aufwertungsbeschreibung",
    "SIS_NC_Aufgewertet",                      "SIS_Resonator_Aufgewertet",              
    "korrigiert_SIS_NC_Aufgewertet",           "SIS_Strahl_Aufgewertet", 
    "korrigiert_SIS_Resonator_Aufgewertet",    "korrigiert_SIS_Strahl_Aufgewertet",      
    "manual_SIS_NC_Aufgewertet",               "manual_SIS_Resonator_Aufgewertet",       
    "manual_SIS_Strahl_Aufgewertet",           "Bereich_SIS_NC_Aufgewertet",             
    "Offset_SIS_NC_Aufgewertet",               "Bereich_SIS_Resonator_Aufgewertet",      
    "Offset_SIS_Resonator_Aufgewertet",        "Bereich_SIS_Strahl_Aufgewertet",         
    "Offset_SIS_Strahl_Aufgewertet",           "SIS_NC_Aufgewertet_hat",                 
    "SIS_Resonator_Aufgewertet_hat",           "SIS_Strahl_Aufgewertet_hat",             
    "SIS_NC_Aufgewertet_resid_abs",            "SIS_NC_Aufgewertet_resid_rel",           
    "SIS_Resonator_Aufgewertet_resid_abs",     "SIS_Resonator_Aufgewertet_resid_rel",    
    "SIS_Strahl_Aufgewertet_resid_abs",        "SIS_Strahl_Aufgewertet_resid_rel",       
    "SIS_NC_Aufgewertet_Steigung",             "SIS_Resonator_Aufgewertet_Steigung",     
    "SIS_Strahl_Aufgewertet_Steigung",         "NC_zu_SA",                               
    "NC_zu_RA",                                "RA_zu_SA",                               
    "quality_score",                           "Differenztage",                          
    "Differenztage_SIS_NC_Aufgewertet",        "Differenztage_SIS_Resonator_Aufgewertet",
    "Differenztage_SIS_Strahl_Aufgewertet",    "SIS_NC_Steigung",                        
    "SIS_Resonator_Steigung",                  "SIS_Strahl_Steigung",                    
    "NC_zu_SA_vorher",                         "NC_zu_RA_vorher",                        
    "RA_zu_SA_vorher"   
  )
  
  write_these_columns <- write_these_columns[write_these_columns %in% columns_in_dat]
  
  for(col in write_these_columns){
    if(is.logical(dat_corrected[[col]])){
      dat_corrected[[col]] <- as.numeric(dat_corrected[[col]])
    }
    dat_corrected[[col]] <- NA_to_minus99(dat_corrected[[col]])
  }
  
  ## Now, row-wise, add the new data to the table:
  for(row in 1:nrow(dat_corrected)){
    if(verbose){
      cat("writing accdb, row", row, "of", nrow(dat_corrected), "\n")
    }
    query <- paste("UPDATE", table, "SET")
    
    ## add all columns computed in the script, and the original ZAEHLER columns:
    add_columns <- sapply(write_these_columns, function(col){
      value <- dat_corrected[[col]][row]
      if(is.numeric(value)){
        paste0(col, " = ", value) 
      } else {
        paste0(col, " = '", value, "'") 
      }
    }) %>% paste(collapse=", ")
    query <- paste(query, add_columns)
    
    query <- paste(query, " WHERE ID =", dat_corrected$ID[row])
    
    ## execute SQL query
    res <- sqlQuery(accdb, query)
    if(length(res) != 0){  # res only has contents if there was an error, otherwise it's character(0)
      stop(paste0("SQL Query for row ", row, " has encountered an error!"))
    }
  }
  
  odbcClose(accdb)
}
