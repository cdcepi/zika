################################################################################
# The following function will validate your csv file before you load it into the 
# repo.
################################################################################

validate <- function(this_csv, data_guide, places) {    
     require(stringr)
     require(dplyr)
     require(lubridate)
     error_found <- FALSE
     
     # Check date format
     if (sum(grepl(this_csv$report_date, pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}")) < nrow(this_csv)) {
          print("This file has wrong date formats on `report_date` column.")
          print("Please, change date formats to 'YYYY-MM-dd'.")
          error_found <- TRUE
     } 
     
     # Check for whitespace
     if (any(whitespace_found <-
             apply(sapply(this_csv, str_detect, pattern = "\\s"), 1, any, na.rm=T))) {
          print("White spaces found in these rows:")
          print(select(this_csv[whitespace_found, ], report_date, location, location_type, 
              data_field, data_field_code)) 
          error_found <- TRUE
          }    
     
     # Check for NAs and output columns containing NAs. Please verify them.
     if (any(na_cols <- sapply(this_csv, function(x) any(is.na(x))))) {
       if (any(ok.na.cols <- na_cols[c("time_period", "time_period_type", "value")])) {
          print("NAs found in these columns (may be okay):")
          print(names(ok.na.cols))
          }
       if (any(error.na.cols <- na_cols[c("report_date", "location", 
            "location_type", "data_field", "data_field_code", "unit")])) {
          print("NAs found in these columns (error):")
          print(names(error.na.cols))
          error_found <- TRUE
          }
        }
     
     ## Check for duplicated rows
     if (sum(dup_idx <- duplicated(select(this_csv, -value))) > 0) {
          print("Duplicated fields found in these rows:")
          print(this_csv[which(dup_idx), ])
          error_found <- TRUE
          }
     
     # test for mismatches in the data guide
     # using dplyr::anti_join: Output will show mismatching rows in dataset
     if (nrow(
          dg_mismatch <- suppressWarnings(anti_join(this_csv, data_guide,
                                                    by=c("data_field_code", "data_field")))) > 0) {
          print("These data_field_code and data_field pair(s) did not match the data guide:")
          print(select(dg_mismatch, data_field_code, data_field))
          error_found <- TRUE
          }
     
     # test for mismatches in the places file
     if (nrow(
          place_mismatch <- suppressWarnings(anti_join(this_csv, places, 
                                                       by=c("location", "location_type")))) > 0) {
          print("These location and location_type pair(s) did not match the places file:")
          print(select(place_mismatch, location, location_type))
          error_found <- TRUE
          }
     
     if (error_found) {
          warning("Have another coffee and fix the errors listed above")
          } else print("No errors found! Go outside and enjoy the day")
} 
