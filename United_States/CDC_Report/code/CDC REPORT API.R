#' Scrape and save US Zika data
#' 
#' \code{cdc_report} scrapes and saves CDC Zika data as reported by cdc.gov for 
#' each US state from webarchives: 
#' https://web.archive.org/web/20170101000000*/https://www.cdc.gov/zika/geo/united-states.html
#'
#' @param data_guide Data.frame. This is the data guide from the github repo:
#' https://github.com/cdcepi/zika/tree/master/United_States
#' @param url The webarchive URL where the versioning of cdc.gov occurred
#' @param date The date of the report.
#' @return This function will require that the user construct a data frame with 
#' two variables: url and date, corresponding to webarchives' url and date of 
#' snapshot available from cdc.gov.
#' @examples
#' # Save a data frame called archive with names() url, date and load:
#' a <- read.csv("original_reports/archive.csv", header=T, stringsAsFactors = F)
#' # Import data guide from repo:
#' dguide <- read.csv("US_Data_Guide.csv", header=T, stringsAsFactors=F)
#' # run a for loop to obtain the number of reports added in the archive data frame:
#' for (i in 1:nrow(a)) {
#'  cdc_report(data_guide = dguide, url=a[i,2], date=a[i,1])
#' }
#' # Save those into the github repo

cdc_report <- function(data_guide, url, date) {
  library(stringr, quietly = T, verbose = F)
  library(dplyr, quietly = T, verbose = F)
  library(htmltab, quietly = T, verbose = F)
  error_found <- FALSE
  
  my.url <- url
  
  reportDate = date
  
  my.table <- htmltab(doc = my.url, which = "//table[@class='table table-bordered']//tbody")
  
  if(!exists("my.table")) {
    error_found <- TRUE
    }
  
  colnames(my.table) <- c("location", "US0001", "US0002")
  
  
  #remove parenthesis and blank locations
  my.table$US0001 <- gsub("\\(.*?\\)", "", my.table$US0001)
  my.table$US0002 <- gsub("\\(.*?\\)", "", my.table$US0002)
  my.table$US0001 <- gsub("?", "", my.table$US0001)
  my.table$US0002 <- gsub("?", "", my.table$US0002)
  my.table$US0001 <- gsub(",", "", my.table$US0001)
  my.table$US0002 <- gsub(",", "", my.table$US0002)
  my.table$US0001 <- gsub("[[:punct:]]", "", my.table$US0001)
  my.table$US0002 <- gsub("[[:punct:]]", "", my.table$US0002)
  
  my.table$location <- gsub("?", "", my.table$location)
  my.table$location <- gsub("???", "", my.table$location)
  
  my.table$US0001 <- as.numeric(stringr::str_trim(my.table$US0001))
  my.table$US0002 <- as.numeric(stringr::str_trim(my.table$US0002))
  my.table <- my.table[!is.na(my.table$US0001) & !is.na(my.table$US0002), ]
  
  #remove spaces in location
  my.table$location <- gsub(" ", "_", my.table$location)
  my.table$location <- paste("United_States", my.table$location, sep="-")
  
  #build table
  US0001 <- my.table %>%
    mutate(
      report_date = reportDate,
      location = location,
      location_type = 
        ifelse(location == "United_States-American_Samoa" |
               location == "United_States-Puerto_Rico" |
               location == "United_States-US_Virgin_Islands",
                    "territory", "state"),
      data_field = data_guide$data_field[which(data_guide$data_field_code == "US0001")],
      data_field_code = "US0001",
      time_period = NA,
      time_period_type = NA,
      value = US0001,
      unit = "cases"
      ) %>%
    select(report_date, location, location_type, data_field, data_field_code,
           time_period, time_period_type, value, unit)
  
    US0002 <- my.table %>%
    mutate(
      report_date = reportDate,
      location = location,
      location_type = 
        ifelse(location == "United_States-American_Samoa" |
               location == "United_States-Puerto_Rico" |
               location == "United_States-US_Virgin_Islands",
                    "territory", "state"),
      data_field = data_guide$data_field[which(data_guide$data_field_code == "US0002")],
      data_field_code = "US0002",
      time_period = NA,
      time_period_type = NA,
      value = US0002,
      unit = "cases"
      ) %>%
    select(report_date, location, location_type, data_field, data_field_code,
           time_period, time_period_type, value, unit)
    
    report <- rbind(US0001, US0002)
    
    if(error_found) {
      warning("Error in htmltab function.")
      warning("Please check that the URL is correct")
    } else {
      print("Very good! Writing into repo!")
    }
    
    write.csv(report, paste0(sep = "CDC_Report-", reportDate, sep = ".csv"), row.names=F)
}
