
# runUS.R -----------------------------------------------------------------

# this script processess data from US CDC website 
# and imports it into R, then creates csv files.



# Libraries ---------------------------------------------------------------

library(XML)
library(stringr)
library(lubridate)
library(reshape2)

# Read html from website --------------------------------------------------

theurl <- "http://www.cdc.gov/zika/geo/united-states.html"
#theurl <- "V:\\Projects\\zika\\test.html"
readData <- function(theurl){
  ## check existence of url
  #con.url <- try(url(theurl, open='rb'))
  #try.error <- inherits(con.url, "try-error")
  
  #if(try.error) stop("Error: URL Does not exist.") # redundant but just in case
  
#   tables <- readHTMLTable(theurl,header=FALSE)
#   tables
  webPage <- readLines(theurl)
  
  
  tables <- grep("<thead>", webPage)
  tablesEnd <- grep("</table>", webPage)

# check for table lengths and position  
  stopifnot(length(tables) == length(tablesEnd))
  stopifnot(all(tables < tablesEnd))
  
# loop through and subset tables
# look for header information
  
  x <- list()
  for(i in 1:length(tables)){
    aTable <- webPage[tables[i]:tablesEnd[i]]
    headers <- grep("<th>.*?</th>", aTable, value=TRUE)
    headers <- gsub("</?th>", "", headers)
   
    x[[i]] <- headers
  }
  
  closeAllConnections()
  
  xmatch <- lapply(x, function(x) {
    c(pmatch("States", x),
      pmatch("Travel", x),
      pmatch("Locally",x))
  }
  )
  
  
  xmatches <- lapply(xmatch, function(x) all(!is.na(x)))
  stopifnot(length(xmatches) ==1)
  tableNum <- which(xmatches==TRUE)
  
  
  #xmatch[xmatches]
  
  tables <- readHTMLTable(theurl, header = TRUE, stringsAsFactors = FALSE)
  zikaData <- tables[[tableNum]]
  row1 <- names(zikaData)
  
  zikaData <- rbind(row1, zikaData)
  header <- c("location", "US0001", "US0002")
  header <- header[xmatch[[1]]]
  names(zikaData) <- header
  
  #remove blank locations
  zikaData <- zikaData[zikaData$location !="",]
  zikaData <- zikaData[!grepl("\\(.*?\\)", zikaData$US0001),]
  zikaData <- zikaData[!grepl("\\(.*?\\)", zikaData$US0002),]
  
  
  #remove spaces in location
  zikaData$location <- gsub(" ", "_", zikaData$location)
  
  #now get the date 
  dateLine <- grep("Laboratory-confirmed Zika virus disease cases reported to ArboNET.*as of", webPage, value=TRUE)
  stopifnot(length(dateLine)==1)
  theDate <- stringr::str_extract_all(string=dateLine, pattern='\\w+\\s\\d+(st)?(nd)?(rd)?(th)?,\\s+\\d+')
  theDate <- lubridate::mdy(theDate)
  theDate <- format(theDate, "%Y-%m-%d")
  zikaData$report_date <- theDate
  
  write(webPage, file=paste0("US_Zika-", theDate, ".html"))
  return(zikaData)
}

cdcZika <- readData(theurl)


# Create Places file ------------------------------------------------------
location_type <- rep(NA, length(cdcZika$location))
state.name.und <- gsub(" ", "_", state.name)
location_type[cdcZika$location %in% state.name.und] <- "state"
location_type[cdcZika$location=="District_of_Columbia"] <- "state"
location_type[is.na(location_type)] <- "territory"

country <- rep("United_States", length(cdcZika$location))

state_province <- cdcZika$location
location <- paste("United_States", cdcZika$location, sep="-")
district_county_municipality <- rep(NA, length(cdcZika$location))
city <- rep(NA, length(cdcZika$location))
            
US_places <- data.frame(location = location, location_type = location_type,
                        country = country, state_province = state_province,
                        district_county_municipality, city = city)






# Read codes then merge and melt ------------------------------------------

codeZ <- read.csv("../US_Data_Guide.csv")

US_Zika <- melt(cdcZika, id.vars = c("report_date", "location"))
US_Zika <- merge(US_Zika, codeZ, by.x = "variable", by.y = "data_field_code")
US_Zika$location <- paste("United_States", US_Zika$location, sep="-")



US_Zika <- merge(US_Zika, US_places, by = "location")
names(US_Zika)[names(US_Zika)=="variable"] <- "data_field_code"
US_Zika$time_period <- NA

US_Zika <- US_Zika[c("report_date", "location", "location_type", "data_field",
          "data_field_code","time_period", "time_period_type",
          "value", "unit")]
# Write CSV Files ---------------------------------------------------------

write.csv(US_places, file = "./US_Places.csv", row.names = FALSE)
write.csv(US_Zika, file=paste0("US_Zika-", US_Zika$report_date[[1]], ".csv"),
          row.names = FALSE)

