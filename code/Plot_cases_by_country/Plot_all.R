####################################################################################################
# This script will produce plots of confirmed and suspected ZIKV cases for each 
# country/territory with repo data. The script will produce one grid of cases by
# report date and a second of cumulative cases.
#
# This script uses two functions to plot the weekly and cumulative case numbers. 
# The functions are applied to "country_variable_selection.csv",  which has all of the info needed to plot.
# 
# You need to download the repo from github
#####################################################################################################

require(plyr)
require(ggplot2)
require(reshape2)
require(gridExtra)
require(grid)

# Set working directory to your downloaded repo.
# Change the path to your current working directory

setwd("PATH/TO/YOUR/DOWNLOADED/FOLDER")

# data frame of data guide info for each country
data_codes <- read.csv("code/Plot_cases_by_country/country_variable_selection.csv", header=TRUE, sep=",")
# convert all to type character 
data_codes[] <- lapply(data_codes, as.character)

# Function to calculate weekly number of confirmed and suspected cases from data (whether or not it is cumulative) 
# Produces plot of confirmed and suspected cases observed by date for a specific country/territory
weekly.cases <- function(file, country_name, confirmed_codes, suspected_codes, location_filter, conf_cumulative_check, susp_cumulative_check) {
  
  file.list = list.files(path = file, full.names = TRUE)

  # Merge dataset from each country
  country.data <- do.call("rbind", lapply(file.list, read.csv, header = TRUE))

  # remove rows with NA values for number of cases
  country.data <- country.data[!(is.na(country.data$value)), ]

  # Split strings confirmed_codes and suspected_codes for subseting data if not empty
  if (any(confirmed_codes != "")){
    confirmed_codes <- unlist(strsplit(confirmed_codes, ",")) }
  if (any(suspected_codes != "")){
    suspected_codes <- unlist(strsplit(suspected_codes, ",")) }
  
  
  # Split string of location filter
  location_filter <- unlist(strsplit(location_filter, ","))
  
  # Subset dataset by confirmed/suspected codes and filter by inputted location_type
  confirmed <- subset(country.data, country.data$data_field_code %in% confirmed_codes, drop = TRUE)
  confirmed <- subset(confirmed, confirmed$location_type %in% location_filter, drop=TRUE)
  suspected <- subset(country.data, country.data$data_field_code %in% suspected_codes, drop = TRUE)
  suspected <- subset(suspected, suspected$location_type %in% location_filter, drop=TRUE)
  
  # Sum across location and data type by report date (sum different types and locations of cases)
  confirmed <- ddply(confirmed, .(report_date), summarise, sum = sum(value))
  suspected <- ddply(suspected, .(report_date), summarise, sum = sum(value))
  
  # Use diff() to subtract cumulative cases to get weekly case count 
  # Append a 0 for the first week and bind the vector to corresponding dates
  # Check if there is data (confirmed_codes) and that it is cumulative (conf_cumulative_check)
  
  if (any(confirmed_codes != "")){
    if (conf_cumulative_check == "yes"){
      confirmed <- cbind(confirmed[1], append(0, diff(confirmed[,2])))
      colnames(confirmed) <- c("report_date", "value")} 
   else if (conf_cumulative_check == "no"){
     confirmed <- confirmed
     colnames(confirmed) <- c("report_date", "value")}
  }
  
  if (any(suspected_codes != "")){
    if (susp_cumulative_check == "yes"){
      suspected <- cbind(suspected[1], append(0, diff(suspected[,2])))
      colnames(suspected) <- c("report_date", "value") } 
    else if (susp_cumulative_check == "no"){
      suspected <- suspected
      colnames(suspected) <- c("report_date", "value") }
  }

  # Set negative values to 0 if not empty
  if (any(confirmed_codes != "")){
    confirmed[2:length(confirmed)][confirmed[2:length(confirmed)] < 0] <- 0 }
  
  if (any(suspected_codes != "")){
    suspected[2:length(suspected)][suspected[2:length(suspected)] < 0] <- 0 }
  
  # create data frame of just report date and value (in case there is no data for either confirmed or suspected)
  confirmed <- cbind(data.frame(confirmed$report_date, confirmed$value))
  suspected <- cbind(data.frame(suspected$report_date, suspected$value))
  
  # rename columns to distinguish confirmed and suspected value
  colnames(confirmed) <- c("report_date", "confirmed_value")
  colnames(suspected) <- c("report_date", "suspected_value")
 
   ##  longer data frame needs to be first for plotting
  if (length(suspected$report_date) > length(confirmed$report_date)){
    weekly.conf.susp <- melt(list(suspected,confirmed))
    colors <- c("#00BFC4", "#F8766D")
  } else {
    weekly.conf.susp <- melt(list(confirmed,suspected))
    colors <- c("#F8766D", "#00BFC4")
  }
  weekly.conf.susp$report_date <- as.Date(weekly.conf.susp$report_date)

weekly.plot <-  ggplot(data=weekly.conf.susp, aes(x=report_date, y=value, fill=variable)) + 
    geom_bar(stat="identity") + 
    geom_vline(aes(xintercept=as.numeric(as.Date("2016-01-01", origin="1970-01-01"))),linetype = "dashed") + 
    labs(x = "", y = "Number of cases",
         title = (country_name)) +
    theme(legend.position="none") + 
    scale_fill_manual(values=colors) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_date(date_labels = format("%b"), date_breaks = "month") 

weekly.plot + 
  annotate(geom="text", x=as.Date("2016-01-10"), 
           y=(max(ggplot_build(weekly.plot)$panel$ranges[[1]]$y.range)*.9), 
           label="2016", size=3, angle=90) +  
  geom_vline(aes(xintercept=as.numeric(as.Date("2016-01-01", origin="1970-01-01"))), linetype = "dashed") 


  
}

# Function to calculate cumulative number of confirmed and suspected cases from data 
# Produces plot of confirmed and suspected cases for a specific country/territory
cumulative.cases <- function(file, country_name, confirmed_codes, suspected_codes, location_filter, conf_cumulative_check, susp_cumulative_check){
  
  file.list = list.files(path = file, full.names = TRUE)
  
  # Merge dataset from one country
  country.data <- do.call("rbind", lapply(file.list, read.csv, header = TRUE))
  
  #remove rows with NA values for number of cases
  country.data<-country.data[!(is.na(country.data$value)), ]
  
  # split strings confirmed_codes and suspected_codes
     if (any(confirmed_codes != "")) {
       confirmed_codes <- unlist(strsplit(confirmed_codes, ",")) }
  

   if (any(suspected_codes != "")) {
     suspected_codes <- unlist(strsplit(suspected_codes, ",")) }
  
  location_filter <- unlist(strsplit(location_filter, ","))
  
  # subset dataset by confirmed/suspected codes and location type
  confirmed <- subset(country.data, country.data$data_field_code %in% confirmed_codes, drop = TRUE)
  suspected <- subset(country.data, country.data$data_field_code %in% suspected_codes, drop = TRUE)
  confirmed <- subset(confirmed, confirmed$location_type %in% location_filter, drop=TRUE)
  suspected <- subset(suspected, suspected$location_type %in% location_filter, drop=TRUE)
  
  # sum across location and data type by report date (sum cities/different categories of cases)
  confirmed <- ddply(confirmed, .(report_date), summarise, sum = sum(value))
  suspected <- ddply(suspected, .(report_date), summarise, sum = sum(value))
  
  # if count is new not cumulative, sum to get cumulative 
  if (conf_cumulative_check == "no"){
    confirmed <- cumsum(confirmed)
  }
  
  if (susp_cumulative_check == "no"){
    suspected$sum <- cumsum(suspected$sum)
  }
  
  # rename columns of data frame 
  colnames(confirmed) <- c("report_date", "value")
  colnames(suspected) <- c("report_date", "value")
  
  # remove negative values
  if (any(confirmed_codes != "")){
    confirmed[2:length(confirmed)][confirmed[2:length(confirmed)] < 0] <- 0 }
  
  if (any(suspected_codes != "")){
    suspected[2:length(suspected)][suspected[2:length(suspected)] < 0] <- 0 }
  
  # create data frame of just report date and value (in case there is no data for either confirmed or suspected)
  confirmed <- cbind(data.frame(confirmed$report_date, confirmed$value))
  suspected <- cbind(data.frame(suspected$report_date, suspected$value))
  
  # rename columns to distinguish confirmed and suspected value
  colnames(confirmed) <- c("report_date", "confirmed_value")
  colnames(suspected) <- c("report_date", "suspected_value")
  
  
  ##  longer data frame needs to be first for plotting
  if (length(suspected$report_date) > length(confirmed$report_date)){
    total.conf.susp <- melt(list(suspected,confirmed))
    colors <- c("#00BFC4", "#F8766D")
  } else {
    total.conf.susp <- melt(list(confirmed,suspected))
    colors <- c("#F8766D", "#00BFC4")
  }
  
  total.conf.susp$report_date <- as.Date(total.conf.susp$report_date)
  
cumulative.plot <-
  ggplot(data=total.conf.susp, aes(x=report_date, y=value, fill=variable)) + 
    geom_bar(stat="identity") + 
    labs(x = "", y = "Number of cases",
         title = country_name) +
    theme(legend.position="none") + 
    scale_fill_manual(values=colors) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_date(date_labels = format("%b"), date_breaks = "month") 

cumulative.plot + 
  annotate(geom="text", x=as.Date("2016-01-10"), 
           y=(max(ggplot_build(cumulative.plot)$panel$ranges[[1]]$y.range)*.9), 
          label="2016", size=3, angle=90) +  
  geom_vline(aes(xintercept=as.numeric(as.Date("2016-01-01", origin="1970-01-01"))), linetype = "dashed") 
}
  

# To produce grid of plot for every country, script will run weekly.cases() and cumulative.cases() on data codes csv

# Run weekly.cases() on each country from data codes csv
weekly <- list()
for (i in 1:length(data_codes$file)){
  weekly[[i]]  <- weekly.cases(data_codes$file[i], data_codes$country_name[i], 
                  data_codes$confirmed_codes[i], data_codes$suspected_codes[i], 
                  data_codes$location_filter[i], data_codes$conf_cumulative_check[i], 
                  data_codes$susp_cumulative_check[i])
}

# Run cumulative.cases() on each country from data codes csv
cumulative <- list()
for (i in 1:length(data_codes$file)){
  cumulative[[i]]  <- cumulative.cases(data_codes$file[i], data_codes$country[i], 
                      data_codes$confirmed_codes[i], data_codes$suspected_codes[i], 
                      data_codes$location_filter[i], data_codes$conf_cumulative_check[i], 
                      data_codes$susp_cumulative_check[i])
}

# create plot from Argentina data (weekly[[1]]) to extract the legend to add that to each plot

plot.with.legend <- weekly[[1]] + 
  theme(legend.position="right") +
  scale_fill_discrete(breaks=c("confirmed_value", "suspected_value"),
                      labels=c("confirmed", "suspected")) + labs(fill=" ") 
# function to extract just the legend from plot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
} 

legend <- g_legend(plot.with.legend)
# change location of legend
legend$vp$x <- unit(.35, 'npc') 
legend$vp$y <- unit(.15, 'npc')

# plot weekly and cumulative cases with legend
do.call(grid.arrange, c(weekly, top="Number of suspected and confirmed cases of ZIKV"))
grid.draw(legend) # add legend

do.call(grid.arrange, c(cumulative,  top="Cumulative number of suspected and confirmed cases ZIKV"))
grid.draw(legend) # add legend

# TO save plots as PDFs ------------------------------------------------------

# Change working directory to location you want plots and run code below
# 
# pdf("New_cases.pdf", width=8, height=12) 
# do.call(grid.arrange, c(weekly, top="Number of suspected and confirmed cases of ZIKV"))
# grid.draw(legend)   # add legend
# dev.off()
# 
# pdf("Cumulative_cases.pdf", width=8, height=12)
# do.call(grid.arrange, c(cumulative,  top="Cumulative number of suspected and confirmed cases ZIKV"))
# grid.draw(legend)
# dev.off()



