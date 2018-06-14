# run this script from the base zika directory
# the working directory should be the `zika` folder, not the `code` folder
# 
# Run it in your command prompt: `Rscript code/validate_stacked_data.R`


# tries to stack all the csv data, if it can't there is probably a problem somewhere
# this bit of code is run in the dashboard which can be found here:
# https://github.com/chendaniely/zika_dashboard_cdc

library(readr)
library(stringr)

# if code is run from a subdirectory:
wd <- getwd()
split_dir <- str_split(string = getwd(), pattern = '/')[[1]]
if (split_dir[length(split_dir)] == 'code') {
    print(sprintf('current directory: %s', wd))
    setwd('..')
    wd <- getwd()
    print(sprintf('changed to: %s', wd))
}

files <- list.files(path = '.',
                    pattern = '[0-9]{4}-[0-9]{2}-[0-9]{2}.csv$',
                    recursive = TRUE,
                    full.names = TRUE)

tables <- lapply(files, readr::read_csv)

# find the files that are throwing errors
# file.check <- data.frame(files = files, col.nums = do.call(rbind,lapply(files, function(x) ncol(read.csv(x)))))

#get columns of interest
variables <- c("report_date", "location", "location_type", "data_field", "data_field_code", 
               "time_period", "time_period_type", "value", "unit")

#combine all of the files, selecting only the relevant columns
combined_df <- do.call(rbind , lapply(tables, subset, select = variables))

print("YAY everything stacked!")
