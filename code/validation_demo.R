################################################################################
# This script will load the validation script to validate the files you need 
# before pulling into the repo.
################################################################################

repo_dir <- "https://raw.githubusercontent.com/cdcepi/zika/master/"

source(paste0(repo_dir, "/code/validation.R"))

dguide <- read.csv(paste0(repo_dir, "/Dominican_Republic/DO_Data_Guide.csv"),
                   header = TRUE)
places <- read.csv(paste0(repo_dir, "/Dominican_Republic/DO_Places.csv"),
                   header = TRUE)

# validate a csv from the repo (data guide and places files match the data)
validate(read.csv(paste0(repo_dir,
    "/Dominican_Republic/Epidemiological_Bulletin/data/Epidemiological_Bulletin-2016-01-23.csv"),
    header = TRUE), dguide, places)

# validate a csv from the repo (data guide and places files from the wrong country)
validate(read.csv(paste0(repo_dir, 
    "/Mexico/DGE_Zika/data/DGE_Zika-2016-01-23.csv"), header = TRUE), dguide, places)



