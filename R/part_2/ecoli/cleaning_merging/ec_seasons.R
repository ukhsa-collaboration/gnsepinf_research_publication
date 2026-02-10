## I am just going to do the seasonal coding here
# Load necessary libraries
library(dplyr)
library(lubridate)

source("paths/import_datasets.R")

# Function to determine the season based on equinox and solstice dates for each year
get_season <- function(date) {
  date <- as.Date(date)
  year <- year(date)
  spring_equinox <- as.Date(paste0(year, '-03-20'))
  summer_solstice <- as.Date(paste0(year, '-06-21'))
  autumn_equinox <- as.Date(paste0(year, '-09-22'))
  winter_solstice <- as.Date(paste0(year, '-12-21'))
  
  if (date >= spring_equinox & date < summer_solstice) {
    return('Spring')
  } else if (date >= summer_solstice & date < autumn_equinox) {
    return('Summer')
  } else if (date >= autumn_equinox & date < winter_solstice) {
    return('Autumn')
  } else {
    return('Winter')
  }
}



# Apply the function to create a new column 'season'
ec_final$season <- sapply(ec_final$data_collection_date, get_season)


tabyl(ec_final$season)
  



