################################
# curr_areas_scraped_extract.R
################################

# Purpose
###################################################################################
# This function extracts a list of a city's neighborhoods that have already
# been recently extracted.

# Considerations
###################################################################################
# This is a support function for a Zillow webscraping program. The program scrapes
# rental listings from Zillow.com. However, there is limit to the number of listings 
# a scrape can capture. Thus, it is necessary to run scrape by neighborhood, instead 
# of by a whole city, in order to capture listings more comprehensively.
#
# In addition, if the program loops through all the neighborhoods too quickly, 
# Zillow server overloads or recognizes that it is being scraped and locks out the
# IP running the scrape for a period of time. To avoid this issue, program breaks
# the entire scrape of a city's listings into multiple scrapes by areas. As part of
# that framework, program needs to keep track of what areas have been recently 
# scraped.
#
# This function reads in all the scraped files from a specific folder and returns
# all neighborhoods present in the scraped files

# Parameters
###################################################################################
# filepath -> the filepath where the scraped output files are located

# Output
###################################################################################
# A list of Zillow neighborhoods for a specified city that are available in the
# existing scraped output files.


#*******************************************************************************************************************************


# variable for testing
# filepath <- 'data/zillow/raw'


curr_areas_scraped_extract <- function(filepath){
  
  # first find all files in raw folder
  raw_file_list <- list.files(filepath)
  
  # Keep only the .CSV files
  raw_file_list <- grep('.csv$', raw_file_list, ignore.case=T, value=T)
  
  # Then load all files in raw folder
  full_listings_table <- data.frame()
  for(file_nm in raw_file_list){
    
    listings_data <- fread(paste0(filepath,'/',file_nm))
    full_listings_table <- rbind.data.frame(full_listings_table, listings_data)
    
  }
  
  # Return all neighborhoods in all the files in raw folder
  areas_scraped <- unique(full_listings_table$area)
  
  
  return(areas_scraped)
  
}


#curr_areas_scraped_extract('data/zillow/raw')
