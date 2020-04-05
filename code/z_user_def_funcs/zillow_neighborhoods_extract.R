################################
# zillow_neighborhood_extract.R
################################

# Purpose
###################################################################################
# This function extracts a list of a city's neighborhoods.

# Considerations
###################################################################################
# This is a support function for a Zillow webscraping program. The program scrapes
# rental listings from Zillow.com. However, there is limit to the number of listings 
# a scrape can capture. Thus, it is necessary to run scrape by neighborhood, instead 
# of by a whole city, in order to capture listings more comprehensively.
#
# This function takes in a specific Zillow
# neighborhood data file downloaded from 
# Zillow.com

# Parameters
###################################################################################
# filepath -> filepath where the zillow database file is located
# city_nm -> name of the city being reviewed
# state_nm -> name of the state where the city is located

# Output
###################################################################################
# A list of Zillow neighborhoods for specified city, state.


#*******************************************************************************************************************************
  

# Variables for testing
#filepath <- zillow_neighborhoods_filepath
#city_nm <- 'Chicago'
#state_nm <- 'IL'


zillow_neighborhood_extract <- function(filepath, city_nm, state_nm){
  
  
  # First load the file that contains all cities and neighborhoods info
  zillow_rental_database <- fread(filepath)
  
  # filter loaded data based on city and state specs
  zillow_rental_database <- zillow_rental_database[City==city_nm & State==state_nm,]
  
  # Arrange filtered data by total record counts by neighborhood.
  # Goal of neighborhood list is to scrape info from Zillow by neighborhood.
  # So starting from most popular neighborhoods is probably good.
  zillow_rental_database %<>% arrange(desc(ZriRecordCnt))
  
  # Extract neighborhoods from filtered data
  neighborhoods <- unique(zillow_rental_database$RegionName)
  
  # Format neighborhood names to drop into Zillow urls
  neighborhoods_proc <- gsub('\\s','-',neighborhoods)
  
  
  return(neighborhoods_proc)
  
}


#neighborhood_names_extract(zillow_neighborhoods_filepath, 'Chicago', 'IL')
