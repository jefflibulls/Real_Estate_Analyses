################################
# zillow_cities_extract.R
################################

# Purpose
###################################################################################
# This function extracts a list of US cities in Zillow database

# Considerations
###################################################################################
# This function takes in a specific Zillow
# neighborhood data file downloaded from 
# Zillow.com

# Paramters
###################################################################################
# filepath -> filepath where the zillow database file is located

# Output
###################################################################################
# A list of Zillow US cities on Zillow.com


#*******************************************************************************************************************************
  

# Variables for testing
# filepath <- zillow_neighborhoods_filepath


zillow_cities_extract <- function(filepath){
  
  
  # First load the file that contains all cities and neighborhoods info
  zillow_rental_database <- fread(filepath)
  
  # aggregate data by state and cities
  zillow_rental_database_agg <- zillow_rental_database[, .(listing_cnt=sum(ZriRecordCnt)), by=.(State, City, Metro)]
  zillow_rental_database_agg <- zillow_rental_database_agg[order(-listing_cnt),]
  
  
  return(zillow_rental_database_agg[,-('listing_cnt')])
  
}



#zillow_cities_extract(filepath)
