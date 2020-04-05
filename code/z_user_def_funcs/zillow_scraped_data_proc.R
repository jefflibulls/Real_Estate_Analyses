###############################
# zillow_scraped_data_proc.R
###############################

# Purpose
########################################################################
# This function takes a raw Zillow scraped RDS dataset and does 
# necessary processing to it.

# Considerations
########################################################################
# This function is set up specifically for Zillow scraped data
# Output from 'zillow_rental_scraper_func_v2.R'
#
# The scraper(function referenced above) returns listing info but 
# all in a text string. This function cleans that up and parses out
# useful info.
#
# Idea is that the data cleaning happening here can be run, tested and
# improved separately from webscraping. This way, when we rerun the 
# code repeatedly, we're not in danger of getting locked out of 
# the Zillow server.

# Parameters
########################################################################
# filepath -> filepath for the raw Zillow scraped RDS dataset


#**********************************************************************************************************************************


# Variables for testing
#filepath <- paste0(zillow_scraped_filepath,'/curr_scrape_output_raw.rds')


zillow_scraped_data_proc <- function(filepath){
  
  listings_tbl <- readRDS(filepath)
  
  
  areas_list <- listings_tbl[,area]
  url_list <- listings_tbl[,url]
  listings <- listings_tbl[,listing_info]
  
  
  #scraped listing text comes in a single string.  Use regex to split string up into useful components.  
  #Consider pull string split work out into separate function
  
  listings_split <- strsplit(listings, '\\$')
  
  
  listings_pt_1 <- as.character()
  listings_pt_2 <- as.character()
  for(i in c(1:length(listings))){
    listings_pt_1 <- c(listings_pt_1, listings_split[[i]][1])
    listings_pt_2 <- c(listings_pt_2, listings_split[[i]][2])
  }
  
  listings_pt_1 <- strsplit(listings_pt_1, ', ')
  
  address <- as.character()
  city <- as.character()
  st_zip_typ <- as.character()
  for(i in c(1:length(listings))){
    address <- c(address, listings_pt_1[[i]][1])
    city <- c(city, listings_pt_1[[i]][2])
    st_zip_typ <- c(st_zip_typ, listings_pt_1[[i]][3])
  }
  
  st_zip_typ <- strsplit(st_zip_typ, ' ')
  
  state <- as.character()
  zip_typ <- as.character()
  for(i in c(1:length(listings))){
    state <- c(state, st_zip_typ[[i]][1])
    zip_typ <- c(zip_typ, st_zip_typ[[i]][2])
  }
  
  zip <- as.numeric(str_extract(zip_typ, '[0-9]+'))
  home_typ <- str_extract(zip_typ, '[A-z]+')
  
  #read from begning of string until '/' is reached.
  #(.*?) means any characters
  rent <- str_extract(listings_pt_2, '^(.*?)/') 
  #when there is a ',' or '/' character, replace to ''.  
  rent <- as.numeric(gsub(',|/', '', rent))
  
  
  #read from part of string beginning with 'mo', until there is empty space '\\s'
  bedrooms <- str_extract(listings_pt_2, 'mo(.*?)\\s')
  #remove any letters from resulting string, leaving only the number of bedrooms
  bedrooms <- as.numeric(gsub('[A-z]','',bedrooms))
  
  #read from part of string beginning with 'bd' or 'bds, until there is empty space '\\s'
  bathrooms <- str_extract(listings_pt_2, 'bds(.*?)\\s | bd(.*?)\\s')
  #remove any letters from resulting string, leaving only the number of bathrooms
  bathrooms <- as.numeric(gsub('[A-z]','',bathrooms))
  
  sqrft <- str_extract(listings_pt_2, 'ba(.*?)\\s')
  sqrft <- as.numeric(gsub('[A-z]|[[:punct:]]','',sqrft))
  
  
  # Create listing table and add to pervious tally
  listings_table <- cbind.data.frame('webpage'=url_list, 'area'=areas_list,
                                     address, city, state, zip, home_typ, bedrooms, bathrooms, sqrft, rent)
  
  return(listings_table)
  
}
