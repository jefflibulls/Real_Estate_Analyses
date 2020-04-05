###############################
# zillow_rental_scraper.R
###############################

# Purpose
########################################################################
# This code works through the workflow of scraping rental listings
# from Zillow.

# CONSIDERATION
##########################################################################################3
# Challenges with Zillow scraping:
#   -> Each Zillow search will be limited to 800 listings returned, (40 listings per page, 20 pages) regardless of actual listings.
#   -> Necessary to loop through smaller area searches so actual listings of search <= 800.
#   -> However, the amount of looping it takes to capture Chicago listings puts stress on Zillow server. (Lots of server calls)
#   -> If runs done in a short time, Zillow will catch this and lock IP temporarily.
#   -> Solution:
#   -> 1) When looping through different pages of a neighborhood, put in a system delay that potentially masks the scraping calls.
#   -> 2) Limit number of neighborhoods to loop through each day.
#
#   -> Following the above strategy, we need a way to keep track of neighborhoods already scraped in order to move through
#   -> all neighborhoods.  See following steps:
#   -> 1) Save all old scrapes to single directory
#   -> 2) With every new scrape, pull up old scraped files and extract neighborhoods previously scraped.
#   -> 3) Then extract neighborhoods still left over from full list
#   -> 4) New scrape will only choose neighborhoods from left over list
#   -> 5) Process continues until all neighborhoods are scraped.
#   -> 6) At next refresh, remove all old scrapes in directory and start over


#***********************************************************************************************************************************


# Load source files
source('code/1_libraries_load.R')
source('code/2_user_def_funcs_load.R')
source('code/3_initial_inputs.R')


# City neighborhoods
# Better to specify search by neighborhoods than city
# in order to ensure all listings are returned
neighborhoods_proc <- zillow_neighborhood_extract(zillow_neighborhoods_filepath, input_city, input_state)


# Solution to determine what neighborhoods to run
#########################################################################################
# Premise is that with each scrape, resulting file is saved in zillow/data/raw/
# with scrape date as post-fix of file name. Then we can manage neighborhood list
# based on neighborhoods already existing in all the files in zillow/data/raw/

# Return all neighborhoods in all the files in scraped output folder
areas_scraped <- curr_areas_scraped_extract(zillow_scraped_filepath)

# Finally, from full list, return all neighborhoods not in the existing files
neighborhoods_proc <- setdiff(neighborhoods_proc, areas_scraped)


if(length(neighborhoods_proc) > 0){
  
  # Actual scraping from Zillow
  ###########################################################################################
  # Scrape Zillow rental listings from specified Chicago neighborhoods
  # all_listings_table <- zillow_rental_scraper_func(input_city, input_state, neighborhoods_proc[1:5])
  
  start_time <- Sys.time()
  all_listings <- zillow_rental_scraper_func(input_city, input_state, neighborhoods_proc[1:5])
  end_time <- Sys.time()
  print(end_time - start_time)
  
  output_filepath <- paste0(zillow_scraped_filepath,'/curr_scrape_output_raw.rds')
  saveRDS(all_listings, file=output_filepath)
  
  
  # Cleaning of the scraped file from Zillow
  ###########################################################################################
  all_listings_table <- zillow_scraped_data_proc(output_filepath)
  
  
  # Outputing scraped file
  ###########################################################################################
  date <- gsub('-','_',as.character(Sys.Date()))
  write.csv(all_listings_table, paste0('data/zillow/raw/zillow_listings_table_',date,'.csv'), row.names=F)
  
}else{
  
  print(paste0('Files in ',
               zillow_scraped_filepath,
               '/ already reflect all Zillow neighborhoods in ',
               input_city,', ',input_state,'.'))
  print('Remove existing files before re-scraping the latest Zillow listings.')
  
}





