# CONSIDERATION
##########################################################################################3
# Challenges with Zillow scraping:
#   -> Each Zillow search will be limited to 800 listings returned, (40 listings per page, 20 pages) regardless of acual listings.
#   -> Necessary to loop through smaller area searches so actual listings of search <= 800.
#   -> However, the amount of looping it takes to capture Chicago listings puts stress on Zillow server. (Lots of server calls)
#   -> If runs done in a short time, Zillow will catch this and lock IP temporarily.
#   -> Solution:
#   -> 1) When looping through different pages of a neighborhood, put in a system delay that potentially masks the fact I'm scraping
#   -> 2) Limit number of neighborhoods to loop through each day.
#
#   -> Following the above strategy, we need a way to keep track of neighborhoods already scraped in order to move through
#   -> all neighborhood.  See following steps:
#   -> 1) Save all old scrapes to single directory
#   -> 2) With every new scrape, pull up old scraped files and extract neighborhoods previously scraped.
#   -> 3) Then extract neighborhoods still left over from full list
#   -> 4) New scrape will only choose neighborhoods from left over list
#   -> 5) Process continues until all neighborhoods are scraped.
#   -> 6) At next refresh, remove all old scrapes in directory and start over


#***********************************************************************************************************************************


## First, load required packages (or install if they're not already) 
pkgs = c("rvest", "magrittr", "httr", "stringr", "data.table", "dplyr") 
for (pkg in pkgs){ 
  if (!require(pkg, character.only = T)){ 
    install.packages(pkg) 
    library(pkg) 
  } 
}

# Chicago neighborhoods
# Better to specify search by neighborhoods than city
# in order to ensure all listings are returned
zillow_rental_database <- fread('data/misc/Neighborhood_Zri_AllHomesPlusMultifamily_Summary.csv')
zillow_rental_database <- zillow_rental_database[City=='Chicago',]
zillow_rental_database %<>% arrange(desc(ZriRecordCnt))
neighborhoods <- zillow_rental_database$RegionName
#neighborhoods <- c('Logan Square','River North')

# Format neighborhood names to drop into Zillow urls
neighborhoods_proc <- gsub('\\s','-',neighborhoods)


# Solution to determine what neighborhoods to run
#########################################################################################
# Premise is that with each scrape, resulting file is saved in zillow/data/raw/
# with scrape date as post-fix of file name. Then we can manage neighborhood list
# based on neighborhoods already existing in all the files in zillow/data/raw/

# first find all files in raw folder
raw_file_list <- list.files('data/zillow/raw')

# Then load all files in raw folder
full_listings_table <- data.frame()
for(file_nm in raw_file_list){
  
  listings_data <- fread(paste0('data/zillow/raw/',file_nm))
  full_listings_table <- rbind.data.frame(full_listings_table, listings_data)
  
}

# Return all neighborhoods in all the files in raw folder
areas_scraped <- unique(full_listings_table$area)

# Finally, from full list, return all neighborhoods not in the existing files
neighborhoods_proc <- setdiff(neighborhoods_proc, areas_scraped)


# Actual scraping from Zillow
###########################################################################################
# Scrape Zillow rental listings from specified Chicago neighborhoods
all_listings_table <- zillow_rental_scraper_func(neighborhoods_proc[1:5])


# Outputing scraped file
###########################################################################################
date <- gsub('-','_',as.character(Sys.Date()))
write.csv(all_listings_table, paste0('data/zillow/raw/zillow_listings_table_',date,'.csv'), row.names=F)



# Data configuring
###########################################################################################

avg_table <- all_listings_table %>% group_by(zip, bedrooms, bathrooms) %>% summarise(sample_size=n(),
                                                                                     rent_avg=mean(rent, na.rm=T),
                                                                                     rent_med=median(rent, na.rm=T),
                                                                                     rent_25=quantile(rent,0.25, na.rm=T),
                                                                                     rent_75=quantile(rent,0.75, na.rm=T)) %>%
  arrange(desc(sample_size))


write.csv(avg_table, 'data/zillow/cleaned/avg_rent_by_zip.csv', row.names=F)

