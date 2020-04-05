###############################
# zillow_rental_scraper_func.R
###############################

# Purpose
########################################################################
# This function takes a processed list of Chicago neighborhoods as 
# parameter and scrapes rental listings from Zillow neighborhood pages
# based on the neighborhood list provided

# Considerations
########################################################################
# This function is set up specifically for Zillow and Chicago

# Parameters
########################################################################
# city_nm -> name of the city being scraped on Zillow
# state_nm -> name of the state being scraped on Zillow
# neighborhood_list -> A character vector of Zillow neighborhoods (Needs to be processed to drop right into Zillow URL strings)
# Note: all neighborhood names need to be connected with "-" since that's how the Zillow urls are set up


#**********************************************************************************************************************************


# Variables for testing
#neighborhood_list <- 'South-Loop'
#j <- 'South-Loop'
#city_nm <- 'Chicago'
#state_nm <- 'IL'


zillow_rental_scraper_func <- function(city_nm, state_nm, neighborhood_list){
  
  # Set up city and state names to specify webpage url
  city <- tolower(city_nm)
  state <- tolower(state_nm)
  
  # First initialize cumulating variables
  all_urls <- character()
  all_areas <- character()
  all_listings <- character()
  
  for(j in neighborhood_list){
    
    # Read in a base url to scrape from
    zillow_url_base <- paste0("https://www.zillow.com/",j,"-",city,"-",state,"/rentals/")
    
    # Next, determine number of listings in designated area
    # Zillow has xpath at top of page that shows number of listings in specified area
    # This will be useful in determing the number of webpages to loop through for scraping later
    zillow_page <- read_html(zillow_url_base)
    listing_cnt_selector <- '//*[contains(concat( " ", @class, " " ), concat( " ", "result-count", " " ))]'
    listing_cnt_nodes <- zillow_page %>% html_nodes(xpath=listing_cnt_selector)
    listing_cnt_txt <- html_text(listing_cnt_nodes)
    
    listing_cnt <- as.numeric(gsub('[A-z]|[[:punct:]]','',listing_cnt_txt))
    
    
    # Now, loop through the designated webpages and scrape available info from rental listings
    ######################################################################################################################
    
    # First initialize cumulating variables
    neighborhood_urls <- character()
    neighborhood_names <- character()
    neighborhood_listings <- character()
    incr_listing_cnt <- 0
    k <- 1
    
    # Use a while loop here. We will tally up all the listings in a single table
    # Loop will continue until the number of records tallied in the listing table is >= 
    # the number of listings in this specified area. (determined above)
    # Zillow's webpage interface only pulls up to 800 listings on a given search, regardless
    # of specification. (40 listings per page, 20 pages)  So the while loop will stop regardless
    # when listing table record count becomes > 800.  
    while(incr_listing_cnt < min(listing_cnt,800)){
      
      ## Read my example html with read_html()
      if(k==1){
        zillow_url <- zillow_url_base
      }else{
        zillow_url <- paste0(zillow_url_base,k,'_p/')
      }
      
      zillow_page = read_html(zillow_url)
      
      selector = '//*[contains(concat( " ", @class, " " ), concat( " ", "list-card-info", " " ))]'
      my_nodes = zillow_page %>% html_nodes(xpath=selector)
      
      listings <- html_text(my_nodes)
      
      #If no info is returned from webpage, webpage likely doesn't exist. Likely means no more listings with given specs
      if(length(listings)==0){   
        break
      }
      
      # Create vectors for tracking urls and neighborhoods
      url_list <- rep(zillow_url, length(listings))
      area_list <- rep(j, length(listings))
      
      # Add listings in current iteration to pervious tally
      neighborhood_urls <- c(neighborhood_urls, url_list)
      neighborhood_names <- c(neighborhood_names, area_list)
      neighborhood_listings <- c(neighborhood_listings, listings)
      
      # Iterate loop variables
      k <- k + 1
      incr_listing_cnt <- length(neighborhood_listings)
      
      # Build in system delay before looping to next page to avoid scraping detection
      Sys.sleep(20)
      
    }
    
    all_urls <- c(all_urls, neighborhood_urls)
    all_areas <- c(all_areas, neighborhood_names)
    all_listings <- c(all_listings, neighborhood_listings)
    
    # Build in system delay before looping to next page to avoid scraping detection
    Sys.sleep(20)
    
  }
  
  
  all_listings_tbl <- data.table('area'=all_areas, 'url'=all_urls, 'listing_info'=all_listings)
  
  
  return(all_listings_tbl)
  
}

