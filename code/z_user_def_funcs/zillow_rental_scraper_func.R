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
# neighborhood_proc - A character vector of Chicago neighborhoods (Needs to be processed to drop right into Zillow URL strings)
# Note: all neighborhood names need to be connected with "-" since that's how the Zillow urls are set up


#**********************************************************************************************************************************


zillow_rental_scraper_func <- function(neighborhood_proc){
  
  start_time <- Sys.time()
  
  # First initialize cumulating variables
  all_listings_table <- data.frame()
  for(j in neighborhoods_proc){
    
    # Read in a base url to scrape from
    #zillow_url_base <- "https://www.zillow.com/chicago-il/rentals/"
    zillow_url_base <- paste0("https://www.zillow.com/",j,"-chicago-il/rentals/")
    
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
    neighborhood_listings_table <- data.frame()
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
      listings_table <- cbind.data.frame('page'=zillow_url, 'area'=j,
                                         address, city, state, zip, home_typ, bedrooms, bathrooms, sqrft, rent)
      neighborhood_listings_table <- rbind.data.frame(neighborhood_listings_table, listings_table)
      
      # Iterate loop variables
      k <- k + 1
      incr_listing_cnt <- nrow(neighborhood_listings_table)
      
      # Build in system delay before looping to next page to avoid scraping detection
      Sys.sleep(10)
      
    }
    
    all_listings_table <- rbind.data.frame(all_listings_table, neighborhood_listings_table)
    
  }
  
  end_time <- Sys.time()
  print(end_time - start_time)
  
  return(all_listings_table)
  
}

