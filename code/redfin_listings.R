#####################
# redfin_listings.R
#####################

# PURPOSE
###############################################################
# This code file downloads Redfin lists and estimates 
# key real estate metrics


#***********************************************************************************************************************************


## First, load required packages (or install if they're not already) 
pkgs = c("rvest", "magrittr", "httr", "stringr", "data.table", "dplyr", "FinancialMath", "openxlsx") 
for (pkg in pkgs){ 
  if (!require(pkg, character.only = T)){ 
    install.packages(pkg) 
    library(pkg) 
  } 
}


#Set dir from which to read in file
setwd("~/Real_Estate/deal_analyses")


# Downlaod Redfin Listings Data
################################################################################################
# Redfin search pages includes link at the bottom of each page to download listings
# in table format. It has a lot of nice details there. Challenge is that the link only downloads 
# ~350 listings to table, regardless the number of listings in the search. So it's incomplete
# to download listings from the city search page. (i.e. Chicago)  Workaround here is to 
# collect Redfin's listing search pages by neighborhood, download individuallys, then combine
# together to get all listings at city level.

#ASSUMPTION - all neighborhood listing counts are <350 as well. Need to build in check for this.


# First pull Chicago neighborhoods from pre-set workbook
chi_neighborhoods <- loadWorkbook('data/misc/chicago_neighborhoods.xlsx')
neighborhoods <- readWorkbook(chi_neighborhoods, sheet='neighborhoods')

neighborhoods %<>% filter(!(is.na(Redfin_URL)))

redfin_urls <- neighborhoods$Redfin_URL

# Use rvest to establish the download link from Redfin
cnt <- 1
for(url in redfin_urls){
  
  page = read_html(url)  # read in webpage
  selector = '//*[(@id = "download-and-save")]'  # use SelectorGadget to determine xpath of link we're targeting
  links = page %>% html_nodes(xpath=selector) %>% html_attr("href")  # returns download link address
  full_link = paste0('https://www.redfin.com', links)  # Create complete download link url
  
  # Based on obtained download link url, download listings table
  download.file(url=full_link, destfile = paste0("data/redfin/raw/redfin_listings_",cnt,".csv"))
  
  cnt <- cnt + 1
  
}


# Analyze Redfin Listings Data
################################################################################################
# Aggregate the downloaded Redfin listings into one table and perform analyses
# on key metrics

# First aggregate the downloaded listing files
list_file_cnt <- length(list.files('data/redfin/raw'))

redfin_listings_table <- data.frame()
for(i in c(1:list_file_cnt)){
  
  data <- fread(paste0('data/redfin/raw/redfin_listings_',i,'.csv'), check.names=T)
  
  if(ncol(data) > 6){
    redfin_listings_table <- rbind.data.frame(redfin_listings_table, data) 
  }
  
}


# Then calculate key metrics for listings
#Inputs
down_pmt_perc <- 0.2
est_prop_tax_rate <- 0.021
est_insurance_rate <- 0.0049


redfin_proc_data <- redfin_listings_table %>% mutate(exp_down_pmt = ifelse(is.na(PRICE),0,round(PRICE*down_pmt_perc)),
                                               loan_amt = ifelse(is.na(PRICE),0,PRICE - exp_down_pmt))

mthly_mortgage <- apply(redfin_proc_data[,'loan_amt',drop=F], MARGIN=1, FUN=function(x){
  
                          if(x>0){
                            round(amort.period(Loan=x, n=360, i=0.0362, ic=12, pf=12)['PMT',])
                          }else{
                            return(0)
                          }
  
                        }
                        )

redfin_proc_data <- cbind.data.frame(redfin_proc_data, mthly_mortgage)

redfin_proc_data %<>% mutate(HOA.MONTH=ifelse(is.na(HOA.MONTH),0,HOA.MONTH),
                             mthly_tax_est = round(PRICE * est_prop_tax_rate / 12),
                             mthly_insurance_est = round(PRICE * est_insurance_rate / 12),
                             mthly_pmt_tot = mthly_mortgage + mthly_tax_est + mthly_insurance_est + HOA.MONTH)


#Tax rate doesn't seem very accurate


avg_rent_by_zip_table <- fread('data/zillow/cleaned/avg_rent_by_zip_bed_bath.csv')

redfin_proc_data %<>% mutate(BEDS_proc=as.integer(round(BEDS)), BATHS_proc=as.integer(round(BATHS)))

deal_analyses_table <- merge(redfin_proc_data, avg_rent_by_zip_table, by.x=c('ZIP.OR.POSTAL.CODE','BEDS_proc','BATHS_proc'),
                             by.y=c('zip','bedrooms','bathrooms'), all.x=T)

deal_analyses_table %<>% mutate(cashflow_avg = rent_avg - mthly_pmt_tot,
                                cashflow_med = rent_med - mthly_pmt_tot,
                                cashflow_25 = rent_25 - mthly_pmt_tot,
                                cashflow_75 = rent_75 - mthly_pmt_tot) %>% arrange(desc(cashflow_25))

write.csv(deal_analyses_table, 'data/redfin/cleaned/deal_analyses.csv', row.names = F)


# This table checks for reasons why no rent estimate is found.  A way to improve the deal_analyses_table
test <- filter(deal_analyses_table, is.na(rent_med))
