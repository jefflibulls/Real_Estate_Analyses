

#Set dir within which all the work will be done
setwd('~/GitHub/Real_Estate_Analyses')


# Inputs
#######################################################################################################

input_state <- 'IL'
input_city <- 'Chicago'


# Data Inputs
#######################################################################################################

chi_neighborhoods <- loadWorkbook('data/misc/chicago_neighborhoods.xlsx')

zillow_neighborhoods_filepath <- paste0('data/misc/Neighborhood_Zri_AllHomesPlusMultifamily_Summary.csv')

zillow_scraped_filepath <- 'data/zillow/raw'
