

#Set dir within which all the work will be done
setwd('~/GitHub/Retirement_Planning')


sp_500_filepath <- "data/sp_500_returns.csv"


# Inputs
#######################################################################################################

# initial nest egg value @ first review ###
init_date <- '11/25/2019'
init_principle <- 158409 #includes both 401K and IRA values

# age related informaiton ####
birth_date <- '10/01/1984'
target_retirement_age <- 60
life_expectancy <- 90

# current income and retirement contribution info ###
curr_salary <- 124400
contrib_401k_perc <- 0.05
company_401k_match_perc <- 0.04
annual_ira_contribution <- 6000
annual_contribution_increase_perc <- 0.02 #expected inflation

# target income in retirement ###
target_retirement_income_lvl <- 80000
annual_withdraw_increase_perc <- 0.02 #expected inflation

# market rate of return on nest egg assumptions
assumed_avg_return_pre_retirement <- 0.07
assumed_avg_return_post_retirement <- 0.03


# Actual results tracking
##########################
date <- c('01/01/2020')
accum_amt <- c(170000)
actual_table <- cbind.data.frame(date, accum_amt)
actual_table$date <- as.Date(actual_table$date, '%m/%d/%Y')