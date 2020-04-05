

# Turn off scientific notation
options(scipen=999)


## First, load required packages (or install if they're not already) 
pkgs = c("rvest", 
         "magrittr", 
         "httr", 
         "stringr", 
         "data.table", 
         "dplyr", 
         "FinancialMath", 
         "openxlsx",
         "ggplot2",
         "lubridate") 

for (pkg in pkgs){ 
  if (!require(pkg, character.only = T)){ 
    install.packages(pkg) 
    library(pkg) 
  } 
}