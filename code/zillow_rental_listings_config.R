

# Data set creation
##########################################################################################

# first find all files in raw folder
raw_file_list <- list.files('data/zillow/raw')

# Then load all files in raw folder
full_listings_table <- data.frame()
for(file_nm in raw_file_list){
  
  listings_data <- fread(paste0('data/zillow/raw/',file_nm))
  full_listings_table <- rbind.data.frame(full_listings_table, listings_data)
  
}


# Data configuring
###########################################################################################

avg_table_zip_bed_bath <- all_listings_table %>% mutate(bedrooms=as.integer(round(bedrooms)),
                                                        bathrooms=as.integer(round(bathrooms))) %>% 
  group_by(zip, bedrooms, bathrooms) %>% summarise(sample_size=n(),
                                                   rent_avg=mean(rent, na.rm=T),
                                                   rent_med=median(rent, na.rm=T),
                                                   rent_25=quantile(rent,0.25, na.rm=T),
                                                   rent_75=quantile(rent,0.75, na.rm=T)) %>%
  filter(!(is.na(zip))) %>%
  arrange(desc(sample_size)) 


write.csv(avg_table_zip_bed_bath, 'data/zillow/cleaned/avg_rent_by_zip_bed_bath.csv', row.names=F)

