#install.packages("ggmap")
library(tidyverse)
library(ggmap)
library(devtools)
library(emo)
library(tidycensus) 
library(dplyr)
library(FinCal)
library(viridis)
library(leaflet)
library(stringr)
library(sf)

census_api_key("f8a54a811f353f5a6d013e2bc70e91d2d0fb1a98", install = TRUE)


#pulling in the mls data

#mlsdata<-read_csv("Data/myRealEstateData.csv") %>% filter(state==c("NJ")) %>% select(-c(description))
# mlsdata_subset<-mlsdata %>% filter(state==c("NY", "CA", "TX", "MD"))
# write_csv(mlsdata_subset, "mlsdatasubset.csv")
# str(mlsdata)

#mlsdata<-read_csv("Data/mlsdatasubset.csv") %>% select(-c(description))
# census_clean_data_2017_bed<-read_csv("Data/census_clean_data_2017_bed.csv") 
# census_clean_data_2017_stat<-read_csv("Data/census_clean_data_2017_stat.csv") 

#summary_census_2017 <- load_variables(2017, "acs1", cache = TRUE) # acs1 survey performed annually
#this is 2012-2016 data 5 year ACS
census_clean_data_2017_bed <- get_acs(geography = "zip code tabulation area",
                                  variables = c("1" = "B25031_003", "2" = "B25031_004", "3" = "B25031_005", "4" = "B25031_006", "5" = "B25031_007"),
                                 geometry = TRUE)
# #write_csv(census_clean_data_2017_bed, "census_clean_data_2017_bed.csv")
# 
census_clean_data_2017_stat <- get_acs(geography = "zip code tabulation area",
                                      variables = c("tot_pop"="B01003_001", "misc_exp"="B25088_003", "vac_dat"="B25004_001"),
                                      geometry = TRUE)

# write_csv(census_clean_data_2017_stat, "census_clean_data_2017_stat.csv")

##pulling mls data
# mlsdata<-read_csv("Data/myRealEstateData.csv") 
# mlsdata %>% group_by(state) %>% summarize(ncount=n()) %>% arrange(., desc(ncount))

mlsdata<-read_csv("Data/myRealEstateData.csv") %>% filter(state==c("NY")) %>% select(-c(description))

## breaks out variables column to match by zip code etc.

census_clean_data_2017_statsp<-census_clean_data_2017_stat %>% #creates column for total population
  spread(variable, estimate) %>% 
  rename(postal_code=GEOID) %>% 
  as.tibble()

# to join mls data with census data 
census_mls_data<-census_clean_data_2017_bed %>%  
  rename(postal_code=GEOID) %>% 
  rename(beds=variable) %>% 
  mutate(beds=as.numeric(beds)) %>% 
  inner_join(mlsdata, by=c("postal_code", "beds")) %>% 
  #filter(state=="NY") %>% 
  filter(price>10000) %>% #remove the incorrect data - rent labeled as price
  mutate(annualized_income=estimate*12) %>% 
  mutate(price_rent_ratio=price/annualized_income) 


 # census_mls_data_inc<- census_mls_data %>%
 #  #filter(postal_code=="06095") %>% 
 #   rowwise() %>% 
 #  do ({
 #    result = as_data_frame(.)
 #    result$annulized_exp = pmt(r=0.05/12, n=360, pv=result$price, fv=0, type=0)*-12
 #    result
 #  }) %>% 
 #   mutate(est_profit=annualized_income-annulized_exp) %>% 
 #   arrange(est_profit)
 
#for example let us look at new york
 census_mls_data_inc<-census_mls_data %>%
   filter()
  mutate(annulized_exp=(pmt(r=0.05/12, n=360, pv=census_mls_data$price, fv=0, type=0)*-12)) - hoa_per_month)%>% 
   mutate(est_profit=annualized_income-annulized_exp) %>% 
   arrange(est_profit) %>% 
   inner_join(census_clean_data_2017_statsp,by=c("postal_code")) # number of years to pay of the house without loans or interest etc.
 
 # count_by_city<-census_mls_data_inc %>% select(city) %>% group_by(city) %>% summarize(ncount=n())
ny_pop <- census_mls_data_inc %>% filter(state=="NY") %>% rename(NAME=NAME.x)

nyc_pop <- ny_pop  %>% filter(city==c("New York", "Manhattan", "New York, Ny")) %>% filter(location != "New York")
pal <- colorQuantile(palette = "viridis", domain = nyc_pop$tot_pop, n=10)
 


 
 # nyc_pop %>%
 #  st_transform(crs = "+init=epsg:4269") %>%
 #   leaflet(width = "100%") %>%
 #   addProviderTiles(provider = "CartoDB.Positron") %>%
 #   addPolygons(popup = ~ str_extract(location, "^([^,]*)"),
 #               stroke = FALSE,
 #               smoothFactor = 0,
 #               fillOpacity = 0.7,
 #               color = ~ pal(tot_pop)) %>%
 #   addLegend("bottomright", 
 #             pal = pal, 
 #             values = ~ tot_pop,
 #             title = "Population percentiles",
 #             opacity = 1)

 # estimate is based on zip code too much variance in prices across the zip code and that is why this is not a good estimate.

 #where are the most expensive houses - what are the most expensive zipcodes?
 #what is variance in a single zip code?
 #break down of active by neighborhood
 #vacancy rates 
 #median price to rent by zip code
 
x<-unique(ny_pop$city)

