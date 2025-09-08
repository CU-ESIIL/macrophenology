# Data Harmonization Processed iDigBio  
#Author: Liz Amador


# This script harmonizes processed iDigBio with NEON, USA-NPN, and other 
#herbarium flowering data. 

# Focal native/non-native species pairs are the following:
#   **Exotic** -- **Native**   
#   *Rubus laciniatus* -- *Rubus spectabilis*  
#   *Ilex aquifolium* -- *Ilex decidua*  
#   *Juglans regia* -- *Juglans nigra*  
#   *Acer platanoides* -- *Acer rubrum*


############
# Libraries 
############
#if statement to automatically install libraries if absent in r library
#tidyverse - mainly for data wrangling & plotting/mapping
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)


##############
# Import data 
##############
## iDigBio
idb = read.csv("Data/L1/FocalSpecies_OpenFlowers_PA_noNA_iDigBio_processed.csv", header = TRUE)
## NEON/NPN/Herbarium data 
phe = read.csv("Data/L1/FocalSpp_OpenFlowers_PA_NeonNpnHerb.csv", header = TRUE)


####################
# Wrangling iDigBio
####################
## Calculating DOY 
#set date column values as date class 
idb$idigbio.eventDate = as.Date(idb$idigbio.eventDate)
#split the datecollected field into three new columns and save in dataframe
idb[c("year_rect", "month", "day")] <- str_split_fixed(idb$idigbio.eventDate, '-', 3)


#Using `lubridate` to create function that applies leap year DOY values to all years.
# NEW Function to make every year account for leap year -- i.e. skip DOY value for 
# 02/29 (DOY == 60) for non-leap years; December 31 == 366
if (!requireNamespace("lubridate", quietly = TRUE)) { #START lubridate function
  install.packages("lubridate")
}
library(lubridate)

leap_every_year <- function(x) {
  ifelse(yday(x) > 59 & leap_year(x) == FALSE, yday(x) + 1, yday(x))
} #END lubridate function

#Run function for `eventDate`
idb$day_of_year= leap_every_year(idb$idigbio.eventDate)


# Extract lat and lon using `gsub`
idb = idb %>%
  separate(idigbio.geoPoint, into = c("latitude", "longitude"), sep = ",", remove = FALSE) %>%
  mutate(
    latitude = as.numeric(str_remove(latitude, '.*: ')), #Remove text before colon and convert to numeric
    longitude = (str_remove(longitude, '.*: ')), #Remove before colon 
    longitude = as.numeric(str_remove(longitude, '[^0-9.-]+$'))) #Remove }
    


#change column name to match main dataset (phe) field names 
idb.cleaned = idb %>%
  #renaming existing columns 
  rename(observation_id = coreid, observation_date = idigbio.eventDate,
         phenophase_status = flowers, species = gbif.canonicalName) %>%
  #Adding new columns - some with info that does not apply to iDigBio - but important to other data sources
  mutate(site_id = NA, state = NA, individual_id = NA, 
         phenophase_id = 501, phenophase_description = "Open flowers", 
         data_name = "iDigBio", DomainID = NA, record_id = NA, references = NA) %>%
  #Removing unnecessary columns 
  select(-c(time, month, day, idigbio.geoPoint))



################
# Merging data - comparing columns 
###############
#check that the structure of the columns are the same before merging! 
# str(idb.cleaned)
# str(phe)

#Set day of year column as an integer class 
idb.cleaned$year_rect = as.integer(idb.cleaned$year_rect)

#Distinct species info from phe -- so phe & phe.idb have equal column #s 
sp = phe %>%
  select(species, species_id, common_name) %>%
  distinct() #unique species
#joing the species information
idb.cleaned = left_join(idb.cleaned, sp, by = "species")


#Need to convert column into date else iDigBio dates converted into weird values
phe$observation_date = as.Date(phe$observation_date)


####################
# Wrangling iDigBio
####################
#Merge all data
phe.all = rbind(phe, idb.cleaned)

#export dataset - uncomment & adjust file pathway as necessary 
write.csv(phe.all, file = "Data/L2/FocalSpp_OpenFlowers_NeonNpnHerb_iDigBio.csv", row.names = FALSE)







