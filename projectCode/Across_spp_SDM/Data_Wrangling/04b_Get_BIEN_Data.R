
#Get BIEN data 
#Author: Lizbeth G Amador 


#This script pulls BIEN occurrence records from the rbien package  


#######################
# Load necessary libraries
#######################
#if statement to automatically install libraries if absent in r library
#tidyverse - mainly for data wrangling & plotting/mapping
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

#terra - spatial package 
if (!requireNamespace("terra", quietly = TRUE)) {
  install.packages("terra")
}
require(terra)

#BIEN - to download species range maps and data 
if (!requireNamespace("BIEN", quietly = TRUE)) {
  install.packages("BIEN")
}
library(BIEN)


#Source file directories
source(file.path(getwd(), "File_Directories.R"))
#conus 
conus = vect(file.path(L2, "conus.shp"))
#read species list 
species_list = read.csv(file = file.path(L2, "Focal_Species_List.csv"), 
                        header = TRUE)
#vector of focal species 
species = species_list$species



#######################
# 1. Download data
#######################
#Lists all available types of trait -- dont have flowering 
# traitslist = BIEN_trait_list() 


#Returns all occurrence records for a specified species
bien = BIEN_occurrence_species(species = species, 
                               native.status = TRUE, #provides native status 
                               observation.type = TRUE) 


#######################
# 2. Make sure the data points fall within the CONUS extent.
#turn into a point vector 
#######################
bien.sh = vect(bien, geom = c("longitude", "latitude"), keep = TRUE, crs = crs(conus))
#plot
plot(conus, main = "All Occurence Observations"); plot(bien.sh, add=TRUE)

#drop any points outside of conus shapefile 
bien.int = intersect(bien.sh, conus)
plot(conus, main="All Occurrence Obs: Post intersect"); plot(bien.int, add=TRUE)

#revert back to a dataframe 
bien.conus = as.data.frame(bien.int)

# Lets see where these data are from: 
#see unique instances of data sources and such - unccoment to see
bien.datsource = bien.conus %>%
  select(datasource, dataset, dataowner, datasource_id) %>%
  distinct()
# This BIEN data includes observations from GBIF (and iNaturalist), and other consortiums!


#######################
# 3. We will need the columns that match with `phe.range`columns ("latitude", 
# "longitude", "year_rect", "species", "data_name"). This will 
# facilitate data merging.  
#######################
#Pull year from date 
bien.conus$year_rect = format(as.Date(bien.conus$date_collected, format="%Y-%m-%d"),"%Y")

#Select columns & rename 
bien.sp = bien.conus %>%
  rename(species = scrubbed_species_binomial, data_name = datasource) %>%
  # mutate(native_status = "native") %>% 
  select(latitude, longitude, year_rect, species, data_name)


#Remove rows with NAs in year_rect column 
bien.sp = bien.sp[!is.na(bien.sp$year_rect),]
sum(is.na(bien.sp$year_rect))

#######################
#Save dataframe 
#######################
write.csv(bien.sp, file = file.path(L1, "BIEN_focal_species_occurrence_conus.csv"), row.names = FALSE)

