# Data Harmonization 
#Author: Liz Amador


# This script harmonizes unprocessed iDigBio with NEON, USA-NPN, and other 
#herbarium flowering data. 

# Focal native/non-native species pairs are the following:
#   **Exotic** -- **Native**   
#   *Rubus laciniatus* -- *Rubus spectabilis*  
#   *Hymenocallis littoralis* -- *Hymenocallis occidentalis*  
#   *Ilex aquifolium* -- *Ilex decidua*  
#   *Juglans regia* -- *Juglans nigra*  
#   *Acer palmatum* -- *Acer rubrum*
  

############
# Libraries 
############
#if statement to automatically install libraries if absent in r library
#tidyverse - mainly for data wrangling & plotting/mapping
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)
#sf - spatial package for mapping 
if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf")
}
require(sf)


##############
# Import data 
##############
## iDigBio
#iDigBio for the focal species can be acquired using the `data_explorationridigbio.R` 
#script. The iDigBio data are unprocessed, meaning they still require the 
#extraction of phenophase characteristics (description, and status 
#-presence/absence) and other spatial information. 

#read in non-native species data
#Acer palmatum
ap = read.csv("exotic_Acer_palmatum_usa.csv", header = TRUE)
#Hymenocallis littoralis
hl = read.csv("exotic_Hymenocallis_littoralis_usa.csv", header = TRUE)
#Ilex aquifolium
ia = read.csv("exotic_Ilex_aquifolium_usa.csv", header = TRUE)
#Juglans regia
jr = read.csv("exotic_Juglans_regia_usa.csv", header = TRUE)
#Rubus laciniatus
rl = read.csv("exotic_Rubus_laciniatus_usa.csv", header = TRUE)


#read in native species data
#Acer rubrum
ar = read.csv("native_Acer_rubrum_usa.csv", header = TRUE)
#Hymenocallis occidentalis
ho = read.csv("native_Hymenocallis_occidentalis_usa.csv", header = TRUE)
#Ilex decidua
id = read.csv("native_Ilex_decidua_usa.csv", header = TRUE)
#Juglans nigra
jn = read.csv("native_Juglans_nigra_usa.csv", header = TRUE)
#Rubus spectabilis
rs = read.csv("native_Rubus_spectabilis_usa.csv", header = TRUE)


## NEON/NPN/Herbarium data 
# The following data file can be found in the (ESIIL Macrophenology Cyverse L1>Across_SDM folder): 
#   - `FocalSpp_OpenFlowers_NeonNpnHerb_iDigBiounprocessed.csv` 
# You can find open flower data for all native species and *Rubus laciniatus* from the NEON, USA-NPN, and herbarium data citations:
# Data Citations: 
# 1. Park, Isaac et al. (2023). Herbarium-Derived Phenological Data in North America [Dataset]. Dryad. https://doi.org/10.25349/D9WP6S. Accessed Feb 2023 
# 2. Park, D., A. Williams, E. Law, A. Ellison, and C. Davis. 2023. Assessing Plant Phenological Patterns in the Eastern United States Over the Last 120 Years ver 5. Environmental Data Initiative. https://doi.org/10.6073/pasta/bfb70a1701ef23f686fcc73840e6ae17 (Accessed 2024-09-24). 
# 3. NEON (National Ecological Observatory Network). 2024. Plant phenology observations, DP1.10055.001 (RELEASE-2023). 2013-2021 for Region: Contiguous United States. Dataset accessed May 2023 via the USA National Phenology Network at
# http://doi.org/10.5066/F78S4N1V.  
# 4. Switzer J, Chamberlain S, Marsh L, Wong K (2024). _rnpn: Interface to the National 'Phenology' Network 'API'_. R package version 1.2.8.0, <https://CRAN.R-project.org/package=rnpn>.  

phe = read.csv("FocalSpp_OpenFlowers_PA_NeonNpnHerb.csv", header = TRUE)


# Let's take a peak into some of the data. Uncomment lines of code that are of interest. 
#phe
#View column names 
names(phe)
#View the structure of the data
# str(phe)
#View data dimensions (row & column numbers)
# dim(phe)
#Unique species present
sort(unique(phe$species))
#Unique years of observation 
unique(phe$year_rect)
#unique phenophase status 
unique(phe$phenophase_status)


# For brevity, lets look at one species for iDigBio 
#ap
#View column names 
names(ap)
#View the structure of the data
# str(ap)
#View data dimensions (row & column numbers)
# dim(ap)



####################
# Wrangling iDigBio
####################
#Function to apply repetitive column formatting. 

#function to reformat and clean iDigBio data 
#inputs: df = data frame, soi = species of interest
#outputs: df with day of year column 
#----------------------------
iDigBio.reformat <- function(df, soi){ #START iDigBio function 
#set date column values as date class 
df$datecollected = as.Date(df$datecollected)
#split the datecollected field into three new columns and save in dataframe
df[c("year_rect", "month", "day")] <- str_split_fixed(df$datecollected, '-', 3)


## Calculating DOY 
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
df$day_of_year= leap_every_year(df$datecollected)


#change column name to match main dataset (phe) field names 
df = df %>%
  #renaming existing columns 
  rename(observation_id = uuid, observation_date = datecollected, 
         longitude = geopoint.lon, latitude = geopoint.lat) %>%
  #Adding new columns - some with info that does not apply to iDigBio - but important to other data sources, -1 == NA
  mutate(species = soi, site_id = NA, state = NA, individual_id = NA, 
         phenophase_id = NA, phenophase_description = NA, 
         phenophase_status = -1, data_name = "iDigBio", DomainID = NA, 
         record_id = NA, references = NA) %>%
  #Removing unnecessary columns 
  select(-c(X, month, day))


#Set day of year column as an integer class 
df$year_rect = as.integer(df$year_rect)

#output the df
return(df)

} #END iDigBio function 


################################
#Apply function to iDigBio data. 
################################
#exotic species 
phe.ap <- iDigBio.reformat(ap, soi = "Acer palmatum")
phe.hl <- iDigBio.reformat(hl, soi = "Hymenocallis littoralis")
phe.ia <- iDigBio.reformat(ia, soi = "Ilex aquifolium")
phe.jr <- iDigBio.reformat(jr, soi = "Juglans regia")
phe.rl <- iDigBio.reformat(rl, soi = "Rubus laciniatus")

#native species 
phe.ar <- iDigBio.reformat(ar, soi = "Acer rubrum")
phe.ho <- iDigBio.reformat(ho, soi = "Hymenocallis occidentalis")
phe.id <- iDigBio.reformat(id, soi = "Ilex decidua")
phe.jn <- iDigBio.reformat(jn, soi = "Juglans nigra")
phe.rs <- iDigBio.reformat(rs, soi = "Rubus spectabilis")



################
# Merging data 
###############
#Merge all iDigBio 
## specific to non-native species
phe.exotic = rbind(phe.ap, phe.hl, phe.ia, phe.jr, phe.rl) 
phe.exotic$native_status = "non-native"
## specific to native species
phe.native = rbind(phe.ar, phe.ho, phe.id, phe.jn, phe.rs) 
phe.native$native_status = "native"

#all combined 
phe.idb = rbind(phe.exotic, phe.native)


#Distinct species info from phe -- so phe & phe.idb have equal column #s 
sp = phe %>%
  select(species, species_id, common_name) %>%
  distinct() #unique species
#joing the species information
phe.idb = left_join(phe.idb, sp, by = "species")


#need to convert column into date else iDigBio dates converted into weird values
phe$observation_date = as.Date(phe$observation_date)


#Merge all data
phe.all = rbind(phe, phe.idb)

#export dataset - uncomment & adjust file pathway as necessary 
write.csv(phe.all, file = "FocalSpp_OpenFlowers_NeonNpnHerb_iDigBiounprocessed.csv", row.names = FALSE)





