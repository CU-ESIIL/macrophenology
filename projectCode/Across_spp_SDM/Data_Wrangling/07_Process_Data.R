#Processing data 
#Author: Lizbeth Amador

# pre-process data for phenology, species occurence, and climate.   
###############################################################################
# Presets 
###############################################################################
# Load necessary libraries
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

#sf - spatial package (required for some computational packages) 
if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf")
}
require(sf)

#tidyterra - spatial package for mapping 
if (!requireNamespace("tidyterra", quietly = TRUE)) {
  install.packages("tidyterra")
}
require(tidyterra)

#usdm -- colinearity stuff 
if (!requireNamespace("usdm", quietly = TRUE)) {
  install.packages("usdm")
}
require(usdm)

#data.table - alternatives to read.csv and friends  
if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table")
}
library(data.table)

#CoordinateCleaner -- to clean coordinates by multiple tests 
if (!requireNamespace("CoordinateCleaner", quietly = TRUE)) {
  install.packages("CoordinateCleaner")
}
require(CoordinateCleaner)


# Directories 
#Source file directories
main.dir = "~/data-store/home/lamador/Data"
L1 = file.path(main.dir, "L1")
L2 = file.path(main.dir, "L2")


###############################################################################
# Climate Data 
###############################################################################
#####################
## i. Precipitation 
#####################
#annual spring averages
ppt.asa = rast(file.path(L1.ppt, #object saving directory 
                         "prism_ppt_Annual_Spring_Avg.tif"))
#Split the annual spring averages between historical and current. 
#annual spring averages - historic 
ppt.asah = ppt.asa[[1:41]]
#annual spring averages - current 
ppt.asac = ppt.asa[[42:81]]

#####################
## ii. Temperature 
#####################
#annual spring averages 
temp.asa = rast(file.path(L1.temp, #object saving directory
                          "prism_temp_Annual_Spring_Avg.tif"))
#Split the annual spring averages between historical and current. 
#annual spring averages - historic 
temp.asah = temp.asa[[1:41]]
#annual spring averages - current 
temp.asac = temp.asa[[42:80]]

#####################
### iii Reproject
#####################
#Let's make sure the precipitation and temperature rasters are in the same projection!
#Notee: precipitation/temperature rasters were calculated in their respective workflows, so within temp/precip groups are the same projection. 
crs(ppt.asa) == crs(temp.asa)
#The precipitation and temperature data are not in the same projection so we will 
#reproject them. 
temp.asa <- project(temp.asa, #raster we want to reproject
                                 crs(ppt.asa)) #reference raster
temp.asah <- project(temp.asah, #raster we want to reproject
                                 crs(ppt.asa)) #reference raster
temp.asac <- project(temp.asac, #raster we want to reproject
                                 crs(ppt.asa)) #reference raster 
#double checking 
crs(temp.asah) == crs(ppt.asa)

#import study area - CONUS 
conus = vect(file.path(L1, #object saving directory
                       "NEON_Domains_CONUS.shp"), crs = crs(ppt.asa))
#check that phe.sh and conus have same crs
# crs(conus) == crs(ppt.asa)
#save with reprojection
writeVector(conus, file.path(L2, #object saving directory 
                             "conus.shp"), overwrite= TRUE)
#Checking to see if they overlap well
# plot(ppt.asa[[1]]); plot(conus, add = TRUE)

#Great! Now the climate data have been distinguished between the two time periods 
#and are in the same projection.

########################
## iv. Raster stacks 
#######################
#We will summarise all years within each variable (i.e. mean annual averages & 
#mean annual anomalies). After that we will stack the climate variables for each 
#respective time period to use in the modeling.  

#Precipitation 
#Historical annual spring avg -- ppt
ppt.asah.avg = app(ppt.asah, mean, na.rm = TRUE)
#Current annual spring avg -- ppt
ppt.asac.avg = app(ppt.asac, mean, na.rm = TRUE)

#Temperature 
#Historical annual spring avg -- temp
temp.asah.avg = app(temp.asah, mean, na.rm = TRUE)
#Current annual spring avg -- temp
temp.asac.avg = app(temp.asac, mean, na.rm = TRUE)


#Average across years for both time periods to get the overall average (i.e. normals).
#We have the precipitation and temperature for all years already, from the very 
#beginning when reading in the climate data. We will use this to calculate anomalies 
#for each time period.  
#ppt avg 
ppt.avg = app(ppt.asa, mean, na.rm = TRUE)

#temp avg 
temp.avg = app(temp.asa, mean, na.rm = TRUE)

# Calculate anomalies 
#ppt 
#historical
ppt.anom.hist = ppt.asah.avg - ppt.avg
#current 
ppt.anom.cur = ppt.asac.avg - ppt.avg

#temp
#historical
temp.anom.hist = temp.asah.avg - temp.avg
#current 
temp.anom.cur = temp.asac.avg - temp.avg


#Combining all the time period specific variables together 
#Historical 
clim.h = c(ppt.asah.avg, ppt.anom.hist, temp.asah.avg, temp.anom.hist)
#Changing column names 
names(clim.h) <- c("ppt.avg", "ppt.anom", "temp.avg", "temp.anom")
#Current  
clim.c = c(ppt.asac.avg, ppt.anom.cur, temp.asac.avg, temp.anom.cur)
#Changing column names 
names(clim.c) <- c("ppt.avg", "ppt.anom", "temp.avg", "temp.anom")

# #uncomment to save 
# graph.out = "./Graphs"
# png(file = file.path(graph.out, "Map_Historical_ClimateStack.png"))
# plot(clim.h)
# dev.off()
# #current 
# png(file = file.path(graph.out, "Map_Current_ClimateStack.png"))
# plot(clim.c)
# dev.off()



###################
## v. Colinearity 
###################
#Predictor climate variables are often highly correlated, and modeling with 
#correlated variables can lead to inaccurate results, so we will remove those with 
#the highest variance inflation factor (VIF), a measure of variable correlation.

### Historic 
# Now we will remove those variables from consideration that have a high VIF.
envs.vif <- usdm::vifstep(clim.h)
envs.rem <- envs.vif@excluded
clim.h <- clim.h[[!(names(clim.h) %in% envs.rem)]]
#check the rasters inside
names(clim.h)
# plot(clim.h[[1]], main = names(clim.h[[1]]))

### Current 
# Now we will remove those variables from consideration that have a high VIF.
envs.vif <- usdm::vifstep(clim.c)
envs.rem <- envs.vif@excluded
clim.c <- clim.c[[!(names(clim.c) %in% envs.rem)]]
#check the rasters inside
names(clim.c)
# plot(clim.c[[1]], main = names(clim.c[[1]]))


#Removing extra spatRast/Vect obj from enviro data -- takes up too much memory 
rm(list = ls(pattern = c("^temp", "^ppt"))) #^ only search for strings beginning with the pattern 

#save the climate data - so we don't have to run again for other scripts  
#historic
writeRaster(clim.h, file = file.path(L2, #object saving directory
                                     "ClimRastS_H_cleaned.tif"))
#current 
writeRaster(clim.c, file = file.path(L2, #object saving directory
                                     "ClimRastS_C_cleaned.tif"))

###############################################################################
# Phenology Data
###############################################################################
#Now we will read in the phenological data 
#load phenology data 
phe = read.csv(file.path(L2, #object saving directory
                         "Species_Peak_Open_Flowers_NPN_Herb.csv"), header = TRUE)

#Let's clean up the data frame 
#remove extra columns  
phe.strpd = phe %>%
  dplyr::select(latitude, longitude, year_rect,
                species, phenophase_status, data_name) 

#Let's split the data into the two time periods.  
#Historical
phe.hist = phe.strpd %>%
  filter(year_rect >= 1910, year_rect <= 1950)
#Current 
phe.cur = phe.strpd %>%
  filter(year_rect >= 1985)

#save objects 
save(phe.hist, phe.cur, file = file.path(L2, "phenology_timeperiods.RData"))
#to import 
# load(file.path(L2, "phenology_timeperiods.RData"), verbose = TRUE)



###############################################################################
# Species Occurrence 
###############################################################################

## Reference dataset
#Here we will adjust `phe.conus` to reflect the species occurrence aspect of the study. 
#Since we are focused on the individual of the species.  
rang = read.csv(file.path(L2, "Species_Occurrence_NPN_Herb.csv"), header = TRUE)
fia.sp = read.csv(file = file.path(L1, "FIA_focal_species_occurrence_conus.csv"), header = TRUE)
bien.sp = read.csv(file = file.path(L1, "BIEN_occurrence_conus_combined.csv"), header = TRUE)
idgb.sp = read.csv(file = file.path(L1, "Combined_iDigBio_Records.csv"), header = TRUE)


#intersecting columns 
cols = intersect(names(rang), names(fia.sp))
#refine columns 
fia = fia.sp %>% 
  select(all_of(cols))
rang = rang %>% 
  select(all_of(cols))
bien = bien.sp %>%
  select(all_of(cols)) 
idgb = idgb.sp %>%
  rename(longitude = geopoint.lon, latitude = geopoint.lat, year_rect = data.dwc.year) %>%
  mutate(data_name = "iDigBio") %>% 
  select(all_of(cols)) %>%
  drop_na()

## Merge all occurrence data 
#Check that all cloumns are the same class -- uncomment to see 
# str(rang); str(fia); str(bien); str(idgb)

#Merge all df
rang.all = rbind(rang, fia, bien, idgb)

#Filter for time periods of interest
#Historical
rang.h = rang.all %>%
  filter(year_rect >= 1910, year_rect <= 1950)
#Current 
rang.c = rang.all %>%
  filter(year_rect >= 1985)
# plot(rang.hist$longitude, rang.hist$latitude, main="Historical Occurrence Observations")
# plot(rang.cur$longitude, rang.cur$latitude, main="Comtemporary Occurrence Observations")


#save objects 
save(rang.h, rang.c, file = file.path(L2, "range_timeperiods.RData"))
#to import 
# load(file.path(L2, "range_timeperiods.RData"), verbose = TRUE)

