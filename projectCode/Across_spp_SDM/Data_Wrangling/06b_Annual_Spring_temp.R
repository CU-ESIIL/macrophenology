

# temp Spring averages
#Author: Lizbeth G Amador 

#This script aggregates monthly (March, April, May) temperature raster into a 
#yearly spring temperature raster (1910-1950 & 1985-2025) - for each pixel   


#tidyverse - mainly for data wrangling & plotting/mapping
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

#terra - spatial data stuff
if (!requireNamespace("terra", quietly = TRUE)) {
  install.packages("terra")
}
library(terra)



#Source file directories
source(file.path(getwd(), "File_Directories.R"))

#Define the folder path containing the .tif files
#Define a bil directory path
temp.dir <- file.path(getwd(), "PRISM/temp")
# Check if the folder exists — if not, create it
if (!dir.exists(temp.dir)) {
  dir.create(temp.dir)
  message("Created PRISM temperature directory at: ", temp.dir)
} else {
  message("Using existing PRISM temperature directory at: ", temp.dir)
}


#Get a list of all .tif files in the folder - number of (d) digits that can change, 
#(range of month values), $: must end with .tif two time periods have a slightly 
#different naming convention so we will pull those separately 
tif_files_M2 = list.files(temp.dir, pattern = "PRISM_temp_stable_4kmM2_\\d{4}(03|04|05)_tif\\.tif$", full.names = TRUE)
tif_files_M3 = list.files(temp.dir, pattern = "PRISM_temp_stable_4kmM3_\\d{4}(03|04|05)_tif\\.tif$", full.names = TRUE)
#combine lists
tif_files = c(tif_files_M2, tif_files_M3)
#Read each raster file using terra - lapply to apply it to all items in list 
raster_list = lapply(tif_files, rast)
#Convert list to SpatRaster (to hold a collection of rasters)
raster_stack = sprc(raster_list)



#######################
#Averaging 
#######################
#Now I need to average the spring months for each individual year. We will start
#by rearranging the way the raster data are in the list so we can easily average 
#across months for each year. We will create a function that pulls the YYYYMM 
#information from the file names. 

#Function to extract year and month from filenames based on the format
extract_date_info = function(filename) { #START of function
  #Filename is the initial list of .tif files 
  base_name = basename(filename)
  #Extract YYYYMM from "PRISM_temp_stable_4kmM2_YYYYMM_tif.tif"
  date_part = substr(base_name, 24, 29)  #starts at character num 24 and ends at 29
  #Extract year (first 4 characters)
  year = as.numeric(substr(date_part, 1, 4)) 
  #Extract month (last 2 characters)
  month = as.numeric(substr(date_part, 5, 6)) 
  
  return(c(year, month))
} #END of function 



# Apply the function the the list of .tif files and rearrange the rasters so each 
#row is a year and contains the monthly data. 
#Apply function to extract year and month
date_info = t(sapply(tif_files, extract_date_info))
years = date_info[, 1]
months = date_info[, 2]
#Create a dataframe linking rasters with their years
raster_df = data.frame(files = tif_files, year = years, month = months)
#Split rasters by year - each row is a year with the monthly data (in our case all spring months per row)
years_list = split(tif_files, years)


# Now we have a new list of rasters, `years_list`, that contains the monthly 
#raster data for each year in the same row. We can go ahead and calculate the averages! 


# Function to compute yearly averages - creating function and applyingit to data simultaneously
yearly_averages = lapply(names(years_list), function(y) {
  # Read rasters for this year
  rasters_in_year = rast(years_list[[y]])  
  # Compute mean
  mean_raster = app(rasters_in_year, mean, na.rm = TRUE)  
  #Rename the files to inform that these are spring averages 
  names(mean_raster) = paste0("springAvg_", y) #ensures unique raster names - comes up empty though
  return(mean_raster)
})


#Combine results into a single SpatRaster and ensure unique names persist
yearly_stack <- rast(yearly_averages)
#Ensure names are retained
names(yearly_stack) <- paste0("temp_SpringAvg_", names(years_list)) 
#check names 
names(yearly_stack)


# #Export temp annual spring averages -- as stacked tif
writeRaster(raster_stack, filename = file.path(L1, #object with output folder pathway
                                               "prism_temp_Annual_Spring_Avg.tif"), 
            overwrite = TRUE)










