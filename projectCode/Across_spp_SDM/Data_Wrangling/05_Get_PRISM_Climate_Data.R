
#bil to tif 
#Author: Jill Fedarick & Lizbeth G Amador 

#Thia script reads PRISM monthly temperature and precipitation data from 
#the prism package, and converts the bil files to tif

# Read In packages --------------------------------------------------------
library(prism)
library(terra)
library(tidyverse)
library(sf)


##############################
# PRISM precipitation data 
##############################
#Automatically make iDigBio folder for current directory 
#Define a bil directory path
ppt.dir <- file.path(getwd(), "PRISM/ppt")
# Check if the folder exists — if not, create it
if (!dir.exists(ppt.dir)) {
  dir.create(ppt.dir)
  message("Created PRISM precipitation directory at: ", ppt.dir)
} else {
  message("Using existing PRISM precipitation directory at: ", ppt.dir)
}
bil = file.path(ppt.dir, "bil") #becasue I know I don't want this file format so I will stow it in its own folder 

#sets directory to save climate data
prism_set_dl_dir(bil)
#Checks that directory is valid
prism_check_dl_dir() 

#Pulling monthly PRISM data - for more options see https://cran.r-project.org/web/packages/prism/prism.pdf
get_prism_monthlys(type = "ppt", #precipitation 
                   years = c(1985:2024), #years
                   mon = 03:05, #spring months 
                   keepZip = TRUE) 

#Tells you folder names that data is in
listoffiles<-prism_archive_ls() 

#can plot one of those folders, will show data
#col is either heat or redblue
# pd_image("PRISM_ppt_stable_4kmM3_yyyymm_bil", col = "heat") 

# way to stack layers in prism
prismannual<-pd_stack(listoffiles)
#turn that into a list
prismlayers<-as.list(prismannual)
#empty list for forloop
prismlayers2 <- list()

# Loop through each raster in the list, convert to SpatRaster
for (i in seq_along(prismlayers)) { #START of loop
  projectedrasters <- as(prismlayers[[i]], "SpatRaster")
  projectedSpatRaster <- project(projectedrasters, 'epsg:4326', method = "bilinear" ,mask = FALSE, align = TRUE)
  prismlayers2[[i]] <- projectedSpatRaster
} #END of loop



# Replace "_bil" with "_tiff" in raster names before the loop
listoffiles_tif <- gsub("_bil$", "_tif", listoffiles)


# Loop through the raster list and save each raster as a GeoTiff
for (i in seq_along(prismlayers2)) { #START of loop
  
  # Extract name from the external list
  raster_name <- listoffiles_tif[i]  
  # Handle missing or NULL names
  # if (is.null(raster_name) || raster_name == "") {
  #   raster_name <- paste0("raster_", i)  # Default name if missing
  # }
  
  # Construct full file path
  file_path <- file.path(ppt.dir, paste0(raster_name, ".tif"))
  # Save the raster
  writeRaster(prismlayers2[[i]], filename = file_path, filetype = "GTiff", overwrite = TRUE)
} #END of loop 

print("files converted to .tif and saved :D")




##############################
# PRISM Temperature data 
##############################
#Automatically make folder for current directory 
#Define a bil directory path
temp.dir <- file.path(getwd(), "PRISM/temp")
# Check if the folder exists — if not, create it
if (!dir.exists(temp.dir)) {
  dir.create(temp.dir)
  message("Created PRISM temperature directory at: ", temp.dir)
} else {
  message("Using existing PRISM temperature directory at: ", temp.dir)
}
bil = file.path(temp.dir, "bil") #becasue I know I don't want this file format so I will stow it in its own folder 

#sets directory to save climate data
prism_set_dl_dir(bil)
#Checks that directory is valid
prism_check_dl_dir() 

#Pulling monthly PRISM data - for more options see https://cran.r-project.org/web/packages/prism/prism.pdf
get_prism_monthlys(type = "tmean", #temperature 
                   years = c(1985:2024), #years
                   mon = 03:05, #spring months 
                   keepZip = TRUE) 

#Tells you folder names that data is in
listoffiles<-prism_archive_ls() 

#can plot one of those folders, will show data
#col is either heat or redblue
# pd_image("PRISM_temp_stable_4kmM3_yyyymm_bil", col = "heat") 

# way to stack layers in prism
prismannual<-pd_stack(listoffiles)
#turn that into a list
prismlayers<-as.list(prismannual)
#empty list for forloop
prismlayers2 <- list()

# Loop through each raster in the list, convert to SpatRaster
for (i in seq_along(prismlayers)) { #START of loop
  projectedrasters <- as(prismlayers[[i]], "SpatRaster")
  projectedSpatRaster <- project(projectedrasters, 'epsg:4326', method = "bilinear" ,mask = FALSE, align = TRUE)
  prismlayers2[[i]] <- projectedSpatRaster
} #END of loop



# Replace "_bil" with "_tiff" in raster names before the loop
listoffiles_tif <- gsub("_bil$", "_tif", listoffiles)


# Loop through the raster list and save each raster as a GeoTiff
for (i in seq_along(prismlayers2)) { #START of loop
  
  # Extract name from the external list
  raster_name <- listoffiles_tif[i]  
  # Handle missing or NULL names
  # if (is.null(raster_name) || raster_name == "") {
  #   raster_name <- paste0("raster_", i)  # Default name if missing
  # }
  
  # Construct full file path
  file_path <- file.path(temp.dir, paste0(raster_name, ".tif"))
  # Save the raster
  writeRaster(prismlayers2[[i]], filename = file_path, filetype = "GTiff", overwrite = TRUE)
} #END of loop 

print("files converted to .tif and saved :D")


