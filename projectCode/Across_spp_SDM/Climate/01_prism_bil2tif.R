# prism_bil2tif 
#Authors: Jill Fedarick and Liz Amador 

#This scripts pulls prism monthly data and converts the bil files to GeoTiffs 
#exports the GeoTiff files 

# Read In packages --------------------------------------------------------
library(prism)
library(terra)
library(tidyverse)
library(sf)
# PRISM data  ---------------------------------------------------

#Set directory to store data
data.dir = "Data/L0/PRISM/ppt"
bil = file.path(data.dir, "bil") #becasue I know I don't want this file format so I will stow it in its own folder 

#sets directory to save climate data
prism_set_dl_dir(bil)
#Checks that directory is valid
prism_check_dl_dir() 

#Pulling monthly PRISM data - for more options see https://cran.r-project.org/web/packages/prism/prism.pdf
get_prism_monthlys(type = "ppt", #precipitation 
                   years = c(1985:2024), #years -- CHANGE this for each set of years 
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
  file_path <- file.path(data.dir, paste0(raster_name, ".tif"))
  # Save the raster
  writeRaster(prismlayers2[[i]], filename = file_path, filetype = "GTiff", overwrite = TRUE)
} #END of loop 

print("files converted to .tif and saved :D")


