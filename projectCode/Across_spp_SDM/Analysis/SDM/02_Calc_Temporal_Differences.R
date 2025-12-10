# Calculate the temporal differences 
#Author: Liz Amador

#This script finds the difference between to time periods across species and 
#type (phenology/range), using SDM raster output. 


#########
#Presets 
#########
# Load necessary libraries
#if statement to automatically install libraries if absent in r library
#tidyverse - mainly for data wrangling & plotting/mapping
if (!requireNamespace("tidyverse", quietly = TRUE)) { install.packages("tidyverse")}; library(tidyverse)
#terra - spatial package 
if (!requireNamespace("terra", quietly = TRUE)) { install.packages("terra")}; require(terra)
#tidyterra - spatial package for mapping 
if (!requireNamespace("tidyterra", quietly = TRUE)) { install.packages("tidyterra")}; require(tidyterra)


#############
#Directories
#############
##Main working directory
mwd = "D:/Career_Journey/Collaborations/ESIIL_Macrophenology/Across_spp_SDM/Data"
# mwd = "/iplant/home/esokol/esiil_macrophenology"
# mwd = "~/data-store/home/lamador/Data"
L2 = file.path(mwd, "L2")
# L2 = file.path(mwd, "L2/Across_spp_SDM") #data 
#output directory -- important to set wd here for biomod2 to pull dependencies
em = file.path(L2, "SDM/SDM_Uncertainty") #output file for results

#dir.create("Range") #create directory 
# output = file.path(L2, "sp_diff")
output = L2

#Ensure the directory exists!
if (!dir.exists(output)) {
  dir.create(output, recursive = TRUE)
  message("Created directory at: ", output)
} else {
  message("Using existing directory at: ", output)
}


##############
# Function 
##############
#type: Phenology or Range

sp.csv.fun = function(type) { #START of function
  ##################
  #pull raster data 
  ##################
  #1. List .tif files 
  tif.files.h = list.files(path = em,
                           pattern = paste0("_cv_mean_H_", type, "\\.tif$"), #e.g., Acer_glabrum_cv_mean_C_Phenology.tif
                           full.names = TRUE)
  tif.files.c = list.files(path = em,
                           pattern = paste0("_cv_mean_C_", type, "\\.tif$"),
                           full.names = TRUE)
  #2. Extract species names from filenames
  # filenames look like: Genus_species_EMwmeanByTSS_..._H_Phenology.tif
  species.names.h = tif.files.h %>%
    basename() %>%
    str_extract("^[^_]+_[^_]+")  #Extracts "Genus_species"
  species.names.c = tif.files.c %>%
    basename() %>%
    str_extract("^[^_]+_[^_]+")  #Extracts "Genus_species"
  #3. Read rasters into list
  rast.h = lapply(tif.files.h, rast)
  rast.c = lapply(tif.files.c, rast)
  #5. Assign species names as layer names
  names(rast.h) <- gsub("_", " ", species.names.h)
  names(rast.c) <- gsub("_", " ", species.names.c)
  
  #Load US boundary vector (replace with your actual file)
  conus <- vect(file.path(L2, "conus.shp"))
  # Reproject US shapefile to match rasters, if needed
  conus <- project(conus, crs(rast.h$`Acer glabrum`))  #Make sure it's in same CRS - pick a random raster
  
  
  #species list - going with the common species
  sp = intersect(names(rast.h), names(rast.c))
  
  
  ###############
  #pull csv data 
  ###############
  #1. List .tif files 
  csv.files.h = list.files(path = em,
                           pattern = paste0("_Biomod_PRJ_H_", type, "\\.csv$"), #e.g., Acer_glabrum_cv_mean_C_Phenology.tif
                           full.names = TRUE)
  csv.files.c = list.files(path = em,
                           pattern = paste0("_Biomod_PRJ_C_", type, "\\.csv$"),
                           full.names = TRUE)
  #2. Extract species names from filenames
  # filenames look like: Genus_species_EMwmeanByTSS_..._H_Phenology.tif
  species.names.h = csv.files.h %>%
    basename() %>%
    str_extract("^[^_]+_[^_]+")  #Extracts "Genus_species"
  species.names.c = csv.files.c %>%
    basename() %>%
    str_extract("^[^_]+_[^_]+")  #Extracts "Genus_species"
  #3. Read rasters into list
  csv.h = lapply(csv.files.h, read.csv)
  csv.c = lapply(csv.files.c, read.csv)
  #5. Assign species names as layer names
  names(csv.h) <- gsub("_", " ", species.names.h)
  names(csv.c) <- gsub("_", " ", species.names.c)
  
  
  #########################
  #Calc differences raster
  #########################
  # container to store results across species
  all_species_out <- list()
  
  #Loop to find mean/sd differences between time periods 
  for(i in sp){ #START of loop
    #skips species with no historic or current raster -- some did not get projected for different reasons (no viable single or ensemble models)
    if (!(i %in% names(rast.h))) {
      message("Skipping ", i, ": no historic raster found.")
      next #skips species 
    }
    if (!(i %in% names(rast.c))) {
      message("Skipping ", i, ": no current raster found.")
      next #skips species 
    }
    
    message("1. Checking Raster specs for ", i)
    # Making sure data is within CONUS:
    r.h.sp <- crop(rast.h[[i]], conus)
    r.h.sp <- mask(r.h.sp, conus)
    
    r.c.sp <- crop(rast.c[[i]], conus)
    r.c.sp <- mask(r.c.sp, conus)
    #force resolution/resampling too
    r.c.sp <- resample(r.c.sp, r.h.sp, method = "bilinear")  # Before dividing or thresholding
    
    #Align current raster to historic raster if needed 
    if (!compareGeom(r.h.sp, r.c.sp)) {
      r.c.sp <- resample(r.c.sp, r.h.sp, method = "near") #use "near" since it's binary
      message("2. Rasters resampled to the same extent")
    } else{message("2. Rasters in the same extent!")}
    
    #Adjust the naming convention for each layer within rasters
    names(r.h.sp) <- gsub(".*_EMcv.*", "cv", names(r.h.sp))
    names(r.h.sp) <- gsub(".*_EMmean.*", "mean", names(r.h.sp))
    
    names(r.c.sp) <- gsub(".*_EMcv.*", "cv", names(r.c.sp))
    names(r.c.sp) <- gsub(".*_EMmean.*", "mean", names(r.c.sp))
    
    
    message("3. Calculate the Mean difference for: ", i)
    #Calculate differences 
    mu.dif = r.c.sp$mean - r.h.sp$mean
    #convert to data frame 
    mu.dif.df = as.data.frame(mu.dif, xy= TRUE)
    #rename columns 
    names(mu.dif.df) <- c("longitude", "latitude", "mean_diff")
    
    
    message("4. Calculate the total error for: ", i)
    df.c.sp = csv.c[[i]]
    df.h.sp = csv.h[[i]]
    #Calculate the variance across folds 
    var1 = apply(df.c.sp[3:length(names(df.c.sp))], 1, var); var2 = apply(df.h.sp[3:length(names(df.h.sp))], 1, var)
    #Calculate the covariance across folds 
    mat1 <- as.matrix(df.c.sp[, -c(1,2)])  # remove longitude/latitude
    mat2 <- as.matrix(df.h.sp[, -c(1,2)])
    cov12 <- apply(
      cbind(mat1, mat2), 1, function(row) {
        n <- ncol(mat1)
        x <- row[1:n]
        y <- row[(n+1):(2*n)]
        cov(x, y)
      }) #END apply
    #Sum the variance for the total error between time periods
    vard = var1 + var2 + (2*cov12) #need the third summation bc time periods are dependent
    #Calculate the standard deviation between time periods 
    sd.dif.df = as.data.frame(sqrt(vard))
    names(sd.dif.df) <- ("sd_diff")
    
    #recombine under a single object
    r.sp.dif = cbind(mu.dif.df, sd.dif.df)
    
    # add species name column for merging
    r.sp.dif$species <- i
    
    # store in list
    all_species_out[[i]] <- r.sp.dif
    
  } #END loop
  
  
  # ---- Combine ALL species into one data frame ----
  final_output <- dplyr::bind_rows(all_species_out)
  
  #Export 
  write.csv(final_output,
            file = file.path(output, paste0("AllSpecies_EMProj_diff_", type, ".csv")),
            row.names = FALSE)
  message("Outputted csv for: ", type)
} #END of function 


#Call function 
sp.csv.fun(type = "Phenology")
sp.csv.fun(type = "Range")



