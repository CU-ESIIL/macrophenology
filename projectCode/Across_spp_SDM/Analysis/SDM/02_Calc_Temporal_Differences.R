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
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")}; library(tidyverse)

#terra - spatial package 
if (!requireNamespace("terra", quietly = TRUE)) {
  install.packages("terra")}; require(terra)

#tidyterra - spatial package for mapping 
if (!requireNamespace("tidyterra", quietly = TRUE)) {
  install.packages("tidyterra")}; require(tidyterra)


#############
#Directories
#############
##Main working directory
mwd = "G:/Shared drives/MSB_Phenology/Amador/Amador, Liz-Phenology Dissertation 2023/Chapters/Chapter4/Data"
# mwd = "/iplant/home/esokol/esiil_macrophenology"
# mwd = "~/data-store/home/lamador/Data"
L2 = file.path(mwd, "L2")
# L2 = file.path(mwd, "L2/Across_spp_SDM") #data 
#output directory -- important to set wd here for biomod2 to pull dependencies
em = file.path("D:/Career_Journey/Collaborations/ESIIL_Macrophenology/Across_spp_SDM/Data/L2/SDM/SDM_Uncertainty") #output file for results

#dir.create("Range") #create directory 
# output = file.path(L2, "sp_diff")
output = file.path("D:/Career_Journey/Collaborations/ESIIL_Macrophenology/Across_spp_SDM/Data/L2/sp_diff")

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
#---------Calc differences raster----------
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
  
  message("4. Calculate the total error for: ", i)
  #Calculate standard deviation for each time period
  sd1 = r.c.sp$cv*r.c.sp$mean
  sd2 = r.h.sp$cv*r.h.sp$mean
  #Calculate the variance for each time period
  var1 = (sd1)^2
  var2 = (sd2)^2
  #Sum the variance for the total error between time periods
  vard = var1 + var2 
  #Calculate the standard deviation between time periods 
  sd.dif = sqrt(vard)
  names(sd.dif) <- ("sd")
  
  #recombine layers under a single object
  r.sp.dif = c(mu.dif, sd.dif)
  # Save raster to file
  out.path <- file.path(output, paste0(gsub(" ", "_", i), "_diff_mean_sd_", type, ".tif"))
  writeRaster(r.sp.dif, filename = out.path, overwrite = TRUE)
  
}#END of for loop



#---------Export into csv---------
message("Begin csv conversion for: ", type)
#convert into a df & export as a csv 
#fields: longitude, latitude, value, metric(mean, sd), species,

#List tif files
tif_files <- list.files(
  path = file.path(output), 
  pattern = paste0(type, "\\.tif$"), 
  full.names = TRUE)

# Read rasters and name them by species (strip extension and path)
em.dif.rasters <- lapply(tif_files, rast)
names(em.dif.rasters) <- gsub(paste0("_diff_mean_sd_", type, "\\.tif$"), "", basename(tif_files))

#Empty list to store data frames
df.list <- list()

for (species in names(em.dif.rasters)) { #START for loop
  r <- em.dif.rasters[[species]]
  
  #Convert raster to data frame with coordinates
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  colnames(df)[1:2] <- c("longitude", "latitude")
  
  #Pivot from wide (mean/sd layers) to long format (metric/val)
  # df_long <- df %>%
  #   pivot_longer(
  #     cols = -c(longitude, latitude),
  #     names_to = "metric",
  #     values_to = "val"
  #   )
  
  #Add species column (clean underscores, keep Genus species)
  df$species <- sub("^((\\w+\\s+\\w+)).*", "\\1", gsub("_", " ", species))
  # df_long$species <- sub("^((\\w+\\s+\\w+)).*", "\\1", gsub("_", " ", species))
  
  message(str(df))
  #Store
  df.list[[species]] <- df
  
  message("Data frame logged for: ", species)
} #END for loop

#Combine all species
em.dif.df <- bind_rows(df.list)

#Quick check
head(em.dif.df, 10)
table(em.dif.df$metric)
length(unique(em.dif.df$species))

#Write to CSV
write.csv(
  em.dif.df,
  file = file.path(output, paste0("AllSpecies_EMProj_diff_", type, ".csv")),
  row.names = FALSE)

message("Outputted csv for: ", type)

} #END of function 


#Call funciton 
sp.csv.fun("Phenology")
sp.csv.fun("Range")




#############
#Plotting
#############
#save plots in the for loop
# pdf(file = file.path(output, paste0("AllSpecies_EMProj_diff(Current-Historic)_", type, ".pdf")),
#     width = 9, height = 7)
# for(i in sp){ #START of plotting for loop 
#   #Define a colorblind-friendly diverging palette (blue-gray-orange)
#   cbf_palette <- colorRampPalette(c("#053061", "#f7f7f7", "#67001f"))  
#   # Other palette options:
#   # c("#008080", "#f0f0f0", "#d95f0e")         # Teal → Gray → Orange
#   # c("#4B0082", "#f5f5dc", "#b7410e")         # Indigo → Beige → Rust
#   # c("#6A5ACD", "#ffffff", "#DAA520")         # Slate Blue → White → Goldenrod
#   
#   #Set fixed breaks
#   breaks_fixed <- seq(-1, 1, by = 0.05)
#   n_colors <- length(breaks_fixed) - 1
#   colors <- cbf_palette(n_colors)
#   #Specify desired legend ticks
#   legend_ticks <- c(-1, -0.5, 0, 0.5, 1)
#   
#   # x11()
#   #Adjust margins to avoid legend clipping
#   par(mar = c(5, 4, 4, 6))  #Leave room for the scale bar on the right
#   #Plot using terra::plot with fixed breaks and legend
#   plot(conus, main = paste(i, "Difference in Suitability of", type, "(Curr - Hist)"),
#        col = NA, border=NA, lwd = 0.2, axes = FALSE)
#   #Adjust margins to avoid legend clipping
#   par(mar = c(5, 4, 4, 6))  #Leave room for the scale bar on the right
#   #Plot raster
#   plot(r.sp.dif, #<- find way to make it species specific
#        col = colors,
#        breaks = breaks_fixed,
#        type = "continuous",
#        plg = list(
#          title = "Change in Prob", 
#          cex = 1,
#          at = legend_ticks, #specify tick locations
#          labels = as.character(legend_ticks)  #specify tick labels
#        ),
#        range = c(-1, 1),
#        axes = TRUE, add = TRUE)
# } #End of plotting for loop
# dev.off()

