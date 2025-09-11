# Calculate the temporal differences of Phenology 
#Author: Lizbeth G Amador 

#This script finds the best threshold for each species and time period to binarize
#the raster SDM outputs. Then the difference between time periods are computed. 


#########
#Presets 
#########
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

#tidyterra - spatial package for mapping 
if (!requireNamespace("tidyterra", quietly = TRUE)) {
  install.packages("tidyterra")
}
require(tidyterra)



#Directories
##Data origin 
getwd() #curious what it looks like 
#ensemble folder for Range - output folder
#dir.create("Range") #create directory 
output = file.path(getwd(), "sp_diff")

#Ensure the directory exists!
if (!dir.exists(output)) {
  dir.create(output, recursive = TRUE)
  message("Created directory at: ", output)
} else {
  message("Using existing directory at: ", output)
}



######################################
# Binarize & calc differences raster
#####################################
#1. List .tif files 
tif.files.h = list.files(path = L2.em,
                        pattern = "_EMproj_H_Phenology\\.tif$",
                        full.names = TRUE)
tif.files.c = list.files(path = L2.em,
                         pattern = "EMproj_C_Phenology\\.tif$",
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
conus <- vect("conus.shp")
# Reproject US shapefile to match rasters, if needed
conus <- project(conus, crs(rast.h$`Acer glabrum`))  #Make sure it's in same CRS - pick a random raster



#species list 
sp = names(rast.c) #going with current since it has less species that made it through modeling #length(names(rast.c)); length(names(rast.h)
 
#save plots in the for loop
pdf(file = file.path(output, paste0("AllSpecies_EMProj_diff(Current-Historic)_Phenology.pdf")),
      width = 9, height = 7)
for(i in sp){
  #skips species with no historic or current raster -- some did not get projected for different reasons (no viable single or ensemble models)
  if (!(i %in% names(rast.h))) {
    message("Skipping ", i, ": no historic raster found.")
    next #skips species 
  }
  if (!(i %in% names(rast.c))) {
    message("Skipping ", i, ": no current raster found.")
    next #skips species 
  }
  
  message("Calculating differences for ", i)
  
  # For each species in loop:
  r.h.sp <- crop(rast.h[[i]], conus)
  r.h.sp <- mask(r.h.sp, conus)
  
  r.c.sp <- crop(rast.c[[i]], conus)
  r.c.sp <- mask(r.c.sp, conus)
  #force resolution/resampling too
  r.c.sp <- resample(r.c.sp, r.h.sp, method = "bilinear")  # Before dividing or thresholding
  
  #Rescale to probabilities 
  r.h.re = r.h.sp / 1000
  r.c.re = r.c.sp / 1000
  
  #Align current raster to historic raster if needed 
  if (!compareGeom(r.h.re, r.c.re)) {
    r.c.re <- resample(r.c.re, r.h.re, method = "near") #use "near" since it's binary
  } else{message("Rasters in the same extent!")}
  
  #Calculate differences 
  em.dif = r.c.re - r.h.re
  
  #Plot
  #Define a colorblind-friendly diverging palette (blue-gray-orange)
  cbf_palette <- colorRampPalette(c("#053061", "#f7f7f7", "#67001f"))  
  # Other palette options:
  # c("#008080", "#f0f0f0", "#d95f0e")         # Teal → Gray → Orange
  # c("#4B0082", "#f5f5dc", "#b7410e")         # Indigo → Beige → Rust
  # c("#6A5ACD", "#ffffff", "#DAA520")         # Slate Blue → White → Goldenrod
  
  #Set fixed breaks
  breaks_fixed <- seq(-1, 1, by = 0.05)
  n_colors <- length(breaks_fixed) - 1
  colors <- cbf_palette(n_colors)
  #Specify desired legend ticks
  legend_ticks <- c(-1, -0.5, 0, 0.5, 1)
  
  # x11()
  #Adjust margins to avoid legend clipping
  par(mar = c(5, 4, 4, 6))  #Leave room for the scale bar on the right
  #Plot using terra::plot with fixed breaks and legend
  plot(conus, main = paste(i, "Difference in Peak Flowering Suitability (Curr - Hist)"),
       col = NA, border=NA, lwd = 0.2, axes = FALSE)
  #Adjust margins to avoid legend clipping
  par(mar = c(5, 4, 4, 6))  #Leave room for the scale bar on the right
  #Plot raster
  plot(em.dif,
       col = colors,
       breaks = breaks_fixed,
       type = "continuous",
       plg = list(
         title = "Change in Prob", 
         cex = 1,
         at = legend_ticks, #specify tick locations
         labels = as.character(legend_ticks)  #specify tick labels
       ),
       range = c(-1, 1),
       axes = TRUE, add = TRUE)
  
  
  #########################
  # DEFAULT COLOR SCHEME
  #########################
  # x11()
  #Adjust margins to avoid legend clipping
  par(mar = c(5, 4, 4, 6))  #Leave room for the scale bar on the right
   #plot transparent conus to standardize the plot extents 
  plot(conus, main = paste(i, "Difference in Peak Flowering Suitability (Curr - Hist)"),
       col = NA, border=NA, lwd = 0.2, axes = FALSE)
  #Adjust margins to avoid legend clipping
  par(mar = c(5, 4, 4, 6))  #Leave room for the scale bar on the right
  #plot raster
  plot(em.dif,
       breaks = breaks_fixed,
       type = "continuous",
       plg = list(
         title = "Change in Prob", 
         cex = 1,
         at = legend_ticks, #specify tick locations
         labels = as.character(legend_ticks)  #specify tick labels
       ),
       range = c(-1, 1),
       axes = TRUE, add = TRUE)
  
  
  # Save raster to file
  out_path <- file.path(output, paste0(gsub(" ", "_", i), "_EMProj_diff_Phenology.tif"))
  writeRaster(em.dif, filename = out_path, overwrite = TRUE)

  }#END of for loop
dev.off()



###################
# Export into csv
###################
#convert into a df & export as a csv 
#fields: longitude, latitude, status(0,1), species, trait(p,r)

#list tif files
tif_files <- list.files(path = file.path(output), 
                        pattern = "Phenology\\.tif$", 
                        full.names = TRUE)

# Read rasters and name them by species (strip extension and path)
em.dif.rasters <- lapply(tif_files, rast)
names(em.dif.rasters) <- gsub("_Phenology\\.tif$", "", 
                              basename(tif_files))


#Convert each raster into a data frame
#empty list to save into 
df.list <- list()

for (species in names(em.dif.rasters)) {#START if for loop
  r <- em.dif.rasters[[species]]
  #Convert raster to data frame with coordinates
  df <- as.data.frame(r, xy = TRUE, cells = FALSE, na.rm = TRUE)
  #Rename value column
  colnames(df) <- c("longitude", "latitude", "diff_val")
  #Add species column
  df$species <- df$species <- sub("^((\\w+\\s+\\w+)).*", "\\1", gsub("_", " ", species))
  #Append
  df.list[[species]] <- df
} #END for loop 

#BInd rows and write as a csv
em.dif.df <- bind_rows(df.list)
# View and write
head(em.dif.df, 10)
# table(em.dif.df$diff_val)
length(unique(em.dif.df$species))
table(em.dif.df$species)
write.csv(em.dif.df,
          file = file.path(output, "AllSpecies_EMProj_diff_Phenology.csv"),
          row.names = FALSE)

