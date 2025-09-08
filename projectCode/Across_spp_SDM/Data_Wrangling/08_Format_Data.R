#Formatting data 
#Author: Lizbeth Amador 

#Format data to fit into `biomod2` (includes spatial-temporal thinning and pivots). 
#Phenological data are presence and absence, occurrence data are presence only and will require pseudo-absences. 


#Presets
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

#sf - spatial package
if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf")
}
require(sf)

#BIEN - species range & occurrence 
if (!requireNamespace("BIEN", quietly = TRUE)) {
  install.packages("BIEN")
}
require(BIEN)

#tidyterra - spatial package for mapping
if (!requireNamespace("tidyterra", quietly = TRUE)) {
  install.packages("tidyterra")
}
require(tidyterra)

#tigris - us census info
if (!requireNamespace("tigris", quietly = TRUE)) {
  install.packages("tigris")
}
require(tigris)

#spThin - thinning points 
if (!requireNamespace("spThin", quietly = TRUE)) {
  install.packages("spThin")
}
require(spThin)



#Source file directories
main.dir = "~/data-store/home/lamador/Data"
L2 = file.path(main.dir, "L2")



###############################################################################
# PHENOLOGY DATA
###############################################################################

#Phenology 
load(file.path(L2, "phenology_timeperiods.RData"), verbose = TRUE)
#conus 
conus = vect(file.path(L2, "conus.shp"))


## i. Spatial Constraints 

#species - matches the delimination from BIEN 
#Group species columns 
species_cols = unique(phe.hist$species)


# #to save in a specified directory 
# bien.range.out = "some/directory"
# BIEN_ranges_species(species = species_cols, directory = bien.range.out)
#to load into environment 
ranges = BIEN_ranges_load_species(species = species_cols)
#To make sure polygons have no floating 
#Disable S2
sf_use_s2(FALSE)
# Apply the zero-width buffer trick
ranges <- st_buffer(ranges, 0)
# Make geometries valid
ranges <- st_make_valid(ranges)
# Drop empty or still-invalid geometries
ranges <- ranges[!st_is_empty(ranges), ]
ranges <- ranges[st_is_valid(ranges), ]
# Optionally turn S2 back on for future operations
sf_use_s2(TRUE)
#change to terra object 
ranges.sh = vect(ranges)
#checking if CRS the same 
crs(ranges.sh) == crs(conus)
#if not use this 
ranges.sh <- project(ranges.sh, #raster we want to reproject
                     crs(conus)) #reference raster
# #checking if CRS the same 
crs(ranges.sh) == crs(conus)
# Create a buffer in meters (adjust distance as needed)
ranges.sh.buff = buffer(ranges.sh, width = 20000) #20km
#ensure ranges are contained withint 
ranges.int = intersect(ranges.sh.buff, conus)
#change characer delimination for the secies column
ranges.sh = mutate(ranges.int, species = gsub("_", " ", ranges.int$species))
# 8 Species not captured in the BIEN database :(


#Maps of species BIEN ranges: Now we want to filter range maps for each species   
#species list -- using ranges.sh for species list 
sp = unique(ranges.sh$species)
#save outputs to pdf
pdf(file = file.path(L2, "sp_ranges", "_Species_BIEN_Range.pdf"),
    width = 8, height = 6)
for(i in sp){
  #subsetting for species 
  sp.range = ranges.sh %>% filter(species == i)
  #save shapefile 
  writeVector(sp.range, file = file.path(L2, #object saving directory
                                         "sp_ranges",
                                         paste0(gsub(" ", "_", i), #replace " " with "_"
                                                "_BIEN_Range.shp")),overwrite = TRUE)
  #plot the range maps and save
  p = ggplot() + 
    #CONUS map layer
    geom_spatvector(data = conus, fill = NA, color='grey30') +
    #species range map
    geom_spatvector(data = sp.range, fill="pink")+ 
    #title 
    ggtitle(paste(i, "BIEN Range")) +
    theme_classic()
  
  print(p)
}
dev.off()



#Now we want to intersect species observations with its corresponding range map - 
#historical and current range maps the same for each species 
#Input
#df: input data frame (to intersect)
#tp: character of time period
sp.pt.drop = function(df, tp){ #START of function
  #patchwork - combine ggplots in same figure 
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    install.packages("patchwork")
  }
  library(patchwork)
  #species list -- using ranges.sh for species list 
  sp = unique(ranges.sh$species) 
  #empty object for the for loop
  df.all = list()
  #set df as vector 
  v = vect(df, geom = c("longitude", "latitude"), keep = TRUE, crs = crs(conus))
  #filter for species in the BIEN range thing 
  v = filter(v, species %in% sp)
  #open pdf device - path to save
  pdf(file = file.path(L2, paste0("Species_Phenology_Observations_", 
                                  tp, ".pdf")), 
      width = 8, height = 6)
  
  #for loop to intersect observations using species-specific BIEN ranges (also plots map)
  for(i in sp){ #START of for loop
    #subsetting BIEN ranges for species 
    sp.range = ranges.sh[ranges.sh$species == i, ]
    #subsetting point observations for species 
    sp.v = v[v$species == i, ]
    #plotting unconstrained
    p.uc = ggplot() + #base map
      #CONUS map layer
      geom_spatvector(data = conus, fill=NA, color='grey')+
      #species range layer
      geom_spatvector(data = sp.range, fill="pink") +
      #Points layer
      geom_spatvector(data = sp.v, color = "black") + 
      #title 
      ggtitle(paste(i, tp, "Unconstrained Observations")) +
      theme_classic()
    #drop any points outside of range shapefile 
    v.int = intersect(sp.v, sp.range)
    #plotting constrained 
    p.c = ggplot() + #base map
      #CONUS map layer
      geom_spatvector(data = conus, fill=NA, color='grey')+
      #species range layer
      geom_spatvector(data = sp.range, fill="pink") +
      #Points layer
      geom_spatvector(data = v.int, color = "black") + 
      #title 
      ggtitle(paste(i, tp, "Constrained Observations")) +
      theme_classic()
    p = p.uc / p.c
    print(p)
    #revert back to a dataframe & save into a larger data frame to contain all species 
    df.all[[i]] <- as.data.frame(v.int, row.names = FALSE)
  } #END of loop 
  #close pdf device 
  dev.off()
  #rbind the listed items 
  length(df.all)
  df.all = do.call(rbind, df.all)
  #function output 
  return(df.all)
} #END of function


#apply function to dataframes 
phe.h.const = sp.pt.drop(phe.hist, tp = "Historical")
phe.c.const = sp.pt.drop(phe.cur, tp = "Current")
#remove the double species column - got there from the for loop 
phe.h.const = phe.h.const[, -7]
phe.c.const = phe.c.const[, -7]



## ii. Spatial Thinning 
#Spatially thinning using sf and a minimum distance -- not using the function for 
#phenology since occurrence points have a ton more and that function cannot handle it. 
#Inputs
#df: data frame with "longitude" and "latitude" columns (No NAs)
#cell: grid cell size in m
sf.thin = function(df, km){
  #converting km to m 
  m= km*1000
  #observations per urban area type 
  # print("pre-thin")
  # print(table(df$species))
  #species list -- using ranges.sh for species list 
  sp = unique(ranges.sh$species)
  #empty object for the for loop
  df.all = list()
  #############################  
  #1. Convert to sf object
  #############################
  sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = crs(conus))
  #############################
  #2. Transform to projected CRS (meters) — important for correct distances
  #############################
  sf <- st_transform(sf, crs = 3857)  # Web Mercator
  #############################
  #3. Create spatial grid (4 km resolution, like PRISM)
  #############################
  grid <- st_make_grid(sf, cellsize = m)
  #############################
  #4. Species for loop
  #############################
  #FOR loop to thin points by species 
  for(i in sp){ #START of for loop
    #subsetting for species 
    sp.sf = sf[sf$species == i, ]
    #############################
    #5. Get one point per grid cell (first one found)
    #############################
    intersection <- st_intersects(grid, sp.sf)
    one_per_cell <- lapply(intersection, function(ix) if (length(ix) > 0) ix[1] else NULL)
    selected_indices <- unlist(one_per_cell)
    #############################
    #6. Subset original data
    #############################
    #filter for only the currecnt species 
    df.sp <- filter(df, species %in% i)
    #subset thinned points from the species df
    df.thin <- df.sp[selected_indices, ]
    #add it into the empty list and set as a data frame object 
    df.all[[i]] <- as.data.frame(df.thin)
  }#END of for loop
  #############################
  #7. Recombine all data 
  #############################
  #rbind the listed items 
  length(df.all)
  df.thinned = do.call(rbind, df.all)
  #function output 
  return(df.thinned)
  #observations per urban area type 
  print(paste0("post-thin ", "(", km, " km)"))
  # print(table(df.thin$UATYP10))
  # print(table(df.thin$`Acer rubrum`))
} #END Function


#This will take a while
#saving randomly selected lat/lon
set.seed(1)
#Historic 
phe.h.thin = sf.thin(phe.h.const, 5)
#Current 
phe.c.thin = sf.thin(phe.c.const, 5)
#check species sample size -- We lost one D: N=179
# length(unique(phe.h.thin$species))
# length(unique(phe.c.thin$species))


save(phe.c.thin, phe.h.thin, file = file.path(L2, "Phenology_postthin.RData"))
# load(file = file.path(L2, "Phenology_postthin.RData"), verbose = TRUE)



## iii. Pivotting 
#We want our dataset to be in wide format, were each column pertains to the 
#species - including the lat/long information. We will also exclude any other columns. 
#Historical
#excluding columns 
phe.h.long = phe.h.thin %>%
  dplyr::select(latitude, longitude, species, phenophase_status)
#pivot wider 
phe.h.wide = phe.h.long %>%
  pivot_wider(
    names_from = species, #column containing values to be converted to columns 
    values_from = phenophase_status, #cell values for the new columns
    values_fill = NA) 
#Current
#excluding columns 
phe.c.long = phe.c.thin %>%
  dplyr::select(latitude, longitude, species, phenophase_status)
#pivot wider 
phe.c.wide = phe.c.long %>%
  pivot_wider(
    names_from = species, #column containing values to be converted to columns 
    values_from = phenophase_status, #cell values for the new columns
    values_fill = NA) 


## iv. Processing Data 
# Coarsening sampling resolution - removing "repeated obs" for same locations 
#(aka pseudo-replications)
# Function to clean list-type species value
clean_species_list <- function(x) { #START of function 
  if (is.null(x) || all(is.na(x))) return(NA)
  # Flatten in case x is a list, convert to character
  x <- unlist(x)
  x <- as.character(x)
  # Split space-separated values and check for any '1'
  tokens <- unlist(strsplit(x, " "))
  if ("1" %in% tokens) { #START of if
    return(1)
  } else { #START of else
    return(0)
  } #END of else
} #END of Function


#Group species colmns - unique vector of species 
species_cols = unique(phe.h.thin$species)

#apply function to data frame
#historical 
phe.h.clean <- phe.h.wide %>%
  mutate(across(all_of(species_cols), ~map(.x, clean_species_list))) %>%
  # Unlist the list-columns to flatten them to atomic columns
  mutate(across(all_of(species_cols), ~unlist(.x)))
#current
phe.c.clean <- phe.c.wide %>%
  mutate(across(all_of(species_cols), ~map(.x, clean_species_list))) %>%
  # Unlist the list-columns to flatten them to atomic columns
  mutate(across(all_of(species_cols), ~unlist(.x)))


# Save data
save(phe.h.clean, phe.c.clean, file= file.path(L2, "phenology_timeperiods_thinned_cleaned.RData"))



###############################################################################
# CLIMATE DATA
###############################################################################
#read climate rasters 
clim.h = rast(file.path(L2, #object saving directory
                        "ClimRastS_H_cleaned.tif"))
clim.c = rast(file.path(L2, #object saving directory
                        "ClimRastS_C_cleaned.tif"))
#Check crs 
crs(conus) == crs(clim.h)
#if not use this 
conus <- project(conus, #raster we want to reproject
                 crs(clim.h)) #reference raster
#Check crs 
crs(conus) == crs(clim.h)



## i. Spatial Constraints 
# For the climate data, we only need to crop the extent to that of the species 
#BIEN range. 
# Intersect the raster with the species range maps:  
  
 
#Automatically make folder for to store raster - it's a lot! 
#Define a FIA directory path
out_dir <- file.path(L2, "sp_ranges_clim") 
# Check if the folder exists — if not, create it
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
  message("Created directory at: ", out_dir)
} else {
  message("Using existing directory at: ", out_dir)
}


#species list -- using ranges.sh for species list 
sp = unique(ranges.sh$species) 
#save outputs to pdf
pdf(file = file.path(out_dir, "_Species_Ranges_ClimateCrops.pdf"),
    width = 8, height = 6)
for(i in sp){ #START of for loop
  #subsetting for species 
  sp.range = ranges.sh[ranges.sh$species == i, ]
  #Historic
  clim.h.crop = crop(clim.h, sp.range)
  clim.h.sp = mask(clim.h.crop, sp.range)
  plot(clim.h.sp)
  #Save raster  
  writeRaster(clim.h.sp, file = file.path(out_dir,#object saving directory
                                          paste0("ClimRastS_H_cropped_", 
                                                 gsub(" ", "_", i), 
                                                 ".tif")),overwrite = TRUE)
  
  #Current 
  clim.c.crop = crop(clim.c, sp.range)
  clim.c.sp = mask(clim.c.crop, sp.range)
  plot(clim.c.sp)
  #save raster 
  writeRaster(clim.c.sp, file = file.path(out_dir,#object saving directory
                                          paste0("ClimRastS_C_cropped_", 
                                                 gsub(" ", "_", i), 
                                                 ".tif")),overwrite = TRUE)
}#END of for loop 
dev.off()



###############################################################################
# OCCURRENCE DATA
###############################################################################
#load R object data  
load(file.path(L2, "range_timeperiods.RData"), verbose = TRUE)


## i. Spatial Constraints 
# we will filter for an individual species. We will also apply spatial contraints 
#to observations based on their range from the BIEN package - **historical and 
#current ranges will be the same** 

#Input
#df: input data frame (to intersect)
#tp: character of time period
sp.pt.drop = function(df, tp){ #START of function
  #species list -- using ranges.sh for species list 
  sp = unique(ranges.sh$species) 
  #empty object for the for loop
  df.all = list()
  #set df as vector 
  v = vect(df, geom = c("longitude", "latitude"), keep = TRUE, crs = crs(conus))
  #fikter for species in the BIEN range thing 
  v = filter(v, species %in% sp)
  #open pdf device - path to save
  # pdf(file = file.path(main.dir, paste0("Species_Constrained_Range_Observations_", 
  #                                 tp, ".pdf")), 
  #     width = 8, height = 6)
  #for loop to intersect observations using species-specific BIEN ranges (also plots map)
  for(i in sp){ #START of for loop
    #subsetting for species 
    sp.range = ranges.sh[ranges.sh$species == i, ]
    #subsetting for species 
    sp.v = v[v$species == i, ] 
    #drop any points outside of range shapefile 
    v.int = intersect(sp.v, sp.range)

    #plotting
    # p = ggplot() + #base map
    #   #CONUS map layer
    #   geom_spatvector(data = conus, fill=NA, color='grey30')+
    #   #species range layer
    #   geom_spatvector(data = sp.range, fill="grey") +
    #   #Points layer
    #   geom_spatvector(data = v.int, color = "palevioletred") + 
    #   #title 
    #   ggtitle(paste(i, tp, "Observations")) +
    #   theme_classic()
    # print(p)

    #revert back to a dataframe & save into a larger data frame to contain all species 
    df.all[[i]] <- as.data.frame(v.int, row.names = FALSE)
    
  } #END of loop 
  #close pdf device 
  # dev.off()
  #rbind the listed items 
  length(df.all)
  df.all = do.call(rbind, df.all)
  #function output 
  return(df.all)
} #END of function



#apply function to dataframes 
rang.h.const = sp.pt.drop(rang.h, tp = "Historical")
rang.c.const = sp.pt.drop(rang.c, tp = "Current")
#remove the double species column - got there from the for loop 
rang.h.const = rang.h.const[, -7]
rang.c.const = rang.c.const[, -7]



## ii. Spatial Thinning
### a) Thinning Urban Areas 
#Based on the plots above, there does seem to be bias near urban areas (clusters). 
#So we will thin across space for each time period.

#Download urban area information from `tigris`
#download urban areas shapefile from tigris 
urb <- urban_areas(year = 2020)
#Clean up the shapefile 
#check any floating polygons 
any(!st_is_valid(urb))    # TRUE if invalid
urb <- st_make_valid(urb)   #Fix it
#change to terra object 
urb.tra = vect(urb)
#fix topology overlaps 
urb.tra <- makeValid(urb.tra)
conus <- makeValid(conus)
#ensure shapefiles have same projections 
urb.tra = project(urb.tra, crs(conus))
#check crs 
crs(urb.tra) == crs(conus)
#remove polygons outside of conus 
urb.conus = intersect(urb.tra, conus)
#remove conus columns 
urb.conus = urb.conus %>%
  select(-c("OBJECTID", "Shape_Leng", "DomainID", "DomainName", "Shape_Le_1", "Shape_Area"))


#plot
# colors <- ifelse(values(urb.conus)$UATYP10 == "C", "green", "blue")
# # Plot with custom colors
# plot(urb.conus, col = colors, main = "Urban Types by UATYP10")
# #zoom in
# plot(urb.conus, 
#      xlim = c(-80, -70), ylim = c(40, 45),
#      col = colors, main = "Urban Types by UATYP10 - zoomed")


#Great, now we have a shapefile with urban area information and in the spatial 
#extent we are interested in. We will see which occurrence points fall in urban 
#areas and thin those points at **20km**.

# Making a function for this so we don't have to repeat the long code for each time preiod. 

#x: data frame with lat long. and 
urb.thin = function(x, km){
#creating unique IDs 
df = x %>%
  mutate(uid = row_number())
#################
# urban info
#################
#Converting to terra vector 
v = vect(df, geom = c("longitude", "latitude"), keep = TRUE, crs = crs(conus))
#intersect points with urban info 
urb.v = intersect(v, urb.conus)
#Add the urban info to the main data set -- This will produce some empty cell values 
#vector to dataframe
urb.df = as.data.frame(urb.v)
#remove extra columns 
urb.df = urb.df %>%
  select(c("uid", "NAMELSAD10", "UATYP10"))
#Add urban information to main dataset 
df.jnd = left_join(df, urb.df, by = "uid")
#replace NAs in "NAMELSAD10", "UATYP10"
df.jnd$NAMELSAD10[is.na(df.jnd$NAMELSAD10)] <- "Non-Urban area"
df.jnd$UATYP10[is.na(df.jnd$UATYP10)] <- "NU"
#################
# thin urban pts
#################
#table
print("pre-thin")
print(table(df.jnd$UATYP10))

#thin points in urbans areas only 
urb.thin <- thin(loc.data = df.jnd[ which(df.jnd$UATYP10 == "U" ) , ], 
        lat.col = "latitude", long.col = "longitude", 
        spec.col = names(df.jnd)[4], 
        thin.par = km, #thinning parameter distance (km) records are separated by 
        reps = 1, #thinning repetitions 
        locs.thinned.list.return = TRUE, 
        write.files = FALSE, 
        write.log.file = FALSE)
#convert thinned urban points to df
df.thin = as.data.frame(urb.thin)
#change column name - lower case to match main df 
names(df.thin) <- c("longitude", "latitude")

#################
# join uid to thin
#################
#uid for lat-lon pair from 'urban info'
d = df.jnd %>%
  select(latitude, longitude, uid) %>%
  distinct()
#join uid info to the thinned urban points 
df.t.uid = left_join(df.thin, d, by = c("latitude", "longitude"))
#save uid vector for thinned urban points 
u.thinned.uid = df.t.uid$uid

#################
# patching data 
#################
#Want to replace any urban points in main data set (i.e.df.jnd) with the thinned ones
#pull non urban and urban clusters from main df
df.nu = df.jnd %>%
  filter(UATYP10 %in% c("NU", "C"))
#pull other info for thinned urban points 
df.u = df.jnd %>%
  filter(uid %in% u.thinned.uid)
#merge
df.u.thinned = rbind(df.nu, df.u)
#observations per urban area type -- C & NU should be the same as df.jnd (pre urban thin)
print("post-thin (20km)")
print(table(df.u.thinned$UATYP10))
print(table(df.u.thinned$`Acer rubrum`))
return(df.u.thinned)
}



set.seed(1)
#Historic 
rang.h.urbthin = urb.thin(rang.h.const, km=20)
#Current 
rang.c.urbthin = urb.thin(rang.c.const, km=20)




### b) Thining all points 
# Lovely, now let's thin all observations at **5km**. 
#THis time, we will use a different thinning method since the current dataset is 
#huge and computationally large. 

#Spatially thinning using sf and a minimum distance -- not using the function for 
#phenology since occurrence points have a ton more and that function cannot handle it. 
#Inputs
#df: data frame with "longitude" and "latitude" columns (No NAs)
#cell: grid cell size in m
sf.thin = function(df, km){
  #converting km to m 
  m= km*1000
  #observations per urban area type 
  print("pre-thin")
  print(table(df$UATYP10))
  #############################  
  #1. Convert to sf object
  #############################
  sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = crs(conus))
  #############################
  #2. Transform to projected CRS (meters) — important for correct distances
  #############################
  sf <- st_transform(sf, crs = 3857)  # Web Mercator
  #############################
  #3. Create spatial grid (4 km resolution, like PRISM)
  #############################
  grid <- st_make_grid(sf, cellsize = m)
  #############################
  #4. Get one point per grid cell (first one found)
  #############################
  intersection <- st_intersects(grid, sf)
  one_per_cell <- lapply(intersection, function(ix) if (length(ix) > 0) ix[1] else NULL)
  selected_indices <- unlist(one_per_cell)
  #############################
  #5. Subset original data
  #############################
  df.thin <- df[selected_indices, ]
  df.thin <- as.data.frame(df.thin)
  
  #observations per urban area type 
  print(paste0("post-thin ", "(", km, " km)"))
  print(table(df.thin$UATYP10))
  print(table(df.thin$`Acer rubrum`))
  
  return(df.thin)
}


set.seed(2)
#Historic
rang.h.thin = sf.thin(rang.h.urbthin, 5)
#Current
rang.c.thin = sf.thin(rang.c.urbthin, 5)



#remove the double species column - got there from the for loop 
rang.h.thin = rang.h.thin[, -6]
rang.c.thin = rang.c.thin[, -6]





# custom.plot = function(df, title){
# #convert into spatvector 
# v = vect(df, geom = c("longitude", "latitude"), keep = TRUE, crs = crs(conus))
# #----------------
# #plotting
# p <- ggplot() + #base map
#       geom_spatvector(data = sp.range, fill="grey") +
#       #CONUS map layer
#       geom_spatvector(data = conus, fill=NA, color='grey30')+ 
#       #Points layer
#       geom_spatvector(data = v, aes(color=as.factor(`Acer rubrum`))) + 
#       #unique point value colors 
#       scale_color_manual(
#         values = c("1" = "#044789ff"),
#         labels = c("Present", "Absent"),
#         name = names(v)[3]) +
#       ggtitle(paste(names(v)[3], title)) +
#       theme_classic() + 
#       theme(legend.position.inside = c(0.85, 0.2), 
#         legend.background = element_rect(color = "black", linewidth = 0.5))
# 
# print(p)
# }
# custom.plot(rang.h.thin, title = "Historical Occur Observations")
# custom.plot(rang.c.thin, title = "Current Occur Observations")



## iii. Pivotting 
#We want our dataset to be in wide format, were each column pertains to the 
#species - including the lat/long information. We will also exclude any other columns. 
#First add a `detection_status` column to help with formatting for `biomod2`. 
#So when we pivot, NAs formed can count as a form of pseudo-absence. 
#Adding column with values of "1" for present
rang.h.thin$detection_status = 1
rang.c.thin$detection_status = 1



#Historical
#excluding columns 
rang.h.long = rang.h.thin %>%
  dplyr::select(latitude, longitude, species, detection_status)
#pivot wider 
rang.h.wide = rang.h.long %>%
  pivot_wider(
    names_from = species, #column containing values to be converted to column 
    values_from = detection_status, #cell values for the new columns
    values_fill = NA) 

#Current
#excluding columns 
rang.c.long = rang.c.thin %>%
  dplyr::select(latitude, longitude, species, detection_status)
#pivot wider 
rang.c.wide = rang.c.long %>%
  pivot_wider(
    names_from = species, #column containing values to be converted to columns 
    values_from = detection_status, #cell values for the new columns
    values_fill = NA) 



## iv. Processing Data 
#Coarsening sampling resolution - removing "repeated obs" for same locations 
#(aka pseudo-replications)
# Function to clean list-type species value
clean_species_list <- function(x) { #START of function 
  if (is.null(x) || all(is.na(x))) return(NA)
  # Flatten in case x is a list, convert to character
  x <- unlist(x)
  x <- as.character(x)
  # Split space-separated values and check for any '1'
  tokens <- unlist(strsplit(x, " "))
  if ("1" %in% tokens) { #START of if
    return(1)
  } else { #START of else
    return(0)
  } #END of else
} #END of Function


#Group species colmns - unique vector of species 
species_cols = unique(rang.h.thin$species)
#apply function to data frame
#historical 
rang.h.clean <- rang.h.wide %>%
  mutate(across(all_of(species_cols), ~map(.x, clean_species_list))) %>%
  # Unlist the list-columns to flatten them to atomic columns
  mutate(across(all_of(species_cols), ~unlist(.x)))
#current
rang.c.clean <- rang.c.wide %>%
  mutate(across(all_of(species_cols), ~map(.x, clean_species_list))) %>%
  # Unlist the list-columns to flatten them to atomic columns
  mutate(across(all_of(species_cols), ~unlist(.x)))
# Filter species based on the species list from the phenological data processing 
sp = unique(phe.h.thin$species)
rang.h = cbind(rang.h.clean[1:2], rang.h.clean[, sp])
rang.c = cbind(rang.c.clean[1:2], rang.c.clean[, sp])


save(rang.h, rang.c, file= file.path(L2, "range_timeperiods_thinned_cleaned_focalspecies.RData"))
