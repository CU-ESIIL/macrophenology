
#Occurrence data
#Author: Lizbeth G Amador 

#This script pulls FIA occurrence data from rfia package 


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

#rFIA -- to download FIA data
if (!requireNamespace("rFIA", quietly = TRUE)) {
  install.packages("rFIA")
}
library(rFIA)

#data.table - alternatives to read.csv and friends  
if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table")
}
library(data.table)

#Source file directories
source(file.path(getwd(), "File_Directories.R"))
#read species list 
species_list = read.csv(file = file.path(data.dir, "OpenFlowers_NPN_Herb_CommSpecies_list.csv"), 
                        header = TRUE)
#vector of focal species 
species = species_list$species



# FIA Citation:
#   Stanke, Hunter; Finley, Andrew O.; Weed, Aaron S.; Walters, Brian F.; Domke, 
# Grant M. 2020. rFIA: An R package for estimation of forest attributes with the 
# US Forest Inventory and Analysis database. Environmental Modelling & Software. 
# 127(9): 104664. https://doi.org/10.1016/j.envsoft.2020.104664. [accessed 05/2025]



#######################
# 1. Setting options, directories, and downloading FIA data 
#######################
#Adjust global timeout 
options(timeout=3600)

#Automatically make FIA folder for current directory 
#Define a FIA directory path
fia_dir <- file.path(getwd(), "FIA")
# Check if the folder exists — if not, create it
if (!dir.exists(fia_dir)) {
  dir.create(fia_dir)
  message("Created FIA directory at: ", fia_dir)
} else {
  message("Using existing FIA directory at: ", fia_dir)
}

#list of state codes 
allstates = c("AK", "AL", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", 
              "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", 
              "MO", "MT", "NE", "NV", "NH", 'NJ', "NM", "NY", "NC", "ND", "OH", "OK", 
              "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", 
              "WI", "WY")

#Get species FIA Data - will take a while to load!
getFIA(
  states = allstates,
  dir = file.path(fia_dir), #FIA folder directory where .csv will be saved  
  tables = c("PLOT", "COND", "TREE", "POP_PLOT_STRATUM_ASSGN", "POP_ESTN_UNIT", 
             "POP_EVAL", "POP_STRATUM", "POP_EVAL_TYP", "POP_EVAL_GRP"), #necessary tables only
  load = FALSE #do you want to store in R? otherwise downloads data 
)


#######################
# 2. calculate data from all states and save a single csv for each state
#######################
#The for loop below is code adapted from Adam Eichenwald 
#for loop to parse through FIA data for all states and exports indv
for (state in allstates){ #START of loop
  
  #Optional: Message to keep track of states being processed 
  message(paste("Processing", state, "...")) 
  
  #1. Load FIA data for the state - see code chunk above for how to download data
  FIAdb <- readFIA(
    dir = fia_dir, #FIA folder directory to where all the .csv files stored
    inMemory = FALSE, 
    ncores = 8, 
    states = state)
  
  #2. Estimate trees per acre 
  TPA <- tpa(
    db = FIAdb,
    byPlot = TRUE,
    nCores = 8,
    bySpecies = TRUE) %>%
    mutate(PLT_CN = as.character(PLT_CN))
  
  #3. Read the corresponding plot file 
  plot_path <- list.files(
    fia_dir, #FIA folder directory 
    pattern = paste0(state, "_PLOT.csv"), 
    full.names = TRUE) 
  
  #Optional: in case there's no data 
  if (length(plot_path) == 0) { #START of if
    message(paste("No plot file found for", state, "- skipping"))
    next
  } #END of if 
  
  #Reading [state]_PLOT.csv
  plot_file <- fread(plot_path[1])
  
  #4. Join plot with TPA data to PLOT file 
  state_plot <- plot_file %>%
    select(CN, LON, LAT) %>%
    distinct() %>%
    mutate(PLT_CN = as.character(CN)) %>%
    data.frame() %>%
    inner_join(TPA, by = "PLT_CN") %>%
    rename(Trees_per_acre = TPA) %>% 
    mutate(state = state) #optional: adds a "state" column
  
  #6. Export result for this state
  output_path <- file.path(fia_dir, paste0(state, "_FIASpecies", ".csv"))
  fwrite(state_plot, output_path)
  
} #End of loop


#######################
# 3. Combine all processed states 
#######################
# Get all output CSVs that start with "_FIASpecies_"
FIAall <- list.files(
  path = fia_dir, #FIA folder directory with saved .csv
  pattern = "_FIASpecies\\.csv$",  # Regex: match files ending with "_FIASpecies.csv"
  full.names = TRUE
) %>%
  lapply(fread) %>%
  rbindlist()

#Optional: Save combined output
fwrite(FIAall, file.path(fia_dir, "_FIASpecies_ALL.csv"))

#Preview combined result
print(head(FIAall))
names(FIAall)


#######################
# 4. We will need the columns that match with `phe.range`columns ("latitude", 
# "longitude", "year_rect", "native_status", "species", "data_name"). This will 
# facilitate data merging.
#######################
fia.sp = FIAall %>%
  select("LON", "LAT", "YEAR", "SCIENTIFIC_NAME") %>%
  rename(longitude = LON, latitude = LAT, year_rect = YEAR, species = SCIENTIFIC_NAME) %>%
  filter(species %in% species) %>%
  mutate(data_name = "FIA", native_status = NA)


#species list in the merged data 
unique(fia.sp$species)
#look at years 
sort(unique(fia.sp$year_rect))


#Plot observations - uncomment 
# rang.sh = vect(fia.sp, geom = c("longitude", "latitude"), keep = TRUE, crs = crs(conus))
# plot(conus, main = "All Phenological Observations"); plot(rang.sh, add=TRUE)


#######################
#Save FIA data 
#######################
write.csv(fia.sp, file = file.path(L1, "FIA_focal_species_occurrence_conus.csv"), 
          row.names = FALSE)













