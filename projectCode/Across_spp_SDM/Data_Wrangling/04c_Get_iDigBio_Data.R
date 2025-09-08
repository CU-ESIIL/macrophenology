
#Get iDigBio data 

#Author: Lizbeth G Amafor & Rohit Jha


#This script downloads iDigBio occurrence data from ridigbio package 

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

#ridigbio - to download iDigBio data 
if (!requireNamespace("ridigbio", quietly = TRUE)) {
  install.packages("ridigbio")
}
library(ridigbio)


#Source file directories
source(file.path(getwd(), "File_Directories.R"))
#read species list 
species_list = read.csv(file = file.path(data.dir, "OpenFlowers_NPN_Herb_CommSpecies_list.csv"), 
                        header = TRUE)
#vector of focal species 
species = species_list$species



#Automatically make iDigBio folder for current directory 
#Define a iDigBio directory path
idb_dir <- file.path(getwd(), "iDigBio")
# Check if the folder exists — if not, create it
if (!dir.exists(idb_dir)) {
  dir.create(idb_dir)
  message("Created iDigBio directory at: ", idb_dir)
} else {
  message("Using existing iDigBio directory at: ", idb_dir)
}


#######################
# 1. Download Species occurrence records
#######################
#Getting the detailed data (uuid, geopoints, and date of observation for species
pull_idb_records = function(species){ #START of function 
  #Downloading data
  # Define the query in the United States
  query <- list("hasImage" = "true", scientificname = species, country = "United States")
  # Fetch records with geopoint to capture latitude and longitude data
  records <- idig_search_records(rq = query)
  # Check the structure of the data to view geopoint information
  # str(records)
  write.csv(records, file.path(idb_dir, 
                               paste0(gsub(" ", "_", species),
                                      "_usa_occurrence.csv")), row.names = FALSE)
  # Print progress
  print(paste("Dowloaded:", species_name, "occurrence"))
  # Optional: Adding a small delay to avoid overwhelming the server
  Sys.sleep(1) # 1 second delay
} #END of function 


#loop the function for each species 
for(i in 1:nrow(species_data)) {#START of for loop 
  species_name <- as.character(species_data$species[i])  # Ensure species name is a character string
  #function 
  pull_idb_records(species_name)
} #END of for loop 


#######################
# 2. Pulling species list
#######################
#List of species names (formatted for function, i.e., lowercase and space-separated)
#Get all CSV files from the folder
csv_files <- list.files(idb_dir, pattern = "*.csv$", full.names = FALSE)

#Extract species names from filenames
#Get "Genus_species"
species_list <- gsub("^(.*)_usa_occurrence\\.csv$", "\\1", csv_files) 
#Convert "_" to " "
species_list <- gsub("_", " ", species_list) 
#Make genus lowercase -- for dunction to work -- may not need anymore 
species_list <- tolower(species_list)   


#Optional: check extracted species names
print(species_list)


###############################
# 3. Apply cleaning function to all species df
###############################
# Loop over species
for (species in species_list) {
  #Convert species name to filename-friendly format (e.g., underscores)
  species_file <- gsub(" ", "_", species)
  #Construct full path to the CSV file
  file_path <- file.path(idb_dir, paste0(species_file, "_usa_occurrence.csv"))
  #Read the CSV
  df <- read.csv(file_path, header = TRUE)
  
  #Optional: print unique scientific names for verification
  print(unique(df$scientificname))
  
  #Apply your function
  idigbio.multimed.clean(df, species, output.folder)
  
  # Print progress
  print(paste("Cleaned:", species))
  
  # Optional: Adding a small delay to avoid overwhelming the server
  Sys.sleep(1) # 1 second delay
}




###############################
# 4. Merge all species into one df 
###############################









