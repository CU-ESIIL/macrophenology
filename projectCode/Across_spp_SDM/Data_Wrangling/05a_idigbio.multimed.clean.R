# idigbio.multimed.clean function
# Author: Liz Amador 

# This function goes through merged multimedia & occurrence information from 
#iDigBio. This filterers any missing information from observation dates and geolocations
# and pulls data for a specified species. 

# Columns of interest: 
# - "gbif.canonicalName": scientific name                    
# - "idigbio.eventDate": data of observation/collection                 
# - "idigbio.geoPoint": geolocation  


## Libraries 
#if statement to automatically install libraries if absent in r library
#tidyverse - mainly for data wrangling & plotting/mapping
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)
#stringr - package for finding character patterns  
if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr")
}
require(stringr)


# Inputs: 
# - df: object, data frame from iDigBio multimedia download 
# - species: character, species of interest based on Unique(df$gbif.canonicalName) 
# - file.dir: object/character string, character string of data directory for file export

idigbio.multimed.clean = function(df, species, file.dir){
  
  # Filter for specific species and within time period of interest
  df.only = df %>%
    filter(str_detect(gbif.canonicalName, species), #to include subspecies too 
           idigbio.eventDate >= "1900*")
  
  #Check - head() only outputs the first so rows
  head(sort(unique(df.only$idigbio.eventDate)), 4)
  print(unique(df.only$gbif.canonicalName))

  
  
  # Remove rows containing NAs in year & geolocation columns - just in case
  #remove rows with NAs in lat/long columns
  df.nona = df.only[!is.na(df.only$idigbio.geoPoint), ]
  df.nona = df.only[!is.na(df.only$idigbio.eventDate), ]
  
  print(paste("sample after removing NAs:", dim(df.nona)[1], sep = " "))
  
  # Remove any subspecies info 
  #If statement to detect any subspecies included
  #will but structure as genus species subspecies, so we will detect anything with a space after species & assume there is a subspecies attached and replace it with just genus & species 
  ls = unique(df.nona$gbif.canonicalName)
  if(length(ls) > 1) { 
    df.nona$gbif.canonicalName[df.nona$gbif.canonicalName != species] <- species
    print(unique(df.nona$gbif.canonicalName))
    print(paste("sample after renaming subspecies:", dim(df.nona)[1]), sep = " ")
  } else {print(unique(df.nona$gbif.canonicalName))}
  
  
 


  # Export data  
  write.csv(df.nona, 
            file = file.path(file.dir, 
                             paste0("merged_",
                                    gsub(" ", "_", species), #grep-like function to include the specific species name & replacing spaces wih "_"
                                    "_nona", ".csv")), row.names = FALSE)
 
  
  
  # Check there are no NAs/empty cells 
 
  # Sum of NAs
  gp.na = sum(is.na(df.nona$idigbio.geoPoint))
  ed.na = sum(is.na(df.nona$idigbio.eventDate))
  # Sum of empty cells 
  gp.emp = sum(df.nona$idigbio.geoPoint=="")
  ed.emp = sum(df.nona$idigbio.eventDate=="")
  # save table contents as vector 
  val = c(species, gp.na, gp.emp, ed.na, ed.emp)
  # Create matrix with 5 columns and 1 row
  table = matrix(val, ncol=5, byrow=TRUE)
  
  # Specify the column names and row names of matrix
  colnames(table) = c("Species", "geoPoint.na", "geoPoint.emp", 
                     "eventDate.na", "eventDate.emp")
  
  # Assign to table
  output = as.table(table)
  
  # Display
  print(output)
  
}


print("idigbio.multimed.clean(df, species, file.dir)") 
print("df: object name of iDigBio data")
print("species: scientific name of focal species")
print("file.dir: file directory fo choice - where to export cleaned data (e.g. proj1/datafolder)")







