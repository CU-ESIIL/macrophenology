#getting the observation on idigbio for all of the species we have on the list

# Load necessary library
if (!requireNamespace("ridigbio", quietly = TRUE)) {
  install.packages("ridigbio")
}
library(ridigbio)


#loading necesary data
species_data <- read.csv("species_list.csv")

# Create a new column to store observation counts for each species in the USA
species_data$Observation_Count <- NA

# Loop through each species and get the count of observations in the USA
for (i in 1:nrow(species_data)) {
  species_name <- as.character(species_data$species_name[i])  # Ensure species name is a character string
  query <- list(scientificname = species_name, country = "United States")  # Limit to USA
  
  # Fetch records for the current species
  records <- idig_search_records(rq = query, fields = "uuid")  # Use rq = query to specify the query
  
  # Count the number of records for the current species
  count <- length(records$uuid)
  
  # Store the count in the Observation_Count column
  species_data$Observation_Count[i] <- count
  
  # Print progress
  print(paste("Processed:", species_name, "- Count:", count))
  
  # Optional: Adding a small delay to avoid overwhelming the server
  Sys.sleep(1) # 1 second delay
}

# Display the results
print(species_data)

# Save the updated data with counts to a new CSV
write.csv(species_data, "all_species_observation_counts_USA.csv", row.names = FALSE)


########################

#################################

#Now getting the detailed data (uuid, geopoints, and date of observation for native- non-native pair

#Downloading data

#exotic_Rubus_laciniatus
# Define the query for Acer circinatum in the United States
query <- list(scientificname = "Rubus laciniatus", country = "United States")

# Fetch records with geopoint to capture latitude and longitude data
records <- idig_search_records(rq = query, fields = c("uuid", "datecollected", "geopoint"))

# Check the structure of the data to view geopoint information
str(records)

write.csv(records, "exotic_Rubus_laciniatus_usa.csv")


#native_Rubus_spectabilis
# Define the query for Acer circinatum in the United States
query <- list(scientificname = "Rubus spectabilis", country = "United States")

# Fetch records with geopoint to capture latitude and longitude data
records <- idig_search_records(rq = query, fields = c("uuid", "datecollected", "geopoint"))

# Check the structure of the data to view geopoint information
str(records)

write.csv(records, "native_Rubus_spectabilis_usa.csv")



###################################


#native_Hymenocallis_occidentalis
# Define the query for Acer circinatum in the United States
query <- list(scientificname = "Hymenocallis occidentalis", country = "United States")

# Fetch records with geopoint to capture latitude and longitude data
records <- idig_search_records(rq = query, fields = c("uuid", "datecollected", "geopoint"))

# Check the structure of the data to view geopoint information
str(records)

write.csv(records, "native_Hymenocallis_occidentalis_usa.csv")

#exotic_Hymenocallis_littoralis
# Define the query for Acer circinatum in the United States
query <- list(scientificname = "Hymenocallis littoralis", country = "United States")

# Fetch records with geopoint to capture latitude and longitude data
records <- idig_search_records(rq = query, fields = c("uuid", "datecollected", "geopoint"))

# Check the structure of the data to view geopoint information
str(records)

write.csv(records, "exotic_Hymenocallis_littoralis_usa.csv")


#####################################

#exotic_Ilex_aquifolium
# Define the query for Acer circinatum in the United States
query <- list(scientificname = "Ilex aquifolium", country = "United States")

# Fetch records with geopoint to capture latitude and longitude data
records <- idig_search_records(rq = query, fields = c("uuid", "datecollected", "geopoint"))

# Check the structure of the data to view geopoint information
str(records)

write.csv(records, "exotic_Ilex_aquifolium_usa.csv")


#native_Ilex_decidua
# Define the query for Acer circinatum in the United States
query <- list(scientificname = "Ilex decidua", country = "United States")

# Fetch records with geopoint to capture latitude and longitude data
records <- idig_search_records(rq = query, fields = c("uuid", "datecollected", "geopoint"))

# Check the structure of the data to view geopoint information
str(records)

write.csv(records, "native_Ilex_decidua_usa.csv")

######################################

#native_Juglans_nigra
# Define the query for Acer circinatum in the United States
query <- list(scientificname = "Juglans nigra", country = "United States")

# Fetch records with geopoint to capture latitude and longitude data
records <- idig_search_records(rq = query, fields = c("uuid", "datecollected", "geopoint"))

# Check the structure of the data to view geopoint information
str(records)

write.csv(records, "native_Juglans_nigra_usa.csv")


#exotic_Juglans_regia
# Define the query for Acer circinatum in the United States
query <- list(scientificname = "Juglans regia", country = "United States")

# Fetch records with geopoint to capture latitude and longitude data
records <- idig_search_records(rq = query, fields = c("uuid", "datecollected", "geopoint"))

# Check the structure of the data to view geopoint information
str(records)

write.csv(records, "exotic_Juglans_regia_usa.csv")

########################################


#exotic_Acer_palmatum
# Define the query for Acer circinatum in the United States
query <- list(scientificname = "Acer palmatum", country = "United States")

# Fetch records with geopoint to capture latitude and longitude data
records <- idig_search_records(rq = query, fields = c("uuid", "datecollected", "geopoint"))

# Check the structure of the data to view geopoint information
str(records)

write.csv(records, "exotic_Acer_palmatum_usa.csv")


#native_Acer_rubrum
# Define the query for Acer circinatum in the United States
query <- list(scientificname = "Acer rubrum", country = "United States")

# Fetch records with geopoint to capture latitude and longitude data
records <- idig_search_records(rq = query, fields = c("uuid", "datecollected", "geopoint"))

# Check the structure of the data to view geopoint information
str(records)

write.csv(records, "native_Acer_rubrum_usa.csv")

#############################################################








