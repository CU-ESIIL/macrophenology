#This space will be used for all the analysis for Herbarium related project.

#Annotate all the steps in as detailed as possible. this will be helpful mainly
#mainly when you revisit the code later on 


####combining Acer palmatum
# list files in the Data folder
# get your file names to be consistent!
csv_files = list.files("Species_folder/Acer_palmatum/", full.names = T)
list(csv_files)


#From each of the species folder we will be using two csv files for now
#multimedia.csv, and occurrence.csv

# Load the CSV files
multimedia_a_palmatum <- read.csv("Species_folder/Acer_palmatum/multimedia.csv") #you get the path link to your csv files from list(csv_fiels)
occurrence_a_palmatum <- read.csv("Species_folder/Acer_palmatum/occurrence.csv" )


# Extract 'coreid' from the first column of each DataFrame
multimedia_coreid <- multimedia_a_palmatum[[1]]  # First column of multimedia_df
occurrence_coreid <- occurrence_a_palmatum[[1]]  # First column of occurrence_df

# Check if each coreid in multimedia is found in occurrence
coreid_matches <- multimedia_coreid %in% occurrence_coreid

# Print the result
print(coreid_matches)

# Check if there are any coreids without matches
if (all(coreid_matches)) {
  print("All coreid values in multimedia.csv have a match in occurrence.csv.")
} else {
  print("Some coreid values in multimedia.csv do not have a match in occurrence.csv.")
}


#now we will merge two dataset by common coreid

# Merge the two DataFrames on 'coreid'
merged_df <- merge(multimedia_a_palmatum, occurrence_a_palmatum, by = "coreid")

unique(merged_df$gbif.canonicalName)

table(merged_df$gbif.canonicalName)
# Display the first few rows of the merged dataset
print(head(merged_df))

# Save the merged dataset to a new CSV file if needed
write.csv(merged_df, "merged_Acer_palmatum.csv", row.names = FALSE)





#############################################

####combining Acer rubrum
# list files in the Data folder
# get your file names to be consistent!
csv_files = list.files("Species_folder/Acer_rubrum/", full.names = T)
list(csv_files)


#From each of the species folder we will be using two csv files for now
#multimedia.csv, and occurrence.csv

# Load the CSV files
multimedia_a_rubrum <- read.csv("Species_folder/Acer_rubrum/multimedia.csv") #you get the path link to your csv files from list(csv_fiels)
occurrence_a_rubrum <- read.csv("Species_folder/Acer_rubrum/occurrence.csv" )


# Extract 'coreid' from the first column of each DataFrame
multimedia_coreid <- multimedia_a_rubrum[[1]]  # First column of multimedia_df
occurrence_coreid <- occurrence_a_rubrum[[1]]  # First column of occurrence_df

# Check if each coreid in multimedia is found in occurrence
coreid_matches <- multimedia_coreid %in% occurrence_coreid

# Print the result
table(print(coreid_matches))

# Check if there are any coreids without matches
if (all(coreid_matches)) {
  print("All coreid values in multimedia.csv have a match in occurrence.csv.")
} else {
  print("Some coreid values in multimedia.csv do not have a match in occurrence.csv.")
}


#now we will merge two dataset by common coreid

# Merge the two DataFrames on 'coreid'
merged_df <- merge(multimedia_a_rubrum, occurrence_a_rubrum, by = "coreid")

unique(merged_df$gbif.canonicalName)

table(merged_df$gbif.canonicalName)
# Display the first few rows of the merged dataset
print(head(merged_df))

# Save the merged dataset to a new CSV file if needed
write.csv(merged_df, "merged_Acer_rubrum.csv", row.names = FALSE)





############
####combining Acer platanoides
# list files in the Data folder
# get your file names to be consistent!
csv_files = list.files("Species_folder/Acer_platanoides//", full.names = T)
list(csv_files)


#From each of the species folder we will be using two csv files for now
#multimedia.csv, and occurrence.csv

# Load the CSV files
multimedia_a_platanoides <- read.csv("Species_folder/Acer_platanoides//multimedia.csv") #you get the path link to your csv files from list(csv_fiels)
occurrence_a_platanoides <- read.csv("Species_folder/Acer_platanoides//occurrence.csv" )


# Extract 'coreid' from the first column of each DataFrame
multimedia_coreid <- multimedia_a_platanoides[[1]]  # First column of multimedia_df
occurrence_coreid <- occurrence_a_platanoides[[1]]  # First column of occurrence_df

# Check if each coreid in multimedia is found in occurrence
coreid_matches <- multimedia_coreid %in% occurrence_coreid

# Print the result
table(print(coreid_matches))

# Check if there are any coreids without matches
if (all(coreid_matches)) {
  print("All coreid values in multimedia.csv have a match in occurrence.csv.")
} else {
  print("Some coreid values in multimedia.csv do not have a match in occurrence.csv.")
}


#now we will merge two dataset by common coreid

# Merge the two DataFrames on 'coreid'
merged_df <- merge(multimedia_a_platanoides, occurrence_a_platanoides, by = "coreid")

unique(merged_df$gbif.canonicalName)

table(merged_df$gbif.canonicalName)
# Display the first few rows of the merged dataset
print(head(merged_df))

# Save the merged dataset to a new CSV file if needed
write.csv(merged_df, "merged_Acer_platanoides.csv", row.names = FALSE)




############
####combining Ilex aquidolium
# list files in the Data folder
# get your file names to be consistent!
csv_files = list.files("Species_folder/Ilex_aquidolium/", full.names = T)
list(csv_files)


#From each of the species folder we will be using two csv files for now
#multimedia.csv, and occurrence.csv

# Load the CSV files
multimedia_i_aquidolium <- read.csv("Species_folder/Ilex_aquidolium/multimedia.csv") #you get the path link to your csv files from list(csv_fiels)
occurrence_i_aquidolium <- read.csv("Species_folder/Ilex_aquidolium/occurrence.csv" )


# Extract 'coreid' from the first column of each DataFrame
multimedia_coreid <- multimedia_i_aquidolium[[1]]  # First column of multimedia_df
occurrence_coreid <- occurrence_i_aquidolium[[1]]  # First column of occurrence_df

# Check if each coreid in multimedia is found in occurrence
coreid_matches <- multimedia_coreid %in% occurrence_coreid

# Print the result
table(print(coreid_matches))

# Check if there are any coreids without matches
if (all(coreid_matches)) {
  print("All coreid values in multimedia.csv have a match in occurrence.csv.")
} else {
  print("Some coreid values in multimedia.csv do not have a match in occurrence.csv.")
}


#now we will merge two dataset by common coreid

# Merge the two DataFrames on 'coreid'
merged_df <- merge(multimedia_i_aquidolium, occurrence_i_aquidolium, by = "coreid")

unique(merged_df$gbif.canonicalName)

table(merged_df$gbif.canonicalName)
# Display the first few rows of the merged dataset
print(head(merged_df))

# Save the merged dataset to a new CSV file if needed
write.csv(merged_df, "merged_Ilex_aquidolium.csv", row.names = FALSE)






############
####combining Ilex decidua
# list files in the Data folder
# get your file names to be consistent!
csv_files = list.files("Species_folder/Ilex_decidua/", full.names = T)
list(csv_files)


#From each of the species folder we will be using two csv files for now
#multimedia.csv, and occurrence.csv

# Load the CSV files
multimedia_i_decidua <- read.csv("Species_folder/Ilex_decidua/multimedia.csv") #you get the path link to your csv files from list(csv_fiels)
occurrence_i_decidua <- read.csv("Species_folder/Ilex_decidua/occurrence.csv" )


# Extract 'coreid' from the first column of each DataFrame
multimedia_coreid <- multimedia_i_decidua[[1]]  # First column of multimedia_df
occurrence_coreid <- occurrence_i_decidua[[1]]  # First column of occurrence_df

# Check if each coreid in multimedia is found in occurrence
coreid_matches <- multimedia_coreid %in% occurrence_coreid

# Print the result
table(print(coreid_matches))

# Check if there are any coreids without matches
if (all(coreid_matches)) {
  print("All coreid values in multimedia.csv have a match in occurrence.csv.")
} else {
  print("Some coreid values in multimedia.csv do not have a match in occurrence.csv.")
}


#now we will merge two dataset by common coreid

# Merge the two DataFrames on 'coreid'
merged_df <- merge(multimedia_i_decidua, occurrence_i_decidua, by = "coreid")

unique(merged_df$gbif.canonicalName)

table(merged_df$gbif.canonicalName)
# Display the first few rows of the merged dataset
print(head(merged_df))

# Save the merged dataset to a new CSV file if needed
write.csv(merged_df, "merged_Ilex_decidua.csv", row.names = FALSE)




#######

############
####combining Juglans nigra
# list files in the Data folder
# get your file names to be consistent!
csv_files = list.files("Species_folder/Juglans_nigra/", full.names = T)
list(csv_files)


#From each of the species folder we will be using two csv files for now
#multimedia.csv, and occurrence.csv

# Load the CSV files
multimedia_j_nigra <- read.csv("Species_folder/Juglans_nigra/multimedia.csv") #you get the path link to your csv files from list(csv_fiels)
occurrence_j_nigra <- read.csv("Species_folder/Juglans_nigra/occurrence.csv" )


# Extract 'coreid' from the first column of each DataFrame
multimedia_coreid <- multimedia_j_nigra[[1]]  # First column of multimedia_df
occurrence_coreid <- occurrence_j_nigra[[1]]  # First column of occurrence_df

# Check if each coreid in multimedia is found in occurrence
coreid_matches <- multimedia_coreid %in% occurrence_coreid

# Print the result
table(print(coreid_matches))

# Check if there are any coreids without matches
if (all(coreid_matches)) {
  print("All coreid values in multimedia.csv have a match in occurrence.csv.")
} else {
  print("Some coreid values in multimedia.csv do not have a match in occurrence.csv.")
}


#now we will merge two dataset by common coreid

# Merge the two DataFrames on 'coreid'
merged_df <- merge(multimedia_j_nigra, occurrence_j_nigra, by = "coreid")

unique(merged_df$gbif.canonicalName)

table(merged_df$gbif.canonicalName)
# Display the first few rows of the merged dataset
print(head(merged_df))

# Save the merged dataset to a new CSV file if needed
write.csv(merged_df, "merged_Juglans_nigra.csv", row.names = FALSE)



############
####combining Juglans regia
# list files in the Data folder
# get your file names to be consistent!
csv_files = list.files("Species_folder/Juglans_regia/", full.names = T)
list(csv_files)


#From each of the species folder we will be using two csv files for now
#multimedia.csv, and occurrence.csv

# Load the CSV files
multimedia_j_regia <- read.csv("Species_folder/Juglans_regia/multimedia.csv") #you get the path link to your csv files from list(csv_fiels)
occurrence_j_regia <- read.csv("Species_folder/Juglans_regia/occurrence.csv" )


# Extract 'coreid' from the first column of each DataFrame
multimedia_coreid <- multimedia_j_regia[[1]]  # First column of multimedia_df
occurrence_coreid <- occurrence_j_regia[[1]]  # First column of occurrence_df

# Check if each coreid in multimedia is found in occurrence
coreid_matches <- multimedia_coreid %in% occurrence_coreid

# Print the result
table(print(coreid_matches))

# Check if there are any coreids without matches
if (all(coreid_matches)) {
  print("All coreid values in multimedia.csv have a match in occurrence.csv.")
} else {
  print("Some coreid values in multimedia.csv do not have a match in occurrence.csv.")
}


#now we will merge two dataset by common coreid

# Merge the two DataFrames on 'coreid'
merged_df <- merge(multimedia_j_regia, occurrence_j_regia, by = "coreid")

unique(merged_df$gbif.canonicalName)

table(merged_df$gbif.canonicalName)
# Display the first few rows of the merged dataset
print(head(merged_df))

# Save the merged dataset to a new CSV file if needed
write.csv(merged_df, "merged_Juglans_regia.csv", row.names = FALSE)




############
####combining Rubus laciniatus
# list files in the Data folder
# get your file names to be consistent!
csv_files = list.files("Species_folder/Rubus_laciniatus/", full.names = T)
list(csv_files)


#From each of the species folder we will be using two csv files for now
#multimedia.csv, and occurrence.csv

# Load the CSV files
multimedia_r_laciniatus <- read.csv("Species_folder/Rubus_laciniatus/multimedia.csv") #you get the path link to your csv files from list(csv_fiels)
occurrence_r_laciniatus <- read.csv("Species_folder/Rubus_laciniatus/occurrence.csv" )


# Extract 'coreid' from the first column of each DataFrame
multimedia_coreid <- multimedia_r_laciniatus[[1]]  # First column of multimedia_df
occurrence_coreid <- occurrence_r_laciniatus[[1]]  # First column of occurrence_df

# Check if each coreid in multimedia is found in occurrence
coreid_matches <- multimedia_coreid %in% occurrence_coreid

# Print the result
table(print(coreid_matches))

# Check if there are any coreids without matches
if (all(coreid_matches)) {
  print("All coreid values in multimedia.csv have a match in occurrence.csv.")
} else {
  print("Some coreid values in multimedia.csv do not have a match in occurrence.csv.")
}


#now we will merge two dataset by common coreid

# Merge the two DataFrames on 'coreid'
merged_df <- merge(multimedia_r_laciniatus, occurrence_r_laciniatus, by = "coreid")

unique(merged_df$gbif.canonicalName)

table(merged_df$gbif.canonicalName)
# Display the first few rows of the merged dataset
print(head(merged_df))

# Save the merged dataset to a new CSV file if needed
write.csv(merged_df, "merged_Rubus_laciniatus.csv", row.names = FALSE)



############
####combining Rubus spectabilis
# list files in the Data folder
# get your file names to be consistent!
csv_files = list.files("Species_folder/Rubus_spectabilis/", full.names = T)
list(csv_files)


#From each of the species folder we will be using two csv files for now
#multimedia.csv, and occurrence.csv

# Load the CSV files
multimedia_r_spectabilis <- read.csv("Species_folder/Rubus_spectabilis/multimedia.csv") #you get the path link to your csv files from list(csv_fiels)
occurrence_r_spectabilis <- read.csv("Species_folder/Rubus_spectabilis/occurrence.csv" )


# Extract 'coreid' from the first column of each DataFrame
multimedia_coreid <- multimedia_r_spectabilis[[1]]  # First column of multimedia_df
occurrence_coreid <- occurrence_r_spectabilis[[1]]  # First column of occurrence_df

# Check if each coreid in multimedia is found in occurrence
coreid_matches <- multimedia_coreid %in% occurrence_coreid

# Print the result
table(print(coreid_matches))

# Check if there are any coreids without matches
if (all(coreid_matches)) {
  print("All coreid values in multimedia.csv have a match in occurrence.csv.")
} else {
  print("Some coreid values in multimedia.csv do not have a match in occurrence.csv.")
}


#now we will merge two dataset by common coreid

# Merge the two DataFrames on 'coreid'
merged_df <- merge(multimedia_r_spectabilis, occurrence_r_spectabilis, by = "coreid")

unique(merged_df$gbif.canonicalName)

table(merged_df$gbif.canonicalName)
# Display the first few rows of the merged dataset
print(head(merged_df))

# Save the merged dataset to a new CSV file if needed
write.csv(merged_df, "merged_Rubus_spectabilis.csv", row.names = FALSE)













