#  Pull_media_iDigBio
# Author: Rohit Jha

#This script pulls media image data from iDigBio website using the merged species csv with no NAs

############
## Libraries 
library(httr)
require(dplyr)
require(stringr)


# Define the folder where your CSV file resides inside your project
#only need to change the lines 8 & 9 species names - change as you go
csv_file <- "/Data/L0/iDigBio/03_merged_occur_multimed_NoNA/merged_rubus_spectabilis_nona.csv" # Replace with the actual path to your CSV file
output_folder <- "/Data/L0/iDigBio/04_multimed_img/Rubus_spectabilis"

# Create a folder to store images
if (!dir.exists(output_folder)) dir.create(output_folder)

# Read the data
image_data <- read.csv(csv_file)

# Ensure distinct `coreid` values
image_data <- image_data %>% distinct(coreid, ac.accessURI)

# Loop through each row to download images
for (i in 1:nrow(image_data)) {
  image_url <- image_data$ac.accessURI[i]
  image_filename <- paste0(output_folder, "/", image_data$coreid[i], ".jpg") # Save as coreid.jpg
  
  # Download the image
  tryCatch({
    response <- GET(image_url, write_disk(image_filename, overwrite = TRUE))
    if (response$status_code == 200) {
      cat("Downloaded:", image_filename, "\n")
    } else {
      cat("Failed to download:", image_url, "\n")
    }
  }, error = function(e) {
    cat("Error downloading", image_url, ":", e$message, "\n")
  })
}
