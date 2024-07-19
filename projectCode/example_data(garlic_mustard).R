library(dplyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(maps)

# list files in the Data folder
# get your file names to be consistent!
csv_files = list.files("Alliaria_petiolata", full.names = T)
list(csv_files)

data <- read.csv("Alliaria_petiolata/inat_annot_Alliaria_petiolata_USA_.csv") 
nrow(data)

range(data$observed_on)


# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)  # For date handling


# Ensure the 'observed_on' column is in date format
data$observed_on <- as.Date(data$observed_on, format = "%Y-%m-%d")

# Calculate DOY (Day of Year)
data$DOY <- yday(data$observed_on)

# Filter data for observations since 2018 and exclude 2024
data_filtered <- data %>% filter(year(observed_on) >= 2018 & year(observed_on) < 2024)

# Extract the year from the observation date
data_filtered$year <- year(data_filtered$observed_on)

# Group by year and calculate mean latitude and mean DOY of flowers
grouped_data <- data_filtered %>%
  filter(flowers == "Y") %>%
  group_by(year) %>%
  summarize(mean_latitude = mean(latitude, na.rm = TRUE),
            mean_DOY_flowers = mean(DOY, na.rm = TRUE))

# Plotting
ggplot(data_filtered, aes(x = latitude, y = DOY,
                         color = as.factor(year))) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Mean Latitude", y = "Mean DOY of Flowers", title = "Mean Latitude vs Mean DOY of Flowers (2018-2023)") +
  theme_minimal()


