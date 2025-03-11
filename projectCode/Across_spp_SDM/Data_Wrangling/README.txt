

01_data_exploration_ridigbio.R 
- pulls unprocessed iDigio data using the ridigbio package
- data for the following species: Acer rubrum, Acer palmatum, Hymenocallis spp, Ilex decidua, Ilex aquifolium, Juglans nigra, Juglans regia, Rubus spectabilis, Rubus liciantus 
- can adjust for different focal species 

02_Data_Harmonization_unprocessed_iDigBio.R
- harmonizes NEON, NPN, select herbarium, and unprocessed iDigBio from step 1

03_Data_exploration_NEON_NPN_Herb_Unprocessed_iDigBio.Rmd
- looks at samples sizes within each focal species, across two different time periods (1910-1950, 1985-2025)
- makes figures and maps to visualize temporal and spatial spread of data 

04_pulling_merged_occurrence_multimedia_iDigBio.R
- merges unprocessed iDigBio occurrence data and the multimedia data

05_Clean_iDIgBio_occurrence_multumedia_data.Rmd
- uses 05a_idigbio.multimedia.clean.R function to clean data from step 4
- done for each species 

05a_pull_multimedia_iDigBio.R
- function that takes unprocessed iDigBio occurrence data and the multimedia data info and removes any rows with empty cells or NAs for the geolocation and observation data
- ensures correct species is filtered for  

06_Pull_media_iDigBio.R
- extracts media jpgs from the multimedia info and saves in folder

07_Code_for_annotations.ias
- author: ROhit Jha
- annotation script for Imageant
- used for multimedia annotations on Imageant (see protocol for Iamageant annotations)

08_Merge_iDigBio_data.R
- merges species occurrence data from step 5 with anontated data from step 7

09_Harmonise_NEON_NPN_Herb_iDigBio.R 
- harmonizes NEON, NPN, select herbarium, and processed iDigBio (step 8)

10_Explore_SpatTemp.Rmd
- looks at samples sizes within each focal species, across two different time periods (1910-1950, 1985-2025)
- makes figures and maps to visualize temporal and spatial spread of data 


