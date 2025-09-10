

#Peak Flower windows 
#Author: Lizbeth G Amador & Katie Jones 


# This script takes observed phenology data and detects the flower rnage and peaks
#For each species across NEON domains -- where present 


#Libraries 
#if statement to automatically install libraries if absent in r library
#tidyverse - mainly for data wrangling & plotting/mapping
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

#terra - spatial data stuff
if (!requireNamespace("terra", quietly = TRUE)) {
  install.packages("terra")
}
library(terra)

#tidyverse - mainly for data wrangling & plotting/mapping
if (!requireNamespace("tidyterra", quietly = TRUE)) {
  install.packages("tidyterra")
}
require(tidyterra)


#ESIIL Macrophenology group - for saving 
main.dir = "G:/Shared drives/ESIIL_Macrophenology/Across_sp_SDM"
graph.dir = file.path(main.dir, "Data/Graphs")
# L2 = file.path(main.dir, "Data/L2")

#Source file directories
source(file.path(getwd(), "File_Directories.R"))
#Phenology data with elongated sp list (no iDigBio)
load(file = file.path(L1, "OpenFlowers_NPN_Herb_CommSpecies_filtered.RData"), verbose = TRUE)
#conus 
conus = vect(file.path(L2, "conus.shp"))
#adjusting convention for domain id values 
values(conus)$DomainID <- sprintf("D%02d", #D is the prefix, %02 formats num as integer with at least 2 digits, padded with 0 on leftif needed
                                  values(conus)$DomainID)



##############################
#Summarizing flowering events 
##############################

#Backing up data
phe1 = phe.h.fin
# range = 5 #the day of year SE -- i.e., full range = range*2
qHigh = 0.84; qMid = 0.5; qLow = 0.16 #emperical distribution to capture center 68% data 
#--------------------------------------
#calculating windows 
df = phe1 %>%
  filter(phenophase_status==1) %>% #historic time period of interest
  #want a window to account for regional and annual variation 
  group_by(DomainID, species)%>% #site_id, individual_id, removed, not populated for every row
  #calculating windows 
  summarize(windowStart = min(day_of_year), #flower start
            windowEnd= max(day_of_year), #flower end
            # windowMid = min(day_of_year) + ((windowEnd-windowStart) / 2), #average flower (peak)
            windowMid = round(mean(day_of_year)), #mean flower -- added
            # windowMed = round(median(day_of_year)), #median flower -- removed 
            quantPeak = round(quantile(day_of_year, probs = qMid, na.rm = TRUE)), #50% quantile -- added
            quantLow = round(quantile(day_of_year, probs = qLow, na.rm = TRUE)), #finding lower quantile -- added
            quantHigh = round(quantile(day_of_year, probs = qHigh, na.rm = TRUE)), #finding lower quantile -- added
            duration = (windowEnd-windowStart), #flower duration 
            .groups = "drop") %>% # prevents over-grouping issues downstream -- added
  # Adjustable window -- added
  mutate(
    # range_days = range,  #You can make this a parameter or variable
    # peakMin = pmax(1, round(qLow)),  # lower bound of the window - makes sure range stays within the num bounds -- added 
    # peakMax = pmin(365, round(qHigh)), # upper bound of the window - makes sure range stays within the num bounds -- added
    # midMin = pmax(1, windowMid - range_days),  # lower bound of the window - makes sure range stays within the num bounds 
    # midMax = pmin(365, windowMid + range_days), # upper bound of the window - makes sure range stays within the num bounds 
    # medMin = pmax(1, windowMed - range_days),  # lower bound of the window - makes sure range stays within the num bounds 
    # medMax = pmin(365, windowMed + range_days), # upper bound of the window - makes sure range stays within the num bounds 
    tag_yr = paste(DomainID, "_", species))


#Save this summary
write.csv(df, file = file.path(L1, "Species_FlowerPeriods_per_Domain.csv"), 
          row.names = FALSE)


#save species list -- for the for loops 
species_list <- sort(unique(df$species))


##########################
#Species line plots by domain
###########################
# Start the PDF device
pdf(file = file.path(graph.dir, "Species_FlowerPeriods.pdf"), 
    width = 8, height = 6) # Adjust height as needed
#for loop plotting the flowering period for each species across domains observed
for(spec in species_list){ #START of for loop
  #filtering based on species  
  p <- df %>%
    filter(species == spec) %>%
    
    #plotting historic flowering across all years 
    ggplot(aes(y = species)) +
    
    #Full flowering window (start to end)
    geom_errorbar(aes(xmin = windowStart, xmax = windowEnd), 
                  width = 0.2, alpha = 0.8, color = "black") +
    
    #Optional: mark the exact midpoint
    geom_point(aes(x = quantPeak), 
               shape = 3, color = "palevioletred") +
    
    #Peak flowering window (windowMin to windowMax)
    geom_errorbar(aes(xmin = quantLow, xmax = quantHigh), 
                  width = 0.2, size = 1, alpha = 0.5, color = "palevioletred") +
    
    
    labs(title = paste(spec, ":", "Observed Flowering Windows historic (1910-1950)"),
         subtitle = "Pink: Observed Peak flower range; Black: Observed flower range",
         x = "Day of Year",
         y = NULL) +
    theme(axis.text.y = element_blank())+
    facet_wrap(~DomainID)
  
   print(p)
   
  # Save the plot in the specified directory
    # ggsave(file.path(graph.dir, paste0("plot_",
    #               gsub(" ", "_", spec), #grep-like function to include the specific species name & adding a "_" in any spaces
    #               "_FlowerPeriods",
    #               ".png")), last_plot(), #png file type
    #        width = 8, units = "in", dpi = 600) #dpi the resolution,

 
} #END of for loop

#close pdf device
dev.off()


##########################
#Species peak flowering maps
###########################
#Start the PDF device
pdf(file = file.path(graph.dir, "Species_PeakFlower.pdf"), 
    width = 8, height = 6) # Adjust height as needed

#for loop mapping peak flowering period for each species across domains 
for (spec in species_list) { #START of for loop
  df_spec <- df %>% filter(species == spec)
  
  
  v_spec <- merge(conus, df_spec, by = "DomainID")
  
  
  #plotting
  p = ggplot() + 
    #CONUS map layer
    geom_spatvector(data = conus, fill=NA, color='grey30')+ 
    # Overlay peak DOY data
    geom_spatvector(data = v_spec, aes(fill = as.integer(quantPeak))) +
    
    #custom color scale and legend title  
    scale_fill_viridis_c(name = "Mean Peak DOY") +
    
    #title 
    ggtitle(paste("Historical Peak DOY -", spec)) +
    
    theme_classic() + 
    
    theme(legend.position.inside = c(0.85, 0.2), color = "Peak DOY", 
          legend.background = element_rect(color = "black", linewidth = 0.5))
  
  print(p)
  
  # # Save the plot in the specified directory
  # ggsave(file.path(graph.dir, paste0("map_",
  #                                    gsub(" ", "_", spec), #grep-like function to include the specific species name & adding a "_" in any spaces
  #                                    "_PeakFlower",
  #                                    ".png")), last_plot(), #png file type
  #        width = 8, units = "in", dpi = 600) #dpi the resolution,
  
} #END of for loop


#Close pdf device 
dev.off()



##########################
#Species peak flowering maps
###########################
#Start the PDF device
pdf(file = file.path(graph.dir, "Species_FlowerPeriods_DensityPlots.pdf"), 
    width = 8, height = 6) # Adjust height as needed

#for loop mapping peak flowering period for each species across domains 
for (spec in species_list) { #START of for loop
  phe1_spec <- phe1 %>% filter(species == spec)
  
  quantiles <- quantile(phe1_spec$day_of_year, probs = c(qLow, qMid, qHigh), na.rm = TRUE)

  # Add mean line
  p <- ggplot(phe1_spec, aes(x = as.integer(day_of_year))) + 
      geom_density(color = "violetred", fill = "palevioletred") +
      geom_vline(xintercept = quantiles[2], color = "black", linetype = "dashed", size = 1) +
      geom_vline(xintercept = quantiles[1], color = "blue", linetype = "dashed", size = 1) +
      geom_vline(xintercept = quantiles[3], color = "black", linetype = "dashed", size = 1) + 
      facet_wrap(~DomainID)
  
  print(p)
      
} #END of for loop


#Close pdf device 
dev.off()

