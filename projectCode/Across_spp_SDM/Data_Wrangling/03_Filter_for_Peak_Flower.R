#Filter species by peak doy 

#Auhtor: Lizbeth G Amador 

#This script filters the phenology data for the peak flower period. This will alos narrow 
#the species list based on sample sizes and overlapping species between time periods

#Libraries 
#if statement to automatically install libraries if absent in r library
#tidyverse - mainly for data wrangling & plotting/mapping
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)


#ESIIL Macrophenology group - for saving 
main.dir = "G:/Shared drives/ESIIL_Macrophenology/Across_sp_SDM"
# L2 = file.path(main.dir, "Data/L2")

#Source file directories
source(file.path(getwd(), "File_Directories.R"))
#Phenology data with elongated sp list (no iDigBio)
load(file = file.path(L1, "OpenFlowers_NPN_Herb_CommSpecies_filtered.RData"), 
     verbose = TRUE)
#Historic flower period info 
f.per = read.csv(file = file.path(L1, "Species_FlowerPeriods_per_Domain.csv"), 
                 header = TRUE)


######################################
# 1. Join the peak flowering information by domain and species 
######################################
#join the flower period info to the main dataset 
phe.fper = left_join(phe.h.fin, f.per, by = c("species", "DomainID"))


######################################
# 2. Drop records that fall outside of peak flowering
######################################
#create column that indicate whether the records falls inside the peak range ("keep")
phe.fper$drop = ifelse(phe.fper$day_of_year >= phe.fper$quantLow & 
                         phe.fper$day_of_year <= phe.fper$quantHigh, "keep", 
                       "drop")

#viewing columns to check join & ifelse 
temp = phe.fper %>% select(species, DomainID, phenophase_status, day_of_year, quantLow, quantHigh, drop)
# #check drop column 
unique(temp$drop); table(temp$drop); sum(is.na(temp$drop)) #22 NAs
#Note: NAs are there bc the script to determine flower windows only include domains
#for species with flower presence. So, a species can still occur in a domain, but
#if there were no active flowers for that domain, the peak flower range was not 
#calculated for that species-Domain combination (also means there are no active flowers
#for a species in that domain, only absent)
#All rows with NA range values have a phenophase status of 0 


#Ignoring the NAs -- only keeping records with "keep"
phe.drop = phe.fper %>% 
  filter(drop == "keep") %>% #filter for records to "keep" 
  select(-drop) #remove drop column 


######################################
# 3. Filter species based on observations 
######################################
#look at sample size for each species (active status for a conservative approach)
drop.sp = phe.drop %>% 
  filter(phenophase_status == 1) %>%
  count(species) 
#filter for observations greater than/equal to 50 
drop.sp.drop = drop.sp %>%
  filter(n>= 50)

#remove species from df with phenology info 
phe.hd = phe.drop %>%
  filter(species %in% drop.sp.drop$species)

## species sample size (historic time period) 
length(unique(phe.hd$species))
ct.h = phe.hd %>%
  count(species)


######################################
# 4. Match species list between historic and current
######################################
# Filter for species list seen in historical time period (ct.h)  
phe.c = phe.c.fin %>%
  filter(species %in% ct.h$species)


######################################
# 5. Join the peak flowering information by domain and species  
######################################
#join the flower period info to the main dataset 
phe.c.fper = left_join(phe.c, f.per, by = c("species", "DomainID"))


######################################
# 6. Drop records that fall outside of peak flowering
######################################
#create column that indicate whether the records falls inside the peak range ("keep")
phe.c.fper$drop = ifelse(phe.c.fper$day_of_year >= phe.c.fper$quantLow & 
                         phe.c.fper$day_of_year <= phe.c.fper$quantHigh, "keep", 
                       "drop")

#viewing columns to check join & ifelse 
temp = phe.c.fper %>% select(species, DomainID, phenophase_status, day_of_year, quantLow, quantHigh, drop)
# #check drop column 
unique(temp$drop); table(temp$drop); sum(is.na(temp$drop)) #818 NAs
#Note: NAs are there bc the script to determine flower windows only include domains
#for species with flower presence. So, a species can still occur in a domain, but
#if there were no active flowers for that domain, the peak flower range was not 
#calculated for that species-Domain combination (also means there are no active flowers
#for a species in that domain, only absent)
#All rows with NA range values have a phenophase status of 0 


#Ignoring the NAs -- only keeping records with "keep"
phe.c.drop = phe.c.fper %>% 
  filter(drop == "keep") %>% #filter for records to "keep" 
  select(-drop) #remove drop column 


######################################
# 7. Filter species based on observations 
######################################
#look at sample size for each species (active status for a conservative approach)
c.drop.sp = phe.c.drop %>% 
  filter(phenophase_status == 1) %>%
  count(species) 
#filter for observations greater than/equal to 50 
c.drop.sp.drop = c.drop.sp %>%
  filter(n>= 50)

#remove species from df with phenology info 
phe.cd = phe.c.drop %>%
  filter(species %in% c.drop.sp.drop$species)

## species sample size (historic time period) 
length(unique(phe.cd$species))
ct.c = phe.cd %>%
  count(species)


######################################
# 8. Compare species list between time periods  
######################################
# Observations with presence threshold only 
#find overlapping species 
comm.sp = intersect(ct.h$species, ct.c$species)
#merge the species count info
#historic
ct.h.red = ct.h %>% 
  filter(species %in% comm.sp)
#current 
ct.c.red = ct.c %>% 
  filter(species %in% comm.sp)
#combine the species counts for both time periods 
comm.sp.ct = cbind(ct.h.red, ct.c.red)
#rename columns 
names(comm.sp.ct) <- c("species", "hist.n", "dud", "curr.n")
comm.sp.ct = select(comm.sp.ct, -dud)



######################################
# 9. Reduce the main df species list  
######################################
#Phenology df
#historic 
phe.hd.fin = filter(phe.hd, species %in% comm.sp)
#current 
phe.cd.fin = filter(phe.cd, species %in% comm.sp)
#combine the two 
phe = rbind(phe.hd.fin, phe.cd.fin)

#Occurrence df -- phenological events doesn't matter for this but need to 
#narrow species 
#historic 
rang.h = phe.h.fin %>%
  filter(species %in% comm.sp) %>%
  select(-phenophase_status) #remove phenology info 
#current 
rang.c = phe.c.fin %>%
  filter(species %in% comm.sp) %>%
  select(-phenophase_status) #remove phenology info  
#combine the two 
rang = rbind(rang.h, rang.c)

length(unique(phe$species))
######################################
# 10. Save   
######################################
#save df 
write.csv(phe, file= file.path(L2, "Species_Peak_Open_Flowers_NPN_Herb.csv"), row.names=FALSE)
#save df 
write.csv(rang, file= file.path(L2, "Species_Occurrence_NPN_Herb.csv"), row.names = FALSE)
#save species list 
write.csv(comm.sp.ct, file= file.path(L2, "Species_List.csv"), row.names = FALSE)



