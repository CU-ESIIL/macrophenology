#Focal species filter 
#Author: Liz Amador

#The first half of this script explores the common species overlap across 
#NPN, and Herbarium data sources. The second half will filter for the 
#focal species decided via conversation. 


#Libraries 
#if statement to automatically install libraries if absent in r library
#tidyverse - mainly for data wrangling & plotting/mapping
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

#Source file directories
source(file.path(getwd(), "File_Directories.R"))

#Read in dataframe object
phe = read.csv(file.path(L1, "OpenFlowers_NEON_NPN_Herb.csv"), header = TRUE)

#neon
npn.spec.list = phe %>%
  filter(data_name %in% c("USA-NPN", "NEON"), phenophase_status == 1) %>% #active phenophase status 
  select(species, species_id, common_name) %>%
  distinct() #unique species 
#herbarium
herb.spec.list = phe %>%
  filter(data_name == "Herbarium", phenophase_status == 1) %>% #active phenophase status 
  select(species, species_id, common_name) %>% ##Liz: try only species field
  distinct() #unique species 

#look at dimensions - all should have similar dimension but herb has NA's (will be filled in this script)
dim(npn.spec.list); dim(herb.spec.list)


#Check the intersecting species names
#overlap between NPN & herbarium (just to compare with neon)
length(intersect(npn.spec.list$species, herb.spec.list$species))


#Neon-Npn & herbarium
species.overlap = intersect(npn.spec.list$species, herb.spec.list$species)
length(species.overlap)
#Make into dataframe
species.overlap = as.data.frame(species.overlap) 
#rename column
names(species.overlap) <- "species"
#sort by alphabetical order - nice to see
species.overlap$species = sort(species.overlap$species) 


#Adding native status information 
#From the USDA search download - https://plants.usda.gov/noxious-invasive-search
usda.invasive = read.csv(file.path(data.dir, "USDA_invasive_status.csv"), header = TRUE, skip = 13)
#Simplifying the status
usda.invasive$inv_status = ifelse(grepl("invasive", 
                                        tolower(usda.invasive$Invasive.Noxious.Status)), 
                                  "invasive", "non-native")
#Rename columns to match main dataset 
colnames(usda.invasive)[c(4, 7)] <- c("common_name", "status") 
  
#Removing the subspecies/varation from the genus-species 
usda.invasive$species <- sapply(strsplit(usda.invasive$Scientific.Name, " "), function(x) paste(x[1:2], collapse = " "))
#Only keep necessary columns 
inv.spec <- usda.invasive %>%
  select("species", "status") %>%
  distinct()

#join the two 
species.overlap = left_join(species.overlap, inv.spec, by = "species") 

#fill in missing status info 
species.overlap$status[is.na(species.overlap$status)] <- "native?"

#save list and send to Rohit - uncomment to save
# write.csv(species.overlap, file = file.path(data.dir, "species_list_NPN_Herb.csv"), row.names = FALSE)



#Now let's add back the species_id & common_name info so we can remove uncommon 
#species from the main dataset -- should be species with & w/0 active status
comm = phe %>%
  select(species, common_name, species_id) %>% 
  distinct() #unique instance of the information
#joing the species information
comm.spec.overlap = left_join(species.overlap, comm, by = "species") 
#dimensions - length should be same as the species.overlap
dim(comm.spec.overlap)


#Let's join the main dataset to the species overlap dataframe 
#This will drop any uncommon species 
#Filtering for only the shared species
phe_comm <- phe %>%
  filter(species %in% comm.spec.overlap$species)

#checking it worked well
length(unique(phe_comm$species)) #should be the same length as comm.spec.overlap
#Checking to see if all data types made it through
unique(phe_comm$data_name)


#species sample sizes
spec.n = phe_comm %>% count(species)


#Save this to the L2 folder
write.csv(phe_comm, 
          file = file.path(L1, "OpenFlowers_NPN_Herb_CommSpecies_unfiltered.csv"), 
          row.names = FALSE)

#---------------------------------------

########################################
#parsing phe_comm by date and filtering 
########################################

#filter by year
#Historic
df.h = phe_comm %>%
  filter(phenophase_status == 1, year_rect >= 1910, year_rect <=1950)
#saving sample size of each species 
length(unique(df.h$species)); spec.h <- count(df.h, species)
#Current
df.c = phe_comm %>%
  filter(phenophase_status == 1, year_rect >= 1985)
#saving sample size of each species 
length(unique(df.c$species)); spec.c <- count(df.c, species)



#filter species with n>=50
#Historic
spec.h.fin = spec.h %>%
  filter(n >= 50)
#pull new list 
list.h = spec.h.fin$species

#Current
spec.c.fin = spec.c %>%
  filter(n >= 50)
#pull new list
list.c = spec.c.fin$species


#find the species that overlap between time periods 
common_species <- intersect(list.h, list.c)



#Remove odd ones out from main data sets
#Historic
phe.h.fin = phe_comm %>%
  filter(year_rect >= 1910, year_rect <=1950,
  species %in% common_species)
#Current
phe.c.fin = phe_comm %>%
  filter(year_rect >= 1985,
         species %in% common_species)



#Now let's save the sapecies list and their sample sizes 
spec.h.ct <- count(phe.h.fin, species)
spec.c.ct <- count(phe.c.fin, species)
#combine them side-by side - will have repeat columns, we will fix that later 
comm.samp = cbind(spec.h.ct, spec.c.ct)
#rename columns 
colnames(comm.samp) <- c("species", "hist.n", "dud", "curr.n")
#remove dud column
comm = select(comm.samp, -dud)


#Save this to the L2 folder
save(phe.h.fin, phe.c.fin, 
          file = file.path(L1, "OpenFlowers_NPN_Herb_CommSpecies_filtered.RData"))
##export
write.csv(comm, file = file.path(L1, "OpenFlowers_NPN_Herb_CommSpecies_list.csv"), 
          row.names = FALSE)












