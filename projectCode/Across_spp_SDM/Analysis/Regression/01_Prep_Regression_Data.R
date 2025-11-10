#Prep Data for Regression
#Author: Lizbeth G Amador

#This script merges the ensemble projection differences data for phenology and
#range. Descriptive information is added and outliers are removed.


############
# Presets
############
suppressPackageStartupMessages({
  if (!requireNamespace("tidyverse", quietly = TRUE)) {install.packages("tidyverse")}; library(tidyverse)
  if (!requireNamespace("terra", quietly = TRUE)) { install.packages("terra")}; library(terra)
  if (!requireNamespace("dggridR", quietly = TRUE)) {install.packages("dggridR")}; library(dggridR)
})

##############
# Directories
#############
mwd = "~/data-store/home/lamador/Data"
L2 = file.path(mwd, "L2")
graphs = file.path(L2, "Graphs")

#########
# Data
#########
#Read the data
rang = read.csv(file.path(L2, "AllSpecies_EMProj_diff_Range.csv"), header = TRUE)
phe = read.csv(file.path(L2, "AllSpecies_EMProj_diff_Phenology.csv"), header = TRUE)

#Clean up the data
setdiff(unique(rang$species), unique(phe$species))
#save the species in common
comm.sp = intersect(unique(rang$species), unique(phe$species))

#filter for common species overlap
rang.comm.sp = rang %>%
  filter(species %in% comm.sp) %>%
  rename(rdiff = diff_val) %>%
  arrange(species, latitude, longitude)
phe.comm.sp = phe %>%
  filter(species %in% comm.sp) %>%
  rename(pdiff = diff_val) %>%
  arrange(species, latitude, longitude)
#free up space
rm(phe, rang); gc()

#Merge the two (whichiver one ihas more rows will be first)
all.data = left_join(rang.comm.sp, phe.comm.sp, by = c("latitude", "longitude", "species"))
all.data = all.data[!is.na(all.data$rdiff),]
#free up space
rm(rang.comm.sp, phe.comm.sp); gc()

################
# Species info
###############
#Add more species information
sp.info = read.csv(file.path(L2, "Focal_Species_List_Information.csv"), header = TRUE)
#phenology list filter by presence threshold only
all.data = left_join(all.data, sp.info, by = "species")

#Save summary info
sink(file= file.path(graphs, "All_Species_Info_Tables_Range_Phenology.txt"))
a = all.data %>% count(status, functional_type)
print(a)

b = all.data %>%
  group_by(species, functional_type, status) %>%
  summarise(n())
print(b, n = 1654)

table(all.data$status)
table(all.data$functional_type)
table(all.data$genus_common_name)
table(all.data$species)

unique(all.data$species)
sink()

#save object
# save(all.data, file = file.path(L2,"All_Species_Data_Range_Phenology.RData"))
# load(file = "~/esiil_macrophenology/Data/L2/All_Species_Data_Range_Phenology.RData", verbose = TRUE)
# load(file = "All_Species_Data_Range_Phenology.RData", verbose = TRUE)


#########################
# Regression Assumptions
##########################
#Remove outliers by species
model.data.clean <- all.data %>%
  filter(!is.na(pdiff)) %>%
  group_by(species) %>%
  mutate(
    Q1 = quantile(pdiff, 0.25, na.rm = TRUE),
    Q3 = quantile(pdiff, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower = Q1 - 1.5 * IQR,
    upper = Q3 + 1.5 * IQR,
    status = factor(status),
    functional_type = factor(functional_type)
  ) %>%
  filter(pdiff >= lower & pdiff <= upper) %>%
  ungroup() %>%
  select(-Q1, -Q3, -IQR, -lower, -upper)  #clean up helper columns

#remove outliers by status
model.data.no_outliers <- model.data.clean %>%
  group_by(status) %>%
  mutate(
    Q1 = quantile(pdiff, 0.25, na.rm = TRUE),
    Q3 = quantile(pdiff, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower_bound = Q1 - 1.5 * IQR,
    upper_bound = Q3 + 1.5 * IQR
  ) %>%
  filter(pdiff >= lower_bound & pdiff <= upper_bound) %>%
  ungroup() %>%
  select(-Q1, -Q3, -IQR, -lower_bound, -upper_bound)  # optional cleanup

# boxplot(model.data.no_outliers$pdiff ~ model.data.no_outliers$status)
#
# #remove outliers by functional type
model.data.no_outliers1 <- model.data.no_outliers %>%
  group_by(functional_type) %>%
  mutate(
    Q1 = quantile(pdiff, 0.25, na.rm = TRUE),
    Q3 = quantile(pdiff, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower_bound = Q1 - 1.5 * IQR,
    upper_bound = Q3 + 1.5 * IQR
  ) %>%
  filter(pdiff >= lower_bound & pdiff <= upper_bound) %>%
  ungroup() %>%
  select(-Q1, -Q3, -IQR, -lower_bound, -upper_bound)  # optional cleanup

# pdf(file.path(graphs, "All_Data_ModelData_NoOut_Plots.pdf"))
# boxplot(model.data.no_outliers1$pdiff ~ model.data.no_outliers1$functional_type)
# boxplot(model.data.no_outliers1$pdiff ~ model.data.no_outliers1$status)
# hist(model.data.no_outliers1$pdiff)
# hist(model.data.no_outliers1$rdiff)
# hist(model.data.no_outliers1$latitude)
#
# hist(log(model.data.no_outliers1$latitude))
# hist((model.data.no_outliers1$latitude)^2)
# hist((model.data.no_outliers1$latitude)^3)
# dev.off()
#
model.data.no_outliers1$lat.sq = model.data.no_outliers1$latitude^2
model.data = model.data.no_outliers1
# save(model.data, file=file.path(L2, "All_Data_ModelData_NoOutliers.RData"))
# load(file = file.path(L2, "All_Data_ModelData_NoOutliers.RData"), verbose = TRUE)


###############
# NEON Domains
###############
#Add NEON Domain information
domains = vect(file.path(L2, "conus.shp"))
v = vect(model.data, geom = c("longitude", "latitude"), keep = TRUE, crs <- crs(domains))
#free up space
rm(all.data); gc()
#intersect to get domain information
v.domain = intersect(v, domains)
#Free up space
rm(v, v.domain); gc()
#turn back into data frame
model.data.d = as.data.frame(v.domain)
# names(model.data.d)
#remove columns
model.data.d = model.data.d %>%
  select(-c("OBJECTID", "Shape_Leng", "DomainName", "Shape_Le_1", "Shape_Area")) %>%
  rename(domain_id = DomainID)

save(model.data.d, file = file.path(L2, "ModelData_NoOutliers.RData"))
# load(file = file.path(L2, "ModelData_NoOutliers.RData"), verbose = TRUE)


#################
#Subsampling
#################
#For computational ease we are going to take a sample from our main dataset.
#We will be using the best practices workflow from the eBird best practices workshop
#https://strimas.com/ebp-workshop/subsampling.html
#https://cran.r-project.org/web/packages/dggridR/vignettes/dggridR.html

#randomly subset data
r = 11
#generate hexagonal grid
dggs <- dgconstruct(res = r)
#> Resolution: 13, Area (km^2): 31.9926151554038, Spacing (km): 5.58632116604266, CLS (km): 6.38233997895802
#read in data
mod.data.cell <- model.data.d %>%
  #get hexagonal cell id and week number for each checklist
  mutate(cell = dgGEO_to_SEQNUM(dggs, longitude, latitude)$seqnum)
#sample from each instance
set.seed(13)
sub.data <- mod.data.cell %>%
  group_by(species, domain_id, cell) %>%
  sample_n(size = 1) %>%
  ungroup()
#subset data dimensions
dim(sub.data)
save(sub.data, file=file.path(L2, "Subset_ModelData_NoOutliers.RData"))
# load(file = file.path(L2, "Subset_ModelData_NoOutliers.RData"), verbose = TRUE)


#Save summary info
sink(file= file.path(L2, "All_Species_Info_Tables_Range_Phenology_ModelData_Subset.txt"))
print(paste0("resolution: ", r))
cat("\n")
str(sub.data)
cat("\n range of psd\n")
range(sub.data$psd)
cat("\n range of rsd\n")
range(sub.data$rsd)
cat("\n")
unique(sub.data$species)
unique(sub.data$domain_id)
unique(sub.data$dispersal)
unique(sub.data$functional_type)

a = sub.data %>% count(nativity, funct_type, dispersal)
print(a)
cat("\n")
table(sub.data$nativity)
table(sub.data$funct_type)
table(sub.data$dispersal)

cat("\n number of species")
c = sub.data %>%
  group_by(species, nativity, funct_type, dispersal) %>%
  summarise(n())
cat("\n")
table(c$nativity)
table(c$funct_type)
table(c$dispersal)
cat("\n")
c1 = c %>%
  group_by(nativity, funct_type, dispersal) %>%
  summarise(n())
print(c1)
cat("\n")
c1 = c %>%
  group_by(nativity, funct_type) %>%
  summarise(n())
print(c1)
cat("\n")
c1 = c %>%
  group_by(funct_type, dispersal) %>%
  summarise(n())
print(c1)
cat("\n")
c1 = c %>%
  group_by(nativity, dispersal) %>%
  summarise(n())
print(c1)
cat("\n")
table(sub.data$genus_common_name)
table(sub.data$species)
cat("\n")
cat("\n")
b = sub.data %>%
  group_by(species, nativity, funct_type, dispersal, domain_id) %>%
  summarise(n())
print(b, n = length(b$species))

sink()



#################
#Adjacency table
#################
# domains = vect(file.path(L2, "conus.shp"))
# #Build adjacency table for the NEON domains
# #Load domain polygons (assuming already read into 'domains')
# domains <- makeValid(domains)
# #Merge (dissolve) polygons by DomainID -- This ensures one polygon per domain
# # domains <- aggregate(domains, by = "DomainID", fun = first)
# domains <- aggregate(domains, by = "DomainID", fun = mean, na.rm = TRUE)
# #Determine touching relationships (adjacency)
# adj_mat <- relate(domains, domains, relation = "touches")
# #Convert logical matrix to binary matrix (0/1)
# adj_binary <- as.matrix(adj_mat) * 1
# #Use DomainID as row/column names (now unique)
# rownames(adj_binary) <- colnames(adj_binary) <- domains$DomainID
# #Manually enter touch boundary for some missing ones
# #List of domain ID pairs to force as neighbors
# adj_pairs <- list(
#   c("2", "3"), c("3", "11"), c("6", "9"), c("6", "10"), c("8", "10"), c("9", "5"),
#   c("9", "6"), c("10", "6"), c("10", "8"), c("10", "11"), c("11", "3"), c("11", "10"),
#   c("11", "13"), c("16", "15"), c("16", "17")
# )
# #Add symmetric adjacency (both directions)
# for (pair in adj_pairs) {
#   i <- pair[1]
#   j <- pair[2]
#   adj_binary[i, j] <- 1
#   adj_binary[j, i] <- 1
# }
# #Save to CSV
# write.csv(adj_binary, file.path(L2, "neon_adjacency_matrix.csv"), row.names = TRUE)

