#iDigBio Data Merge 
#Auhtor: Liz Amador 


#This script takes the merged species occurrence and multimedia (no NA) data, and joins 
# the presence/absence information from the annotated species media images 
#Then it merges all species data together 

#Read function to automate the joining process 
source("08a_idigbio.annot.clean.R")

#We need an output file directory for exporting final data frame 
out.path = "/Data/L1" #folder path 

#So we need to read in two data sets for the same species

#1. merged occurrence and multimedia (no NA) data
# Acer
ar.m = read.csv("Data/L0/iDigBio/03_merged_occur_multimed_NoNA/merged_acer_rubrum_nona.csv")
ap.m = read.csv("Data/L0/iDigBio/03_merged_occur_multimed_NoNA/merged_acer_platanoides_nona.csv")
# Ilex
id.m = read.csv("Data/L0/iDigBio/03_merged_occur_multimed_NoNA/merged_ilex_decidua_nona.csv")
ia.m = read.csv("Data/L0/iDigBio/03_merged_occur_multimed_NoNA/merged_ilex_aquifolium_nona.csv")
# Juglans 
jn.m = read.csv("Data/L0/iDigBio/03_merged_occur_multimed_NoNA/merged_juglans_nigra_nona.csv")
jr.m = read.csv("Data/L0/iDigBio/03_merged_occur_multimed_NoNA/merged_juglans_regia_nona.csv")
#Rubus
rs.m = read.csv("Data/L0/iDigBio/03_merged_occur_multimed_NoNA/merged_rubus_spectabilis_nona.csv")
rl.m = read.csv("Data/L0/iDigBio/03_merged_occur_multimed_NoNA/merged_rubus_laciniatus_nona.csv")

#2. Anotation data 
# Acer
ar.a = read.csv("Data/L0/iDigBio/05_imageant_annotations/acer_rubrum.csv")
ap.a = read.csv("Data/L0/iDigBio/05_imageant_annotations/acer_platanoides.csv")
# Ilex
id.a = read.csv("Data/L0/iDigBio/05_imageant_annotations/ilex_decidua.csv")
ia.a = read.csv("Data/L0/iDigBio/05_imageant_annotations/ilex_aquifolium.csv")
# Juglans 
jn.a = read.csv("Data/L0/iDigBio/05_imageant_annotations/juglans_nigra.csv")
jr.a = read.csv("Data/L0/iDigBio/05_imageant_annotations/juglans_regia.csv")
#Rubus
rs.a = read.csv("Data/L0/iDigBio/05_imageant_annotations/rubus_spectabilis.csv")
rl.a = read.csv("Data/L0/iDigBio/05_imageant_annotations/rubus_laciniatus.csv")


############################
#rename species value so it follows correct notation 
ar.m$gbif.canonicalName <- "Acer rubrum"
ap.m$gbif.canonicalName <- "Acer platanoides"
id.m$gbif.canonicalName <- "Ilex decidua"
ia.m$gbif.canonicalName <- "Ilex aquifolium"
jn.m$gbif.canonicalName <- "Juglans nigra"
jr.m$gbif.canonicalName <- "Juglans regia"
rs.m$gbif.canonicalName <- "Rubus spectabilis"
rl.m$gbif.canonicalName <- "Rubus laciniatus"

############################
#apply native status  
ar.m$native_status <- "native"
ap.m$native_status <- "non-native"
id.m$native_status <- "native"
ia.m$native_status <- "non-native"
jn.m$native_status <- "native"
jr.m$native_status <- "non-native"
rs.m$native_status <- "native"
rl.m$native_status <- "non-native"

#############################
#Applying function 
#Acer
sp.ar = idigbio.annot.clean(ar.m, ar.a, out.path)
sp.ap = idigbio.annot.clean(ap.m, ap.a, out.path)
#Ilex
sp.id = idigbio.annot.clean(id.m, id.a, out.path)
sp.ia = idigbio.annot.clean(ia.m, ia.a, out.path)
#Juglans
sp.jn = idigbio.annot.clean(jn.m, jn.a, out.path)
sp.jr = idigbio.annot.clean(jr.m, jr.a, out.path)
#Rubus
sp.rs = idigbio.annot.clean(rs.m, rs.a, out.path)
sp.rl = idigbio.annot.clean(rl.m, rl.a, out.path)

##################################
#Combine all species data 
idigbio = rbind(sp.ar, sp.ap, sp.ia, sp.id, sp.jn, sp.jr, sp.rl, sp.rs)

#Export file 
write.csv(idigbio, file = file.path(out.path, "FocalSpecies_OpenFlowers_PA_noNA_iDigBio_processed.csv"), row.names = FALSE)

