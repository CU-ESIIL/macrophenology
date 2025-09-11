library(tidyverse)

wd <- "G:/Shared drives/ESIIL_Macrophenology/Across_sp_SDM/Data"

load(paste(wd, "dispersal_trait.RData", sep='/'))

dis_bien <- select(dispersal_bien, sci_name=scrubbed_species_binomial,
                   disp_type=method)
dis_bien$db <- "bien"

dis_gift <- select(dispersal_gift, sci_name=work_species, disp_type=value)
dis_gift$db <- "gift"

dis_try <- dispersal_try%>%
  #filter(DataName=="Dispersal syndrome (agent)")%>%  #drops too many species
  filter(!str_detect(tolower(OrigValueStr), "weigh|mass"))%>%
  select(sci_name=AccSpeciesName, disp_type=OrigValueStr)%>%
  filter(!sci_name%in%dis_gift$sci_name)

dis_try$db <- "try"

dis_all <- bind_rows(dis_bien, dis_gift, dis_try)

dis_all <- arrange(dis_all, sci_name)

dis_all <- dis_all%>%
  mutate(disp_type = if_else(str_detect(tolower(disp_type), "wind"),
                             "anemochory",
                             disp_type))
dis_sum <- dis_all%>%
  group_by(sci_name, db, disp_type)%>%
  summarise(count=n())%>%
  group_by(sci_name)%>%
  filter(count==max(count))

write.csv(dis_sum, paste(wd, 'dispersal_summary.csv', sep="/"), 
                     row.names=F)
