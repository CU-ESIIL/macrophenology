library(tidyverse)
library(neonUtilities)

source(paste0(getwd(), "/projectCode/Across_spp_SDM/Data_Wrangling/kj_windows/estPheDuration_macrophenologyModified.r"))


# Get data ----------------------------------------------------------------


#NEON portal
pheDat <- loadByProduct(
  dpID = "DP1.10055.001",
  site = "HARV",
  #startdate = "2021-01",
  #enddate = "2024-12",
  package = "basic",
  include.provisional = T,
  check.size = FALSE)

# all sources
# downloaded from cyverse - update filepath
df <- read.csv('C:/Users/kjones/Downloads/FocalSpp_OpenFlowers_NeonNpnHerb_iDigBio.csv',
               stringsAsFactors = F)


# Using NEON portal download ----------------------------------------------

t <- estimatePheTransByTag(pheDat)

t2 <- t%>%
  filter(taxonID=="ACRU" & phenophaseName=="Open flowers" &
           !transitionType%in%c("onset", "NA-no", "no-no", "NA-yes",
                                "no-missed", "missed-no", "missed-yes",
                                "yes-missed"))%>%
  group_by(siteID, taxonID, year, individualID, nthTransition)%>%
  summarize(windowStart = min(doyIntervalEnd), windowEnd=max(doyIntervalEnd),
            windowMid = min(doyIntervalEnd)+((windowEnd-windowStart)/2),
            duration=(windowEnd-windowStart))%>%
  group_by(siteID, taxonID, year, individualID)%>%
  filter(duration>3)%>%
  mutate(tag_yr = paste(year, "_", individualID))


neon <- ggplot(t2, aes(y = tag_yr, color=year)) +
  geom_errorbar(aes(xmin = windowStart, xmax = windowEnd), width = 0.2) +
  labs(title = "NEON Flowering Windows HARV - ACRU",
                       x = "dayOfYear",
                       y = "year x tag") +
  theme(axis.text.y = element_blank())

window_summary <- t2 %>%
  group_by(year, taxonID) %>%
  summarize(
    n_individuals = n_distinct(individualID), # Number of unique individuals in each year
    min_start = min(windowStart, na.rm = TRUE),       # Earliest window start
    max_start = max(windowStart, na.rm = TRUE),       # Latest window start
    earliest_end = min(windowEnd, na.rm = TRUE),         # Earliest window end
    latest_end = max(windowEnd, na.rm = TRUE),           # Latest window end
    mean_start = mean(windowStart, na.rm = TRUE),      # Average window start
    mean_end = mean(windowEnd, na.rm = TRUE),        # Average window end
    median_start = median(windowStart, na.rm = TRUE),  # Median window start
    median_end = median(windowEnd, na.rm = TRUE),    # Median window end
    range_start = diff(range(windowStart, na.rm = TRUE)), # Range of window starts
    range_end = diff(range(windowEnd, na.rm = TRUE)),     # Range of window ends
  )

write.csv(t2, paste0(getwd(),'/projectCode/Across_spp_SDM/Data_Wrangling/kj_windows/neon_acru_windows.csv'),
          row.names=F)

write.csv(window_summary, paste0(getwd(),'/projectCode/Across_spp_SDM/Data_Wrangling/kj_windows/neon_acru_summary.csv'),
          row.names=F)

ggsave(paste0(getwd(),'/projectCode/Across_spp_SDM/Data_Wrangling/kj_windows/neon_acru_windows.png'), plot=neon)



# Using FocalSpp_OpenFlowers_NeonNpnHerb_iDigBio --------------------------

df <- read.csv('C:/Users/kjones/Downloads/FocalSpp_OpenFlowers_NeonNpnHerb_iDigBio.csv',
               +                stringsAsFactors = F)
df2 <- df%>%
  filter(phenophase_status==1, species%in%c("Acer rubrum", "Juglans nigra",
                                            "Ilex decidua", "Rubus laciniatus",
                                            "Rubus spectabilis", "Acer platanoides"))%>%
  group_by(state, species, data_name,  phenophase_description,year_rect, )%>% #DomainID, site_id, individual_id, removed, not populated for every row
  summarize(windowStart = min(day_of_year), windowEnd=max(day_of_year),
            windowMid = min(day_of_year)+((windowEnd-windowStart)/2),
            duration=(windowEnd-windowStart))%>%
  mutate(tag_yr = paste(year_rect, "_", state))

historic <- ggplot(df2[df2$year_rect>1910 & df2$year_rect<1950,], aes(y = year_rect,color=year_rect)) +
  geom_errorbar(aes(xmin = windowStart, xmax = windowEnd), width = 0.2) +
  labs(title = "Flowering Windows historic (1910-1950)",
       x = "day_of_year",
       y = "year_rect") +
  theme(axis.text.y = element_blank())+
  facet_wrap(~species)

current <- ggplot(df2[df2$year_rect>1985,], aes(y = year_rect, color=year_rect)) +
  geom_errorbar(aes(xmin = windowStart, xmax = windowEnd), width = 0.2) +
  labs(title = "Flowering Windows current (1985-2025)",
       x = "day_of_year",
       y = "year_rect") +
  theme(axis.text.y = element_blank())+
  facet_wrap(~species)

window_summar_allSources <- df2 %>%
  group_by(year_rect, species) %>%  #needs a spatial grouping variable to be meaningful
                                    # domain, site, state...
  summarize(
    n_recs = n(), # Count of records in year/sp subset
    min_start = min(windowStart, na.rm = TRUE),       # Earliest window start
    max_start = max(windowStart, na.rm = TRUE),       # Latest window start
    earliest_end = min(windowEnd, na.rm = TRUE),         # Earliest window end
    latest_end = max(windowEnd, na.rm = TRUE),           # Latest window end
    mean_start = mean(windowStart, na.rm = TRUE),      # Average window start
    mean_end = mean(windowEnd, na.rm = TRUE),        # Average window end
    median_start = median(windowStart, na.rm = TRUE),  # Median window start
    median_end = median(windowEnd, na.rm = TRUE),    # Median window end
    range_start = diff(range(windowStart, na.rm = TRUE)), # Range of window starts
    range_end = diff(range(windowEnd, na.rm = TRUE)),     # Range of window ends
  )




write.csv(df2, paste0(getwd(),'/projectCode/Across_spp_SDM/Data_Wrangling/kj_windows/allSources_flowering_windows.csv'),
          row.names=F)

write.csv(window_summar_allSources, paste0(getwd(),'/projectCode/Across_spp_SDM/Data_Wrangling/kj_windows/allSources_flowering_summary.csv'),
          row.names=F)

ggsave(paste0(getwd(),'/projectCode/Across_spp_SDM/Data_Wrangling/kj_windows/allSources_historic_windows.png'), plot=historic)

ggsave(paste0(getwd(),'/projectCode/Across_spp_SDM/Data_Wrangling/kj_windows/allSources_current_windows.png'), plot=current)
