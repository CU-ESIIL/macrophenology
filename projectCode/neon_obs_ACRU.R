library(tidyverse)
library(neonUtilities)

dat <- loadByProduct(dpID="DP1.10055.001",
                     site = "HARV",
                     release = "LATEST",
                     check.size = FALSE,
                     token = Sys.getenv('latestTok'))

ind <- dat$phe_perindividual
obs <- dat$phe_statusintensity

acruTags <- filter(ind, taxonID=="ACRU")%>%
                   select(domainID, siteID, plotID, subtypeSpecification,
                          decimalLatitude, decimalLongitude,
                          sampleLatitude, sampleLongitude, taxonID, scientificName,
                          nativeStatusCode, individualID, growthForm)

acruObs <- filter(obs, individualID%in%acruTags$individualID &
                    phenophaseName=="Leaves")

acruAll <- left_join(acruTags, acruObs, by='individualID')


