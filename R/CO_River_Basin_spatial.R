
#read in states that fall within the CO river basin
AZ=NAMCr::query('samples',
                usStateId=4)
CO=NAMCr::query('samples',
                usStateId=6)
UT=NAMCr::query('samples',
                usStateId=45)
WY=NAMCr::query('samples',
                usStateId=51)
NV=NAMCr::query('samples',
                usStateId=34)
CA=NAMCr::query('samples',
                usStateId=5)

library(sf)
CO_Riv_samps=rbind(AZ,CA,CO,NV,UT,WY)
CO_Riv_samps=st_as_sf(CO_Riv_samps,coords=c('siteLongitude','siteLatitude'),crs=4326)
#read in HUCs. These have already been clipped to the CO river basin area!
HUCs=st_read("C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC WATS Department Files//GIS//GIS_Stats//CONUS//streams//HUC//CO_River_Basin//CO_River_Basin_HUC10s.shp")

#make them a projected crs for speed
HUCs=st_transform(HUCs,crs=102008)
CO_Riv_samps=st_transform(CO_Riv_samps,st_crs(HUCs))
#spatial join of points and the HUCs
CO_Riv_samps_sub=st_intersection(CO_Riv_samps,HUCs)

#create a year column so we can look at year ranges
CO_Riv_samps_sub$Year=lubridate::year(CO_Riv_samps_sub$sampleDate)
#dropping odd blanks from early samples or imported data
CO_Riv_samps_sub=CO_Riv_samps_sub[!is.na(CO_Riv_samps_sub$ecosystemId),]
CO_Riv_samps_sub=CO_Riv_samps_sub[!is.na(CO_Riv_samps_sub$Year),]
#summarize the data
library(tidyverse)
summ_tab=CO_Riv_samps_sub %>%
  st_drop_geometry() %>%
  group_by(HUC10, customerName) %>%
  summarise(N_samples=n(),
            N_Benthic=sum(sampleType=='Benthic'),
            N_Drift=sum(sampleType=='Drift'),
            N_Diets=sum(sampleType=='Fish Diet'),
            N_Zoop=sum(sampleType=='Zooplankton'),
            N_sites=length(unique(siteId)),
            Timespan=ifelse(length(unique(Year))==1,
                                   paste0(Year),
                                   paste0(min(Year), ' to ', max(Year))))




write.csv(summ_tab,'C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC//Misc-requests//CO_River_Basin_samples.csv')
