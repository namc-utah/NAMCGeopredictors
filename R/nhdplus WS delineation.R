~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Name: Quick Watershed Delineation using NHDPlusTools
  # Coder: Nate Jones (cnjones7@ua.edu),
  # adapted by NAMC staff
  # Date: 8/114/2023
  # Purpose: Watershed Delineation using NHDPlust Tools
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 1.0 Setup Environment --------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#clear memory
rm(list=ls())
boxId<-5745
#Call relevant libraries
library(tidyverse)
library(mapview)
library(raster)
library(sf)
library(tigris)
library(nhdplusTools) #this was not loaded originally
library(NAMCr)
pred_geometry_base_path="C://Users//andrew.caudillo//Box//NAMC//"
watershed_file_path=paste0(pred_geometry_base_path,"GIS//Watersheds//Mastersheds//mastersheds.shp")
mastershed<-st_read(watershed_file_path)


samps<-NAMCr::query(
  api_endpoint = "samples",
  boxId=boxId
)

missings_sheds<-samps$siteId[samps$siteId %in% mastershed$siteId==F]
missings_sheds
samps_For_sheds<-samps[samps$siteId %in% missings_sheds,]
#Load state shape
bama <- states() %>% dplyr::filter((STUSPS %in% c('WY')))

samps_For_sheds_Coords<-samps_For_sheds[,c("sampleLatitude","sampleLongitude")]
#making df of points

outlet<-sf::st_as_sf(samps_For_sheds_Coords, coords = c("sampleLongitude","sampleLatitude"),crs=4269)


#Rerpoject and clip to continental US
bama <- st_transform(bama, 5070)
outlet <- st_transform(outlet, 5070)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Delineate ----------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create starting point
shed_list<-st_sfc(crs=5070)


for(i in 1:nrow(outlet)){
  print(i)
start_comid <- discover_nhdplus_id(outlet$geometry[i])
start_comid

#Snag flowline
flowlines <- navigate_nldi(list(featureSource = "comid",
                                featureID = start_comid),
                           mode = "upstreamTributaries",
                           distance_km = 1000)

#plot for funzies
#mapview(cahaba_outlet) + mapview(flowlines)

# Note, the next step pulls NHDplus data from NLNDI server. For large watersheds,
# it will likely be faster to download the WBD form the USGS National Map website (link below),
# and intersect the "fowlines" shape above with the downloaded layer.
# https://www.usgs.gov/national-hydrography/access-national-hydrography-products

#get nhdplus files
subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowlines$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download",
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)
#Snag catchment files
catchment <- sf::read_sf(subset_file, "CatchmentSP")

#Dissovle files to single catchment
catchment <- sf::st_union(catchment)
ouch<-sfheaders::sf_remove_holes(catchment)

ouch<-st_transform(ouch,5070)
shed_list = rbind(shed_list, st_as_sf(ouch))

}

setwd('C://Users//andrew.caudillo//Box//NAMC//GIS//Watersheds//nhdPlusTools')

st_write(shed_list,dsn='missingWY_sheds.shp')
