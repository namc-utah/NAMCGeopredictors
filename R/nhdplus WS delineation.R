# Quick Watershed Delineation using NHDPlusTools
  # Coder: Nate Jones (cnjones7@ua.edu),
  # adapted for NAMC use by Andrew Caudillo


  # 1.0 Setup Environment --------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#clear memory
rm(list=ls())

#Call relevant libraries
library(tidyverse)
library(mapview)
library(raster)
library(sf)
library(tigris)
library(nhdplusTools) #this was not loaded originally
library(NAMCr)
#pred_geometry_base_path="C://Users//andrew.caudillo//Box//NAMC//"

mastershed<-st_read(watershed_file_path)

#nv<-st_read('C://Users//andrew.caudillo//Box//NAMC//GIS//Watersheds//SnappedPointsSentStreamStats//CA_2023-11-01.shp')
samps<-NAMCr::query(
  api_endpoint = "samples",
  args=list(siteIds=c(46316,46317,46318,46319,
                      46320,46321,46322,46323,
                      46324,46325,46326,46327,
                      46328,46329,46330,46331,
                      46332,46333,46334,46335,
                      46336,46337,46338,11888,
                      46339,46340,46341,46342,
                      46343,46344,46345,46346,
                      46347,46383,46384,46385,
                      46386,46397,46398,46399,
                      46400,46401,46402,46403,
                      46404,46405,46406,46407,
                      46408,46409,46410,46411,
                      46412,46413,46414,46415,
                      46416,46417,46418,46419,
                      46420,46421,46422,46423,
                      46424,46425,46426,46427,
                      46428,46429,46430,46431,
                      46432,46433,46434,46435,
                      46436,46437,46438,46388,
                      46380)))
#samps<-samps[samps$sampleId==213513,]
#for input from WS Delineation script...
samps_For_sheds<-samps

#missings_sheds<-st_read('C://Users//andrew.caudillo//Box//NAMC//GIS//Watersheds//ManualDelineations//2023-11-01.shp')

#missings_sheds<-missings_sheds[missings_sheds$siteId %in% mastershed$siteId==F,]

missings_sheds<-samps[samps$siteId %in% mastershed$siteId==F,]
#missings_sheds<-samps
#missings_sheds<-samps$siteId[samps$siteId %in% missings_sheds]
samps_For_sheds<-samps[samps$siteId %in% missings_sheds$siteId,]

#samps_For_sheds<-missings_sheds
#samps_For_sheds<-samps

#Load state shape
#it is imperative to change the state abbreviation!
bama <- states() %>% dplyr::filter((STUSPS %in% c('NV,WY')))

#making df of just the site points
#remember that NAMC uses sites, not samples, for model application and predictors
samps_For_sheds<-samps_For_sheds[!duplicated(samps_For_sheds$siteId),]
samps_For_sheds_Coords<-samps_For_sheds[,c("siteLatitude","siteLongitude")]


#this is the "pour point" essentially
outlet<-sf::st_as_sf(samps_For_sheds_Coords, coords = c("siteLongitude","siteLatitude"),crs=4269)
outlet$siteId<-samps_For_sheds$siteId

#Rerpoject and clip to continental US
bama <- st_transform(bama, 5070)
outlet <- st_transform(outlet, 5070)
#outlet<-st_transform(missings_sheds,5070)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Delineate ----------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create starting point
NHDshed_list<-st_sfc(crs=5070)

#for loop that allows N sheds to be delineated
#and then binds them into one object at the end
#excellent use for large sets, especially
#AIM sets when they first come in.
for(i in 1:nrow(outlet)){
  print(i)
start_comid <- discover_nhdplus_id(outlet$geometry[i])
start_comid

# flowlines
flowlines <- navigate_nldi(list(featureSource = "comid",
                                featureID = start_comid),
                           mode = "upstreamTributaries",
                           distance_km = 1000)



# Note, the next step pulls NHDplus data from NLNDI server. For large watersheds,
# it will likely be faster to download the WBD form the USGS National Map website (link below),
# and intersect the "flowlines" shape above with the downloaded layer.
# https://www.usgs.gov/national-hydrography/access-national-hydrography-products

#get nhdplus files
subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowlines$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download",
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)
#catchments
catchment <- sf::read_sf(subset_file, "CatchmentSP")

#Dissovle files to single catchment
catchment <- sf::st_union(catchment)
ouch<-sfheaders::sf_remove_holes(catchment)

ouch<-st_transform(ouch,5070)
#make a list of sheds that grows with each iteration
NHDshed_list = rbind(NHDshed_list, st_as_sf(ouch))

}
samps_For_sheds$siteId[samps_For_sheds$siteId %in% shed_list$siteId==F]
#plot just to see everything looks alright
#if a shed looks incorrect, use a different method like whitebox.
#could also use global watersheds for a quick look.
mapview(list(outlet,NHDshed_list))
NHDshed_list$siteId<-outlet$siteId
mapview(list(NHDshed_list,shed_list))
mapview(NHDshed_list[10,])
mapview(shed_list)
pal=mapviewPalette("mapviewTopoColors")
mapview(shed_list,col.regions=pal(11))

st_write(shed_list,dsn='C://Users//andrew.caudillo//Box//NAMC//GIS//Watersheds//nhdPlusTools//nv_sheds_231101.shp',append = F)
st_write(ouch, dsn='C://Users//andrew.caudillo//Box//NAMC//GIS//Watersheds//NHD_streamstats_Compare_231102//ORID_shedtocomp.shp',append = F)

st_write(NHDshed_list,dsn='C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC WATS Department Files//GIS//Watersheds//nhdPlusTools//problem_sheds3_241008.shp',append=F)
NHDshed_list$x
st_geometry(NHDshed_list)<-'geometry'
