#WS delin using whitebox
#a very fast and usually effective way of delineation.
#be sure that the DEMs are projected into WGS 84 (see below)
rm(list=ls())
#library(tidyverse)
library(raster)
library(sf)
library(whitebox)
library(NAMCr)
#library(tmap)
#library(stars)
#library(rayshader)
#library(rgl)


boxnum<-5747
x<-query(
  api_endpoint = "samples",
  args = list(boxId = boxnum))
dfcoords<-data.frame(x$sampleLongitude,x$siteLatitude)
coords<-SpatialPoints(dfcoords,proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs'))

setwd('C://Users//andrew.caudillo//Box//NAMC//GIS//DEMs//TNM//NV_DEMs')
for(i in 1:85){
  #read in DEM
  #be sure it has been projected before running! or whitebox will break
  j<-x[i,]
  DEM<-elevatr::get_elev_raster(coords[i,],z=14)

  wbt_breach_depressions_least_cost(
    dem=DEM,
    output='NV_Test_breached.tif',
    dist=10,
    fill=T)
  #flow accum
  #use this to get streams, too
  wbt_d8_flow_accumulation(input = "NV_Test_breached.tif",
                           output = "NV_Test_breached_acc.tif")

  #flow dir
  wbt_d8_pointer(dem = "NV_Test_breached.tif",
                 output = "NV_test_breached_dir.tif")

  pps<-coords[i,]

  shapefile(pps, filename = paste("NV_Test_pps",j$siteId,".shp",sep=''), overwrite = TRUE)

  wbt_extract_streams(flow_accum = "NV_Test_breached_acc.tif",
                      output = "NV_Test_streams.tif",
                      threshold = 6000)

  wbt_jenson_snap_pour_points(pour_pts = paste("NV_Test_pps",j$siteId,".shp",sep=''),
                              streams = "NV_Test_streams.tif",
                              output = paste("NV_Snapped_pps",j$siteId,".shp",sep=''),
                              snap_dist = 0.0005) #careful with this! Know the units of your data
  wbt_watershed(d8_pntr = "NV_test_breached_dir.tif",
                pour_pts = paste("NV_Snapped_pps",j$siteId,".shp",sep=''),
                output = paste("NV_Test_watersheds",j$siteId,".tif",sep=''))

}
nrow(coords)

1:nrow(dfcoords)
