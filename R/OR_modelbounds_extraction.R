boxId=5745

library(NAMCr)
library(sf)
library(sp)
library(raster)
library(rgdal)
x<-query(
  api_endpoint = "samples",
  args = list(boxId = boxId))
coords<-x[c('sampleLongitude','sampleLatitude','sampleId')]


OR_model_bounds<-shapefile('C://Users//andrew.caudillo//Box//NAMC//GIS//GIS_Stats//Oregon//ecoregion//OR_Model_Boundaries.shp')

WY_model_bounds<-shapefile('C://Users//andrew.caudillo//Box//NAMC//GIS//GIS_Stats//Wyoming//ecoregion//BIOREGIONS_2011_modifiedCP.shp')

OR_coords<-SpatialPointsDataFrame(coords[,1:2],coords,proj4string = CRS('+proj=longlat'))
OR_coords<-spTransform(OR_coords,crs(OR_model_bounds))

WY_coords<-SpatialPointsDataFrame(coords[,1:2],coords,proj4string = CRS('+proj=longlat'))
WY_coords<-spTransform(WY_coords,crs(WY_model_bounds))

mapview::mapview(list(WY_model_bounds,WY_coords))
extraction<-over(WY_coords,WY_model_bounds)
extraction$sampleID<-x$sampleId
extraction
?over
model10<-extraction$sampleID[extraction$L3_KEY=='10  Columbia Plateau']
model11<-extracton$sampleID[extraction$L3_KEY=='1  Coast Range']
model12<-extraction$sampleID[extraction$L3_KEY=='80  Northern Basin and Range']
unique(extraction$L3_KEY)

OR_model_bounds@data$L3_KEY
