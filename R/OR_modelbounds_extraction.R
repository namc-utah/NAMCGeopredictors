boxId=4490

library(NAMCr)
library(sf)
library(sp)
library(raster)
library(rgdal)
x<-query(
  api_endpoint = "samples",
  args = list(boxId = boxId))
coords<-data.frame(x$sampleLongitude,x$sampleLatitude,x$sampleId)



OR_model_bounds<-shapefile('C://Users//andrew.caudillo//Box//NAMC//GIS//GIS_Stats//Oregon//ecoregion//OR_Model_Boundaries.shp')

OR_coords<-SpatialPointsDataFrame(coords[,1:2],x,proj4string = CRS('+proj=longlat'))
crs(OR_coords)
OR_coords<-spTransform(OR_coords,crs(OR_model_bounds))

mapview::mapview(list(OR_model_bounds,OR_coords))
extraction<-over(OR_coords,OR_model_bounds)
extraction$sampleID<-x$sampleId
extraction

model10<-extraction$sampleID[extraction$L3_KEY=='10  Columbia Plateau']
model12<-extraction$sampleID[extraction$L3_KEY=='80  Northern Basin and Range']
unique(extraction$L3_KEY)
