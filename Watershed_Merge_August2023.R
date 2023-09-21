library(sf)
library(mapview)
#load the separate shapefiles
shp <- st_read("C://Users//andrew.caudillo//Box//NAMC//GIS//Watersheds//nhdPlusTools//miscAIM22_sheds.shp")
#shp<-shed_list
#Combine the shapefiles into one
#shp <- rbind(shp_42877, shp_42879, shp_42880, shp_42881)
#Check to see if the watersheds make sense
mapview(shp)
#Add siteId as a column to the shapefile
#get this from the nhd WS delineation code, which you should run before this
shp$siteId <- shp$siteid
shedsToAdd <- subset(shp[,c('siteId', 'geometry')])
shedsToAdd<-shedsToAdd[!duplicated(shedsToAdd$siteId),]
#load mastersheds
mastersheds <- st_read("C://Users//andrew.caudillo//Box//NAMC//GIS/Watersheds//Mastersheds//mastersheds.shp")
#Make sure shapefile is in the same crs as mastersheds shapefile
crs2use <- crs(mastersheds)
shedsToAdd <- st_transform(shedsToAdd, crs2use)
#Convert sheds from "POLYGON" to "MULTIPOLYGON"
shedsToAdd <- st_cast(shedsToAdd, "MULTIPOLYGON")

#Combine shapefiles
mastershedsnew=plyr::rbind.fill(mastersheds,shedsToAdd)
head(mastersheds)
head(mastershedsnew)
dim(mastersheds)
dim(mastershedsnew)
#Make sure mastershedsnew is an sf object
temp <- st_as_sf(mastershedsnew)
#write output as mastersheds file
st_write(temp, "C://Users//andrew.caudillo//Box//NAMC//GIS/Watersheds//Mastersheds//mastersheds.shp")
#copy and paste siteIds from shedsRoAdd shapefie to excel file
clipr::write_clip(shedsToAdd$siteId)
