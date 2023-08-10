library(sf)
library(mapview)
#load the separate shapefiles
shp_42877 <- st_read("~/Library/CloudStorage/Box-Box/NAMC/GIS/Watersheds/streamstats_R/ws_shp/42877_shed.shp")
shp_42879 <- st_read("~/Library/CloudStorage/Box-Box/NAMC/GIS/Watersheds/streamstats_R/ws_shp/42879_shed.shp")
shp_42880 <- st_read("~/Library/CloudStorage/Box-Box/NAMC/GIS/Watersheds/streamstats_R/ws_shp/42880_shed.shp")
shp_42881 <- st_read("~/Library/CloudStorage/Box-Box/NAMC/GIS/Watersheds/streamstats_R/ws_shp/42881_shed.shp")
#Combine the shapefiles into one
shp <- rbind(shp_42877, shp_42879, shp_42880, shp_42881)
#Check to see if the watersheds make sense
mapview(shp)
#Add siteId as a column to the shapefile
shp$siteId <- c(42877, 42879, 42880, 42881)
shedsToAdd <- subset(shp[,c('siteId', 'geometry')])
#load mastersheds
mastersheds <- st_read("~/Library/CloudStorage/Box-Box/NAMC/GIS/Watersheds/Mastersheds/mastersheds.shp")
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
st_write(temp, "~/Library/CloudStorage/Box-Box/NAMC/GIS/Watersheds/Mastersheds/mastersheds.shp")
#copy and paste siteIds from shedsRoAdd shapefie to excel file
clipr::write_clip(shedsToAdd$siteId)
