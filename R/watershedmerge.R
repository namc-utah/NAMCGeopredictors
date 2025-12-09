library(sf)
library(mapview)
#load the separate shapefiles
shp <- st_read('C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC WATS Department Files//GIS//Watersheds//PIBO_Shapefiles//PIBO Watersheds//NPS//Catchment_523_14_I.shp')
shp2<-st_read('C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC WATS Department Files//GIS//Watersheds//PIBO_Shapefiles//PIBO Watersheds//NPS//Catchment_523_15_I.shp')


shp<-shed_list
shp2<-listy[,c('siteId','geometry')]
shp<-sf::st_transform(shp,crs(shp2))
#shp2<-st_transform(shp2,crs(shp))
#Combine the shapefiles into one
shp <- rbind(shp,shp2)
#Check to see if the watersheds make sense
mapview(shp)
shp<-shp[shp$siteId %in% c(46667,46735)==F,]
#Add siteId as a column to the shapefile
#get this from the nhd WS delineation code, which you should run before this
shp$siteId <- shp$siteId
shedsToAdd <- subset(shp[,c('siteId', 'geometry')])

shedsToAdd<-shedsToAdd[!duplicated(shedsToAdd$siteId),]
shedsToAdd<-shp
#load mastersheds
mastersheds <-st_read(watershed_file_path)
mastersheds<-mastersheds[mastersheds$siteId %in% shedsToAdd$siteId==F,]
  #st_read("C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC WATS Department Files//GIS//Watersheds//Matersheds//mastersheds.shp")
#Make sure shapefile is in the same crs as mastersheds shapefile
crs2use <- crs(mastersheds)
shedsToAdd <- st_transform(shedsToAdd, crs2use)
#Convert sheds from "POLYGON" to "MULTIPOLYGON"
shedsToAdd <- st_cast(shedsToAdd, "MULTIPOLYGON")
#mastersheds<-mastersheds[mastersheds$siteId %in% shedsToAdd$siteId==F,]
shedsToAdd<-shedsToAdd[shedsToAdd$siteId %in% mastersheds$siteId==F,]
#Combine shapefiles
mastershedsnew=plyr::rbind.fill(mastersheds,shedsToAdd)
head(mastersheds)
head(mastershedsnew)
dim(mastersheds)
dim(mastershedsnew)
mastershedsnew<-subset(mastershedsnew[,c('siteId','geometry')])
#Make sure mastershedsnew is an sf object
temp <- st_as_sf(mastershedsnew)
#write output as mastersheds file
st_write(temp, "C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC WATS Department Files//GIS/Watersheds//Mastersheds//mastersheds.shp")
#copy and paste siteIds from shedsRoAdd shapefie to excel file
clipr::write_clip(shedsToAdd$siteId)

sites<-NAMCr::query('sites',projectIds=5304)
#st_write(shedsToAdd,'C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC WATS Department Files//GIS//Watersheds//Matersheds//test_sheds.shp')
MS<-mastersheds[mastersheds$siteId %in% sites$siteId,]

st_write(MS,'C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC WATS Department Files//GIS/Watersheds//AIM_sheds//AIM2024.shp')
