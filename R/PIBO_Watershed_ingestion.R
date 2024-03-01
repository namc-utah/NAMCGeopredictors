#PIBO watershed import Spatial join prep

Peabo<-NAMCr::query('sites',projectIds=3)
#write.csv(Peabo,'C://Users//andrew.caudillo//Box//NAMC//GIS//Watersheds//PIBO_Shapefiles//PIBO Watersheds//PIBO_NAMCpts.csv')
#Peabo_sf<-sf::st_as_sf(Peabo,coords=c('longitude','latitude'),crs=4269)

#sf::st_write(Peabo_sf,'C://Users//andrew.caudillo//Box//NAMC//GIS//Watersheds//PIBO_Shapefiles//PIBO Watersheds//PIBO_NAMCpts.shp')
if(0){
Psheds<-sf::st_read('C://Users//andrew.caudillo//Box//NAMC//GIS//Watersheds//PIBO_Shapefiles//PIBO Watersheds//PIBO_sheds_toimport.shp')
Psheds_good<-Psheds[is.na(Psheds$siteNam)==F,]
Psheds_matchbutNA<-Psheds[Psheds$SiteID %in% c(2282,1282,1289,1953,12,915,3195),]
Psheds_matchbutNA$siteId_2<-c(17312,18168,17532,17539,16544,17976,18434)
final_psheds<-rbind(Psheds_good,Psheds_matchbutNA)
final_psheds<-final_psheds[final_psheds$SiteID %in%
                             c(2047,1862,793,307,3538,3539,3540,3499,3541,
                               3262,3542,3547,3548,3549,3551,3553)==F,]
final_psheds[duplicated(final_psheds$siteId_2),]
}

#join instead?
#read in file
P<-sf::st_read('C://Users//andrew.caudillo//Box//NAMC//GIS//Watersheds//PIBO_Shapefiles//PIBO Watersheds//PIBO_All_Watersheds_2019.shp')
#pad the site names to match how NAMC has it stored, e.g., 4 digits
P$SiteID<-sprintf('%04d',P$SiteID)
#make a copy, just in case
Pea<-Peabo
#remove "PIBO" from site name for direct comparison
Pea$siteName<-gsub(".*:","",Pea$siteName)
#rename for a join
names(P)[2]<-'siteName'

library(tidyverse)
#join the two
PIBO<-P %>% left_join(Pea)
#just want those which we have. We do not have every site, based on their shapefiles
PIBO<-PIBO[is.na(PIBO$siteId)==F,]

#now subset so we can merge
shedsToAdd<-PIBO[,c('siteId','geometry')]


mastersheds <- st_read("C://Users//andrew.caudillo//Box//NAMC//GIS/Watersheds//Mastersheds//mastersheds.shp")
#Make sure shapefile is in the same crs as mastersheds shapefile
crs2use <- terra::crs(mastersheds)
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

PIBO
