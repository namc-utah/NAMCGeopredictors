#aremp shed clean up
rm(list=ls())
#read in AREMP data, pulled from SQL.

AREMP_historic<-read.csv('C://Users//andrew.caudillo//Box//NAMC//GIS//Watersheds//AREMP_to_NAMC.gdb//AREMP_sitenums.csv',stringsAsFactors = F)
#rename
names(AREMP_historic)[1]<-'siteId'
#remove duplicates, as sheds are linked by siteId, not sample.
AREMP_historic<-AREMP_historic[!duplicated(AREMP_historic$siteId),]
#remove any (old) samples with no siteId in SQL database
AREMP_historic<-AREMP_historic[!is.na(AREMP_historic$siteId),]
#read in mastersheds
pred_geometry_base_path="C://Users//andrew.caudillo//Box//NAMC//"
Msheds=sf::st_read(paste0(pred_geometry_base_path,"GIS//Watersheds//Mastersheds//mastersheds.shp"))
#make it into a df so we can join
MSheds_sites<-as.data.frame(Msheds)


#first join to see what we have in mastersheds
AREMP_in_MS<-plyr::join(AREMP_historic,as.data.frame(MSheds_sites),by='siteId',)
#these are sites with no geometry in mastersheds. we likely never
#did much with these data.
empty_geos<-AREMP_in_MS[sf::st_is_empty(AREMP_in_MS$geometry),]
#these are AREMP sites with sheds in MASTERSHEDS.
#replace these
#then add the others, too?
AREMP_in_MS_clean<-AREMP_in_MS[!sf::st_is_empty(AREMP_in_MS$geometry),]

#the sheds we received straight from AREMP that they would like us to use from now on
aremp_new<-sf::st_read('C://Users//andrew.caudillo//Box//NAMC//GIS//Watersheds//AREMP_to_NAMC.gdb//Feb2024//AREMP_sheds_updated2024.shp')
#rename column for joins
names(aremp_new)[4]<-'siteName'
#coerce so we can join
aremp_new<-as.data.frame(aremp_new)
#rename geometry so we don't get confused or overlapping colnames
names(aremp_new)[ncol(aremp_new)]<-'AREMPgeometry'
#quert site names so we can join with AREMP's new data
#they only use siteName, so we need that.
AREMP_sites_NAMC<-NAMCr::query(api_endpoint = 'sites',
             siteIds=AREMP_in_MS$siteId)
#getting sitename
big_AREMP<-plyr::join(AREMP_in_MS,AREMP_sites_NAMC[,c("siteId",'siteName')],by='siteId')
#double cheked-- the same missing sites as before
empty_bigAREMP<-big_AREMP[sf::st_is_empty(big_AREMP$geometry),]
#sites that do exist in MS
big_AREMP<-big_AREMP[!sf::st_is_empty(big_AREMP$geometry),]


#the final join where we get mastershed sites, site names, and AREMP geometries
final_AREMP<-plyr::join(big_AREMP,aremp_new[,c('siteName','AREMPgeometry')],by='siteName')

#need to find these siteIds
#these are new AREMP sites that are NOT  in MS
AREMP_new_noMS<-aremp_new[aremp_new$siteName %in% final_AREMP$siteName==F,]

#we just found them
siteIDs_for_empty_new_AREMP<-NAMCr::query(api_endpoint = 'sites',
             args=list(siteNames=AREMP_new_noMS$siteName))
empty_new_AREMP_with_siteID<-plyr::join(AREMP_new_noMS,siteIDs_for_empty_new_AREMP[,c('siteId','siteName')],by='siteName')


#make the shapefiles to then merge into MS

final_AREMP_sites_with_sheds<-sf::st_as_sf(final_AREMP)
extra_AREMP_sheds<-sf::st_as_sf(empty_new_AREMP_with_siteID)
#no change, but just want to make sure{
extra_AREMP_sheds<-extra_AREMP_sheds[!duplicated(extra_AREMP_sheds$siteName),]
extra_AREMP_sheds<-extra_AREMP_sheds[!is.na(extra_AREMP_sheds$siteId),]
final_AREMP_sites_with_sheds<-final_AREMP_sites_with_sheds[!duplicated(final_AREMP_sites_with_sheds$siteId),]
#}

#now we can add these to MS...
#getting only necessary information. Probably could have subset, but it is ok.
finalshp<-sf::st_as_sf(data.frame(final_AREMP_sites_with_sheds$siteId,final_AREMP_sites_with_sheds$AREMPgeo))
extra_shp<-sf::st_as_sf(data.frame(extra_AREMP_sheds$siteId,extra_AREMP_sheds$AREMPgeometry))

names(finalshp)[1:2]<-c('siteId','geometry')
names(extra_shp)[1:2]<-c('siteId','geometry')
AREMP_to_import<-rbind(finalshp,extra_shp)
AREMP_to_import<-AREMP_to_import[!duplicated(AREMP_to_import$siteId),]
crs_2_use<-sf::st_crs(Msheds)

AREMP_to_import<-sf::st_transform(AREMP_to_import,crs_2_use)
#AREMP had z and m coordinates, although they were 0, but sf does not like those
#so we drop them
AREMP_to_import<-sf::st_zm(AREMP_to_import)
AREMP_to_import<-sf::st_cast(AREMP_to_import,'MULTIPOLYGON')

#omitting the old AREMP sheds via siteId
Msheds<-Msheds[Msheds$siteId %in% AREMP_to_import$siteId==F,]

#combine the shapefiles
newest_msheds<-plyr::rbind.fill(Msheds,AREMP_to_import)


temp <- sf::st_as_sf(newest_msheds)
#write output as mastersheds file
sf::st_write(temp, "C://Users//andrew.caudillo//Box//NAMC//GIS/Watersheds//Mastersheds//mastersheds.shp")
#copy and paste siteIds from shedsRoAdd shapefie to excel file
clipr::write_clip(AREMP_to_import$siteId)
