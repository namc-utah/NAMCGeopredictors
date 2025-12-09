
library(sf)
library(maptools)
df<-st_read(paste0(genpath,'AIM_2023_leftovers_issuepoints_needsheds','.shp')) #issus with streamstats
NSS_df<-read.csv(paste0(genpath,'AIM_2023_leftovers_nonStreamStats.csv')) #NV or WY
#}
#this is for pulling coordinates via NAMCr
library(NAMCr)
NSS_Sites<-NAMCr::query(api_endpoint = 'sites',
                 args=list(siteIds=c(
                           43257)
                 ))
#we can change this to be multiple boxIDs for a bulk job
boxes<-c(5744)
flag=0

if( length(boxes) > 1){
  box_list<-list()
  for(i in 1:length(boxes)){
    result<-query(api_endpoint = 'samples',
                  args=list(boxId=boxes[[i]]))
    box_list[[i]]<-result
  }
  #make the list into a dataframe
  result<-do.call(rbind,box_list)
}else{
  #or just read in the one box
  result<-query(api_endpoint = 'samples',
                args=list(boxId=boxes))
}
#Global watersheds offers an API of sorts
#all that it needs is a lat/long with the below url (with some other pieces, see for loop below)
#the "API" then returns a JSON of the watershed, which we can turn into a shapefile
#and write out into the master watersheds file. Note that the API uses MERIT-hydro as the source
#for flow accumulation and direction.

#this is the base url. this part will not change
myhydro<-'https://mghydro.com/app/watershed_api?'

#make a dataframe of the coordinates that we want watersheds for
coords<-data.frame(Lat=df$OrgLAT,Lon=df$OrgLONG)
coords<-data.frame(Lat=result$siteLatitude,Lon=result$sampleLongitude)
coords<-data.frame(Lon=NSS_Sites$longitude,Lat=NSS_Sites$latitude)
sf_coords<- st_as_sf(x=coords,
                  coords=c('Lon','Lat'),
                 crs=4269)
#set this wherever this needs to go
setwd()
shed_list<-list()
for(i in 1:nrow(coords)){
  #create the link by pasting the dynamic parts with the static parts
  hydrolink<-paste0(myhydro,'lat=',coords$Lat[i],'&','lng=',coords$Lon[i],'&precision=high')
  #sf has the capability to read a spatial file via the web
  #so just use that here.
  shed<-read_sf(hydrolink)
  shed_list[[i]]<-shed
  x<-class(shed_list[[i]]$geometry)
  if(x[1]=='sfc_MULTIPOLYGON'){
    flag=1
    shed_list[[i]]<-st_cast(shed_list[[i]],"POLYGON")
  }
  #write the file out
  #sf::st_write(shed,dsn=paste('Box',result$boxId[i], 'Site',result$siteId[i],'watershed',sep=''),driver='ESRI Shapefile')
  #counter for progress
  if(flag==0){
    message(paste('exported watershed ',i,' of ',nrow(coords),sep = ''))
  }else{message(paste('exported watershed ',i,' of ',nrow(coords)," . Multipolygon coerced to Polygon.",sep = ''))}
}

#make them all into one shapefile

newsheds<-sf::st_as_sf(data.table::rbindlist(shed_list))
newsheds$siteId<-as.character(NSS_Sites$siteId)
windows(10,8)
mapview::mapview(newsheds)
