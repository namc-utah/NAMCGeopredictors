#YET ANOTHER Streamcat workaround.
#Streamcat is no longer available for 4.3.2+
#so we will just fetch the JSONs from their website. No... it is not stealing...
#As long as we are using the correct areas that StreamStats covers, we should
#not have any failed watersheds.

rm(list=ls())
library(dplyr);library(sf)
boxId=9271
x<-NAMCr::query('sites',
                boxIds=boxId)
state_lookup<-data.frame(usState=c('California','Washinton','Orgeon',
                                 'Idaho','Utah','New Mexico','Arizona',
                                 'Colorado','Wyoming','Montana'),
                         Abbrv=c('CA','WA','OR',
                                 'ID','UT','NM',
                                 'AZ','CO','WY','MT'))

x<-plyr::join(x,state_lookup,by='usState')
sheds<-list()
for(i in 1:nrow(x)){
  y<-x[i,]
  message('accessing URL...')
  url<-paste0('https://streamstats.usgs.gov/streamstatsservices/watershed.geojson?',
              'rcode=',y$Abbrv,'&xlocation=',y$longitude,'&ylocation=',y$latitude,'&crs=4326&includeparameters=false&includeflowtypes=false&includefeatures=true&simplify=false')
  jsonURL<-jsonlite::fromJSON('https://streamstats.usgs.gov/streamstatsservices/watershed.geojson?rcode=CA&xlocation=-122.8521&ylocation=41.8352&crs=4326&includeparameters=false&includeflowtypes=false&includefeatures=true&simplify=false')
  message('URL accessed. Processing into an object.')
  jsonDF<-as.data.frame(jsonURL[["featurecollection"]][["feature"]][["features"]][[2]][["geometry"]][["coordinates"]][[1]][[2]])
  jsonSF<-sf::st_as_sf(jsonDF,coords=c('V1','V2'),crs=4326)
  if(st_geometry_type(jsonSF)[1]=='MULTIPOINT'){
    jsonSF<-st_cast(jsonSF,"POINT")
  }
  message('coercing to polygon...')
  polygon <- jsonSF %>%
    summarize(do_union = FALSE) %>%
    st_cast("POLYGON")
  message('watershed saved!')

  sheds[[i]]<-polygon
  print('iteration done')
}

mapview::mapview(sheds)
