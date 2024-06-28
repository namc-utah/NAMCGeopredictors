#YET ANOTHER Streamcat workaround.
#Streamcat is no longer available for 4.3.2+
#so we will just fetch the JSONs from their website. No... it is not stealing...
#As long as we are using the correct areas that StreamStats covers, we should
#not have any failed watersheds. The failures in the past are not from StreamStats,
#but the streamstats package, which USGS apparently had no affiliation with.

rm(list=ls())
#load some necessary libraries, if not already loaded.
library(dplyr);library(sf)

x<-NAMCr::query('sites',
                boxIds=boxId)
#making a little data frame that will act as a lookup table
#for when we need to build the StreamStats URL.
state_lookup<-data.frame(usState=c('California','Washinton','Orgeon',
                                 'Idaho','Utah','New Mexico','Arizona',
                                 'Colorado','Wyoming','Montana','Nevada',
                                 'Alaska'),
                         Abbrv=c('CA','WA','OR',
                                 'ID','UT','NM',
                                 'AZ','CO','WY','MT','NV','AK'))
#join the two so we get state abbreviations
x<-plyr::join(x,state_lookup,by='usState')
#subset out only states where StreamStats has coverage
x<-x[x$Abbrv %in% c('NV','WY','AK')==F,]
sheds<-list()
for(i in 1:nrow(x)){
  #subset out the ith row
  y<-x[i,]
  message('accessing URL...')
  #creating the url. We are just pasting together the skeleton of the url
  #plus the dynamic pieces from y-- not too difficult.
  url<-paste0('https://streamstats.usgs.gov/streamstatsservices/watershed.geojson?',
              'rcode=',y$Abbrv,'&xlocation=',y$longitude,'&ylocation=',y$latitude,'&crs=4326&includeparameters=false&includeflowtypes=false&includefeatures=true&simplify=false')
  #now we just read the url using the jsonlite package
  jsonURL<-jsonlite::fromJSON('https://streamstats.usgs.gov/streamstatsservices/watershed.geojson?rcode=CA&xlocation=-122.8521&ylocation=41.8352&crs=4326&includeparameters=false&includeflowtypes=false&includefeatures=true&simplify=false')
  message('URL accessed. Processing into an object.')
  #now coerce that json's geometry to a dataframe.
  jsonDF<-as.data.frame(jsonURL[["featurecollection"]][["feature"]][["features"]][[2]][["geometry"]][["coordinates"]][[1]][[2]])
  #convert that data frame to an sf object with a coordinate system
  jsonSF<-sf::st_as_sf(jsonDF,coords=c('V1','V2'),crs=4326)
  #it is going to be points, for some reason, so we are just going
  #to coerce the points to a polygon in these two steps:
  if(st_geometry_type(jsonSF)[1]=='MULTIPOINT'){
    jsonSF<-st_cast(jsonSF,"POINT")
  }
  message('coercing to polygon...')
  polygon <- jsonSF %>%
    summarize(do_union = FALSE) %>%
    st_cast("POLYGON")
  message('watershed saved!')
  polygon$siteId=x$siteId
#save that polygon to our list
  sheds[[i]]<-polygon
  message('iteration done!')
  message(paste('Extracted shed ',i, ' of ',nrow(x)))
}
#plot to see the results
mapview::mapview(sheds,col.regions='red',border='black',lwd=3)
