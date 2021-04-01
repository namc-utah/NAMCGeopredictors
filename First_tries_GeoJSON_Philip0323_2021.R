install.packages("remotes")
remotes::install_github("namc-utah/NAMCr")

library(NAMCr)
library(sf)
library(geojsonio)
library(geojsonR)
library(geo)
library(mapview)
library(geojsonsf)

### list of watersheds

purrunga<-read.csv("/Users/alexhernandez/Desktop/GitHubs/NAMCGeopa/sites_with_catchments.csv",header = FALSE)
purrunga<-c(28305,27902,27992,7135,
            7162,
            7140,
            7146,
            7149,
            7154,
            27911)
pirin<-list()
for (i in 1:nrow(purrunga)){
  ii<-purrunga[i,]
  print(ii)
  ttcpurrunga<-NAMCr::query("siteInfo", siteId=ii)
  ttcmaje<-ttcpurrunga[["catchment"]] # Extract the geojson object from the list
  siteident<-ttcpurrunga[["siteName"]] # Extract the site identifier
  sfttc<-geojson_sf(ttcmaje) # convert the geojson to sf object
  sfttc$nombre<-NA 
  sfttc$nombre<-siteident # add the watershed identifier as an attribute to the sf object
  pirin[[i]]<-sfttc # fill the empty list
}


sftotal<-do.call(rbind, pirin) # get the list components and turn them into a full sf object


for (i in 1:nrow(purrunga)){
  ii<-purrunga[i,]
  print(ii)
}











ttc<-NAMCr::query("siteInfo", siteId=c(28305,27902,27992))
ttc1<-ttc[["catchment"]]

# file_js<-FROM_GeoJson(url_file_string = ttc1)
sf01<-geojson_sf(ttc1)
