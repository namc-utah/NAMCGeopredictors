#install.packages("remotes")
remotes::install_github("namc-utah/NAMCr",force=TRUE)

library(NAMCr)
library(sf)
library(geojsonio)
library(geojsonR)
library(geo)
library(mapview)
library(geojsonsf)
library(units)

### list of watersheds

AIMCatch<-read.csv("/Users/alexhernandez/Desktop/GitHubs/NAMCGeopa/sites_with_catchments02.csv",header = TRUE)
AIMCatch<-as.data.frame(AIMCatch[,1])

pirin<-list()
# Start the clock!
ptm <- proc.time()
for (i in 1:nrow(AIMCatch)){
  tryCatch({
    ii<-AIMCatch[i,]
    print(ii)
    ttcobjecto<-NAMCr::query("siteInfo", siteId=ii)
    geobjecto<-ttcobjecto[["catchment"]] # Extract the geojson object from the list
    siteident<-ttcobjecto[["siteName"]] # Extract the site identifier
    sfttc<-geojson_sf(geobjecto) # convert the geojson to sf object
    sfttc$nombre<-NA 
    sfttc$nombre<-siteident # add the watershed identifier as an attribute to the sf object
    sfttc$siteID<-ii
    pirin[[i]]<-sfttc # fill the empty list
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
# Stop the clock
proc.time() - ptm

sftotal<-do.call(rbind, pirin) # get the list components and turn them into a full sf object

mapview(sftotal)


write_sf(sftotal, "/Users/alexhernandez/Desktop/GitHubs/NAMCGeopa/May05_test.shp")

#### Testing functions with catchments
list2test<-as.data.frame(sftotal$siteID)

piringo<-list()
# Start the clock!
ptm <- proc.time()
for (i in 7:20){
  tryCatch({
    ii<-list2test[i,]
    print(ii)
    ttcobjecto<-NAMCr::query("siteInfo", siteId=ii)
    geobjecto<-ttcobjecto[["catchment"]] # Extract the geojson object from the list
    siteident<-ttcobjecto[["siteName"]] # Extract the site identifier
    sfttc<-Pmin_WS(geobjecto) # convert the geojson to sf object
    print(sfttc) 
    #sfttc$nombre<-siteident # add the watershed identifier as an attribute to the sf object
    piringo[[i]]<-sfttc # fill the empty list
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
# Stop the clock
proc.time() - ptm


dftotal<-do.call(rbind, piringo) # get the list components and turn them into a full df object


#### Testing functions with locations
list2test<-as.data.frame(sftotal$siteID)

piringopoint<-list()
# Start the clock!
ptm <- proc.time()
for (i in 1:15){
  tryCatch({
    ii<-list2test[i,]
    print(ii)
    ttcobjecto<-NAMCr::query("siteInfo", siteId=ii)
    geobjecto<-ttcobjecto[["location"]] # Extract the geojson object from the list
    siteident<-ttcobjecto[["siteName"]] # Extract the site identifier
    sfttc<-Pmin_PT(geobjecto) # convert the geojson to sf object
    print(sfttc) 
    #sfttc$nombre<-siteident # add the watershed identifier as an attribute to the sf object
    piringopoint[[i]]<-sfttc # fill the empty list
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
# Stop the clock
proc.time() - ptm


dftotalpoint<-do.call(rbind, piringopoint) # get the list components and turn them into a full df object


############################################################################
############################################################################
############################################################################
############## Predictors with dates
### list of watersheds with sample date

SampleDateCatch<-read.csv("/Users/alexhernandez/Desktop/GitHubs/NAMCGeopa/samples_with_catchments.csv",header = TRUE)
SampleDateCatch<-as.data.frame(SampleDateCatch[,1])

#list2test<-as.data.frame(sftotal$siteID)

piringopoint<-list()
# Start the clock!
ptm <- proc.time()
for (i in 1:150){
  tryCatch({
    ii<-SampleDateCatch[i,]
    print(ii)
    ttcobjecto01<-NAMCr::query('sampleInfo', sampleId=ii)
    sampledate<-ttcobjecto01[["sampleDate"]] # grab the sample date from the list
    siteID<-ttcobjecto01[["siteId"]]
    print(str(sampledate))
    Year<-as.numeric(substr(sampledate, start = 1, stop = 4))
    print(str(Year))
    print(siteID)
    ttcobjecto<-NAMCr::query("siteInfo", siteId=siteID)
    geobjecto<-ttcobjecto[["catchment"]] # Extract the geojson object from the list
    siteident<-ttcobjecto[["siteName"]] # Extract the site identifier
    sfttc<-PRCPSHORTW(geobjecto,Year) # convert the geojson to sf object
    print(paste0("the pcp is...",sfttc)) 
    #sfttc$nombre<-siteident # add the watershed identifier as an attribute to the sf object
    piringopoint[[i]]<-sfttc # fill the empty list
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
# Stop the clock
proc.time() - ptm


dftotalpoint<-do.call(rbind, piringopoint) # get the list components and turn them into a full df object






############################################################################
############################################################################


ttc<-NAMCr::query("siteInfo", siteId=c(28305,27902,27992))
ttc1<-ttc[["catchment"]]

# file_js<-FROM_GeoJson(url_file_string = ttc1)
sf01<-geojson_sf(ttc1)
