#rivnet?

#Sys.setenv(TAUDEM_PATH = "C://Program Files//TauDEM//TauDEM5Exe")
#Sys.setenv(MPI_PATH = 'C://Program Files//Microsoft MPI//Bin')
#Sys.setenv(GDAL_PATH= 'C://Program Files//GDAL')
library(sf)

library(NAMCr)
library(sp)
library(rivnet)
library(tidyverse)
library(traudem)

library(terra)

boxnum<-7852
#query the box in question
x<-NAMCr::query(
  api_endpoint = "samples",
  args = list(boxId = boxnum))

#can make this more robust if needed
#x<-x[x$sampleId %in% 211612,]

#make a dataframe of just the coordinates
#but make Lon positive for easy url import.
#more detail below, but TNM stores their
#files with positive integers in the filenames
#so we need a positive integer for Lon
dfcoords<-data.frame(abs(x$siteLongitude),x$siteLatitude)
#give them better names
names(dfcoords)<-c('Lon','Lat')
#attach siteId for a flag when checking against master sheds
#or for future delineations with an existing siteID
dfcoords$siteId<-x$siteId
#make a a spatial dataframe for the actual processing
coords<-SpatialPoints(dfcoords,proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs'))
#name of the state the box is from. This will be used for writing out the raster
Namestate<-unique(x$usState)

#this is a smaller dataframe that is just the rounded integer value
#for the coordinates above.
#When using the National Map, DEMs are stored by cells
#labeled with the integer values, 1x1 degree tiles.
#round up to get the desired DEM. e.g., Lat/long:
#40.7,-119.8 will be in 41,-120.
#Cavaet: the integers are positive (probably for URL reasons)
#so we need positive values, even though we use negative
#Longitude

coarse_pts<-transmute_all(dfcoords[,1:2],ceiling)

#remove duplicates, because we don't need them
Lcoarse_pts<-coarse_pts[!duplicated(coarse_pts),]
dfcoords$cLon<-coarse_pts$Lon;dfcoords$cLat<-coarse_pts$Lat
#change it back to Western hemisphere
dfcoords$Lon<-(-1*dfcoords$Lon)
setwd('C://Users//andrew.caudillo//Box//NAMC//GIS//DEMs//TNM')
# a list of URLs from the National Map.
#Essentially, every DEM in NAMC's relevant study area
#that TNM hosts. Could update every year or so, if we see it fit.
#some files are updated periodically, where some have not been updated
#since 2013.
url_list<-read.csv('DEM_url_list.csv',stringsAsFactors = F)
setwd('C://Users//andrew.caudillo//Box//NAMC//GIS//DEMs//TNM//WY')

if(0){
  #stringr::str_match(NV$url,"3_\\s*(.*?)\\s*_2")
  for(i in 1:nrow(coarse_pts)){
    print('reading raster')
    x<-raster::raster(NV$url[i])
    locs<-stringr::str_match(NV$url[i],"3_\\s*(.*?)\\s*_2")
    print('writing raster')
    raster::writeRaster(x,paste(Namestate,locs[,2],'10m.tif',sep='_'),'GTiff',overwrite=T)
    print('Ho-kay, mane. Onto next one!')
  }
}
#for loop 1) download only necessary DEMs
for(i in 1:1){#nrow(Lcoarse_pts)){
  #subset the url list to match the one we need via the pattern towards the
  #end of the url "_nYwX_" where Y and X are integers of Lat/Long, repesctively
  url<-url_list[grep(pattern=paste0('_n',Lcoarse_pts$Lat[i],'w',abs(Lcoarse_pts$Lon[i]),'_'),url_list$URL),]
  print('url is being accessed')
  print('now reading in Raster')
  #read in the raster
  #R::raster is smart enough to retrieve a raster from a URL
  rast<-raster::raster(url)
  print('Raster is being written')
  #write the raster. Normally, I am against this because it takes up unnecessary space
  #but whitebox requires a filepath, not a URL, when reading in rasters.
  #raster::writeRaster(rast,paste(Namestate,Lcoarse_pts$Lat[i],abs(Lcoarse_pts$Lon[i]),'10m.tif',sep='_'),'GTiff',overwrite=T)

  print(paste('finished iteration',i,'of', nrow(Lcoarse_pts)))

}


fp <- rast#system.file("extdata/wigger.tif", package = "rivnet")
r <- extract_river(outlet = c(dfcoords$Lon[1],dfcoords$Lat[1]),
                   DEM = fp,
                   EPSG = 4269)
#works but no shed??
st_transform(fp,crs(coords))
nyeh<-aggregate_river(r)
mapview(r)
st_as_sfc(r)
plot(nyeh, chooseCM=1)

windows(10,8)
rivnet::plot(r)
rivnet::plot(nyeh)
