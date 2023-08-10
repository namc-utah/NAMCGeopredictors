#WS delin using whitebox
#a very fast and usually effective way of delineation.
#be sure that the DEMs are projected into WGS 84 (see below)
rm(list=ls())
#library(tidyverse)
library(raster)
library(sf)
library(whitebox)
library(NAMCr)
library(sp)
#library(tmap)
#library(stars)
#library(rayshader)
#library(rgl)

#box query for NAMCr
boxnum<-4453
#query the box in question
x<-query(
  api_endpoint = "samples",
  args = list(boxId = boxnum))
#special case for CA
x<-x[x$sampleId %in% c(211231,211233,211234,211235),]
#make a dataframe of just the coordinates
#but make Lon positive for easy url import.
#more detail below, but TNM stores their
#files with positive integers in the filenames
#so we need a positive integer for Lon
dfcoords<-data.frame(abs(x$sampleLongitude),x$siteLatitude)
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
coarse_pts<-as.data.frame(sapply(dfcoords[,1:2],ceiling))
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
setwd('C://Users//andrew.caudillo//Box//NAMC//GIS//DEMs//TNM//NV_DEMs')
setwd('C://Users//andrew.caudillo//Box//NAMC//GIS//DEMs//TNM//NV_DEMs//AllNV')
NV<-read.csv('ALL_NV_DEMS.csv',stringsAsFactors = F)

stringr::str_match(NV$url,"3_\\s*(.*?)\\s*_2")
for(i in 1:nrow(NV)){
  print('reading raster')
  x<-raster::raster(NV$url[i])
  locs<-stringr::str_match(NV$url[i],"3_\\s*(.*?)\\s*_2")
  print('writing raster')
  raster::writeRaster(x,paste(Namestate,locs[,2],'10m.tif',sep='_'),'GTiff',overwrite=T)
  print('Ho-kay, mane. Onto next one!')
}
#for loop 1) download only necessary DEMs
for(i in 1:nrow(Lcoarse_pts)){
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
raster::writeRaster(rast,paste(Namestate,Lcoarse_pts$Lat[i],abs(Lcoarse_pts$Lon[i]),'10m.tif',sep='_'),'GTiff',overwrite=T)

print(paste('finished iteration',i,'of', nrow(Lcoarse_pts)))

}

#for loop 2) Watershed delineation and export
for(i in 1:length(unique(dfcoords$siteId))){
  #setwd('C://Users//andrew.caudillo//Box//NAMC//GIS//DEMs//TNM//NV_DEMs')
  setwd('C://Users//andrew.caudillo//Box//NAMC//GIS//DEMs//TNM')
  #subset out the first siteId
  print(paste('starting loop, iteration: ',i,sep=''))
  coord_subset<-dfcoords[dfcoords$siteId == unique(dfcoords$siteId)[i],]


  #this step fills the DEM, freeing it of any imperfections
  #that would obscure the flow of water. Ensure that a file path is given
  #or if not a file path, a file in the active working directory.
  #issues will arise if you hand it a URL

  wbt_breach_depressions_least_cost(
    dem=paste(Namestate,coord_subset$cLat,abs(coord_subset$cLon),'10m.tif',sep='_'),
    output=paste(Namestate,'fill',coord_subset$siteId,'_breached.tif',sep=''),
    dist=10,
    fill=T)
  #this step does the flow accumulation analysis, based on the filled DEM

  wbt_d8_flow_accumulation(input = paste(Namestate,'fill',coord_subset$siteId,"_breached.tif",sep=''),
                           output = paste(Namestate,'fill',coord_subset$siteId,"breached_acc.tif",sep=''))

  #this step does the flow direction analysis
  wbt_d8_pointer(dem = paste(Namestate,'fill',coord_subset$siteId,"_breached.tif",sep=''),
                 output = paste(Namestate,'fill',coord_subset$siteId,"_breached_dir.tif",sep=''))

#this is making a little dataframe of the pour point

  pps<-data.frame(coord_subset$Lon,coord_subset$Lat)
  pps<-SpatialPointsDataFrame(pps[,1:2],data=pps,proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs'))
#write the pour point out as its own shapefile. helps with QCing watersheds and pour points
  shapefile(pps, filename = paste(Namestate,"_pps",coord_subset$siteId,".shp",sep=''), overwrite = TRUE)
#this step gets the streams.not really used by NAMC, but required for the watershed process.

  wbt_extract_streams(flow_accum = paste(Namestate,coord_subset$siteId,"breached_acc.tif",sep=''),
                      output = paste(Namestate,"_streams",coord_subset$siteId,".tif",sep=''),
                      threshold = 6000)
#this step is snapping the pour points to the streams from above.

  wbt_jenson_snap_pour_points(pour_pts = paste(Namestate,"_pps",coord_subset$siteId,".shp",sep=''),
                              streams = paste(Namestate,"_streams",coord_subset$siteId,".tif",sep=''),
                              output = paste(Namestate,"snap_pps",coord_subset$siteId,".shp",sep=''),
                              snap_dist = 0.0005) #careful with this! Know the units of your data
  #this is the final step. the watershed delineation.

  wbt_watershed(d8_pntr = paste(Namestate,'fill',coord_subset$siteId,"_breached_dir.tif",sep=''),
                pour_pts = paste(Namestate,"snap_pps",coord_subset$siteId,".shp",sep=''),
                output = paste(Namestate,"_watersheds",coord_subset$siteId,".tif",sep=''))
  print(paste('Watershed ', i, ' delineated. Onto the next one.'))
}

#plot with mapview?

setwd('C://Users//andrew.caudillo//Box//NAMC//GIS//DEMs//bioclim')
elevWC<-raster::raster('wc2.1_30s_elev.tif')
library(maptools)
library(tigris)

yes<-tigris::states()
nv<-yes[yes$NAME=='Nevada',]
nv<-spTransform(nv,CRS(elevWC))
CRS(elevWC)
lilNV<-SpatialDataFrame(o[,1:2],o,proj4string = CRS("+proj=longlat +ellps=GRS80"))
o<-as.data.frame(nv$geometry[[1]][1])
CRS(nv)
shapefile(lilNV,filename='TIGER_NV')
clipNV30s<-raster::raster('NV_elev30s.tif')

clipper<-raster(resolution=c(9.259259e-05, 9.259259e-05),crs=proj4string(clipNV30s),ext=extent(clipNV30s))

rs<-resample(clipNV30s,clipper,method='ngb')
