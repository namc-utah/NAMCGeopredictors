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


#for loop 1) download only necessary DEMs
#and make an empty list for storing the results
temp_RASTER_path<-tempdir()
df<-st_read(paste0(genpath,'AIM_2023_leftovers_issuepoints_needsheds','.shp'))
shed_list<-st_sfc(crs=4269)
adjs=seq(0.1,0.3,by=0.1)
for(i in 1:3){#nrow(df)){
if(i >1){
  unlink(paste0(temp_RASTER_path,'/*'))
}
  if(exists("r")){
    rm(r)
  }

  print(i)

  #remove any temp folder that R creates
  #to not let elevatr build up a lot of junk
  #tempdir()
  #temp_files<-list.files("C://Users//ANDREW~1.CAU//AppData//Local//Temp",full.names = T)
  #R_temps<-temp_files[grepl("Rtmp",temp_files)]
  #unlink(R_temps,recursive = T)

old <- Sys.time() # get start time
Lat<-df$OrgLAT[i]
Lon<-df$OrgLONG[i]

#temp_RASTER_path<-gsub('.{7}$','',temp_RASTER_path)
print('attempting shed extraction')
for(j in 1:length(adjs)){
  print(paste0('extraction try ',j))
  withRestarts(
tryCatch({
r <- extract_river(outlet = c(Lon,Lat),
                   ext=c(Lon-adjs[j],Lon+adjs[j],
                         Lat-adjs[j],Lat+adjs[j]),
                   EPSG = 4269,
                   n_processes = 8,z=13)
if(exists("r")){
  print('extraction done! Processing shed')
  break}
},
error=function(e){
  invokeRestart("retry")}
),
  retry=function(){
    message("issue with DEM extent, trying another DEM")
  } #retry
) #with restarts

} #i
#the river object stores a lot of neat stuff
#some that we don't need for watershed delineation
#what we do want is "catchment" or CM
#but it is stored in a wieird list format
#so we will unlist it, then take the raw coords and make them
#into an sf object

if(j==3){
  next
}
o<-unlist(r$CM)
#this is synonymous with the LIKE
#operator in SQL. We want only values
#whose columns contain X or Y countour
xs<-o[ grepl( "XContour" , names( o ) ) ]
ys<-o[ grepl( "YContour" , names( o ) ) ]
#make a little df
raw_shed_coords<-data.frame(xs,ys)
#change the little df names
names(raw_shed_coords)<-c('X','Y');row.names(raw_shed_coords)<-NULL
#make the shed into an sf object
sf_shed <- raw_shed_coords %>%
  st_as_sf(coords = c("X", "Y"), crs = 4269) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
#fill the list
shed_list<-rbind(shed_list,sf_shed)
print(paste('finished iteration',i,'of', nrow(df)))
# print elapsed time
new <- Sys.time() - old # calculate difference
print(new)
}

mapview(shed_list)


Lon<-df$OrgLONG[1]
Lat=df$OrgLAT[1]
r <- extract_river(outlet = c(Lon,Lat),
                   ext=c(Lon-adjs[1],Lon+adjs[1],
                         Lat-adjs[1],Lat+adjs[1]),
                   EPSG = 4269,
                   n_processes = 8,z=13)

tempdir()
unlink("C://Users//ANDREW~1.CAU//AppData//Local//Temp//RtmpMl1ma7/*")
