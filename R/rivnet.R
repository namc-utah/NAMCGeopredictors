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

boxnum<-8809
#query the box in question
x<-NAMCr::query(
  api_endpoint = "sites",
  args = list(boxIds = boxnum))

#df<-out_xy[out_xy$siteId %in% c(45552,45556)==F,]
#for loop 1) download only necessary DEMs
#and make an empty list for storing the results
temp_RASTER_path<-tempdir()
#df<-st_read(paste0(genpath,'AIM_2023_leftovers_issuepoints_needsheds','.shp'))

shed_list<-st_sfc(crs=4269)
adjs=seq(0.1,1,by=0.1)
for(i in 1:nrow(x)){#nrow(df)){
  #this line removes an existing shed, if it exists
  if(exists("r")){
    rm(r)
  }else{
    message('No sheds exist yet or the first shed was unable to run')
  }

  print(i)


old <- Sys.time() # get start time
Lat<-x$latitude[i]
Lon<-x$longitude[i]
#force R to save the files from elevatr here
temp_RASTER_path<-gsub('.{7}$','',temp_RASTER_path)
message('attempting shed extraction')
for(j in 1:length(adjs)){

  message(paste0('extraction try ',j))
  withRestarts(
    #clear the temp path of any elevatr tiles
tryCatch({
  if(i >1){
    unlink(paste0(temp_RASTER_path,'/*'))
  }
  #extract the river/shed (you get both)
r <- extract_river(outlet = c(Lon,Lat),
                   ext=c(Lon-adjs[j],Lon+adjs[j],
                         Lat-adjs[j],Lat+adjs[j]),
                   EPSG = 4269,
                   n_processes = 8,z=13)
#if r was successfully created, then let the user know
if(exists("r")){
  message('extraction done! Processing shed')
  break}
},
#else, jump back up to the outer loop and try the next DEM size
error=function(e){
  invokeRestart("retry")}
),
#let the user know why you had to restart
  retry=function(){
    message("issue with DEM extent, trying another DEM")
  } #retry
) #with restarts

} #i

#the river object stores a lot of neat stuff
#some that we don't need for watershed delineation
#what we do want is "catchment" or CM
#but it is stored in a weird list format
#so we will unlist it, then take the raw coords and make them
#into an sf object

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
message(paste('finished iteration',i,'of', nrow(df)))
# print elapsed time
new <- Sys.time() - old # calculate difference
print(new)
}

mapview(shed_list,col.regions='red',col='red')

if(0){
Lon<-df$OrgLONG[1]
Lat=df$OrgLAT[1]
r <- extract_river(outlet = c(Lon,Lat),
                   ext=c(Lon-adjs[1],Lon+adjs[1],
                         Lat-adjs[1],Lat+adjs[1]),
                   EPSG = 4269,
                   n_processes = 8,z=13)
}
