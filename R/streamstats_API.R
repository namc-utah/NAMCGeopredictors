#streamstats API
#written by Andrew Caudillo

#The API utilizes  the streamstats package in R
#because accessing the url streamstats creates
#(via sf) directly does not work anymore.
#HOWEVER
#the streamstats package returns an ugly, unworkable file
#called a "wastershed" file.
#sf doesn't recognize it and it cannot be worked with in
#R until it is a shape of sf file.
#We need to massage those data into something more workable
#Also, the R package is much faster than the webiste
#and you won't get in trouble for sending too many points in
#for delineation!
genpath<-'C://Users//andrew.caudillo//Box//NAMC//GIS//Watersheds//'
library(NAMCr)
library(mapview)
library(tidyverse)
library(sf)
library(streamstats)
#read in the sample info from INSTAR
#assume that these are NOT in
#WY or NV. If so, use nhd script!!

if(exists("boxId")){
points2process<-NAMCr::query(
  api_endpoint = "samples",
  args = list(boxId = boxId))
}else{
points2process<-NAMCr::query(
  api_endpoint = "samples",
  args = list(projectId=projectId))

}
MS<-st_read(watershed_file_path)
points2process<-points2process[points2process$siteId %in% MS$siteId ==F,]

#assigning a state abbreviation
#This only encompasses NAMC's main study region, but if we were
#working in a different region, we would need to edit this.
points2process$STATE_ABBR<-ifelse(points2process$usState=='California','CA',
                                  ifelse(points2process$usState=='Oregon','OR',
                                         ifelse(points2process$usState=='Washington','WA',
                                                ifelse(points2process$usState=='Idaho','ID',
                                                       ifelse(points2process$usState=='Utah','UT',
                                                              ifelse(points2process$usState=='Colorado','CO',
                                                                     ifelse(points2process$usState=='Arizona','AZ',
                                                                            ifelse(points2process$usState=='New Mexico','NM',
                                                                                   ifelse(points2process$usState=='Alaska','AK',
                                                                                       ifelse(points2process$usState=='Nevada','NV',
                                                                                           ifelse(points2process$usState=='Wyoming','WY','MT')))))))))))
#remove duplicate siteIds from a box (which does happen)
points2process<-points2process[!duplicated(points2process$siteId),]
points2process<-points2process[points2process$usState!='Alaska',]
#making the above oject an sf object
points2process= sf::st_as_sf(points2process,coords=c("siteLongitude","siteLatitude"),crs=4269)

#use this for rivnet or another delineation process
nonStStats<-points2process[points2process$STATE_ABBR %in% c('NV','WY'),]
#these don't need sheds and we don't have stream stats grids for them anyway
no_sheds_needed<-points2process[points2process$STATE_ABBR %in% c('AZ','NM'),]
points2process<-points2process[points2process$siteId %in% c(nonStStats$siteId, no_sheds_needed$siteId)==F,]
#set buffer distance for snapping etc.
if (inherits(points2process, "sf")) n = nrow(points2process)
#if (inherits(x, "sfc")) n = length(x)
max_dist=400
buffer_size=200

#snapping the points to streamstats flowgrids
# for each point in the points2process sf object get the streams and then snap the points to those lines
out = do.call(c,lapply(seq(n), function(i) {
  geometry_input_path=paste0(pred_geometry_base_path,"GIS/StreamStatsGrids/",points2process$STATE_ABBR[i],"_stream_stats_polyline.shp")
  # transform points from NAD83 to albers equal area conic USGS- all stream polylines are in albers and must be in albers (meters) not NAD83 (decimal degrees) to ensure that the buffer below gets the correct streams
  AOItrans<-sf::st_transform(points2process, 5070) # must use the same EPSG as in the shapefile
  #subset the sf object to only contain one point at a time
  AOItranss=AOItrans[i,"geometry"]
  #create well known text to query the stream layer by
  AOItrans_wkt <- AOItranss %>%
    sf::st_geometry() %>% # convert to sfc
    sf::st_buffer(buffer_size) %>% # buffer 200 meters
    sf::st_as_text() # convert to well known text
  #read in streams within a 200 m buffer of the point
  line_geometry<-sf::st_read(geometry_input_path, wkt_filter = AOItrans_wkt)


  # this part:
  # 1. loops through every input point
  # 2. snaps a point to the nearest line geometries
  # 3. calculates the distance from point to line geometries
  # 4. retains only the shortest distances and generates a point at that intersection
  nrst = st_nearest_points(st_geometry(AOItrans)[i], line_geometry)
  nrst_len = st_length(nrst)
  nrst_mn = which.min(nrst_len)
  if(length(nrst_len)==0) return(AOItrans$geometry[i])
  if (as.vector(nrst_len[nrst_mn]) > max_dist) return(AOItrans$geometry[i])
  return(st_cast(nrst[nrst_mn], "POINT")[2])
})
)


#convert list output to dataframe
out_xy <- st_coordinates(out) %>% as.data.frame()
#convert dataframe to sf object and tranform albers back to NAD83
out_xy <-out_xy %>%
  st_as_sf(coords=c("X","Y"), crs=5070, remove=FALSE)%>%
  st_transform(out_xy,crs=4269)
out_xy$SnpLONG=sf::st_coordinates(out_xy)[,1]
out_xy$SnpLAT=sf::st_coordinates(out_xy)[,2]
out_xy=subset(out_xy,select=-c(X,Y))
#Add in identifiers from orignial dataframe
out_xy=cbind(out_xy,st_drop_geometry(points2process[,c('siteId','STATE_ABBR')]),st_coordinates(points2process))
out_xy=dplyr::rename(out_xy,OrgLAT=Y)
out_xy=dplyr::rename(out_xy,OrgLONG=X)
#calculate the distance between the snapped point and the orignial point in meters
out_xy$distm=round(acos(sin(as.numeric(out_xy$OrgLAT)*3.141593/180)*sin(as.numeric(out_xy$SnpLAT)*3.141593/180) + cos(as.numeric(out_xy$OrgLAT)*3.141593/180)*cos(as.numeric(out_xy$SnpLAT)*3.141593/180)*cos(as.numeric(out_xy$SnpLONG)*3.141593/180-as.numeric(out_xy$OrgLONG)*3.141593/180)) * 6371000,digits=0)
#order points by siteId so that we get the points back from Streamstats in a consistent order
out_xy=out_xy[order(out_xy$siteId),]

#Use the API for streamstats via
#their R package.
#remember that the output is not user friendly!
#so we will manipulate the output into something more
#familiar.

#just getting coords (and nothing else) from out_xy
snapt<-as.data.frame(st_coordinates(out_xy))
#make an empty list with a crs so we can
#add sheds to it.
STshed_list<-st_sfc(crs=4269)
#issue_points<-matrix(ncol=6,nrow=nrow(out_xy)) #everything except the geometry, which will be difficult to store in a matrix

for(i in 1:nrow(snapt)){
  #the if statement will remove a previous
  #"test" so the loop can work properly.
  #see "test" below deeper in the loop

  print(i)
  if(i>1){
    if(exists("o"))
    rm(o)
  }else{}

  #tryCatch is saying if the link doesn't work or there is a weird
  #curl error, store that iteration's info in its own list
  #so we can assess the damage later
  tryCatch({
    #this function from streamstats
    #is essentially making the url that streamstats hides the
    #geojson in behind the scenes

  ws<-streamstats::delineateWatershed(xlocation = snapt$X[i],
                                      ylocation = snapt$Y[i],
                                      includefeatures = 'true',includeparameters = 'false',
                                      includeflowtypes = 'false',
                                      crs=4269,rcode = out_xy$STATE_ABBR[i])
  #just isolate the watershed coords from the ugly feature the package makes
  o<-ws$featurecollection[[2]]$feature$features[[1]]$geometry$coordinates
  },
  error=function(e){
    #several issues can arise when making the watershed
    #1) curl timeout. No way around that.
    #2) the API breaks
    #so the error will jump to the next iteration, if that is the case.
    writeLines('the site is taking too long\nor there is an error with the API\nthis siteId is saved in "issue_points"')
    message(paste('site ID ',out_xy$siteId[i]))
    message('Moving onto the next iteration')
  }
  )

  #this tryCatch is seeing if the point really did make it to a grid
  #if you cannot get "o" to exist, then there is no watershed file,
  #just a pour point.
  #if that is the case, then assign that iteration to its own little list
  #so we can assess the damage later.
  #this is just saying if test exists,
  #then go to the rest of the loop. if not, NEXT
  #if "test" does exist, then parse out the watershed files
  #and rbind the list.
  if(exists("o")){
  #unlist the coords into a vector
  ws_raw<-unlist(o)
  #isolate even numbers (the latitudes), since we know that
  #"o" consists of lists made up two values each: Latitude and Longitudes,
  #respectively.
  Lats<-ws_raw[seq_along(ws_raw) %%2 == 0]
  #now isolate the odd values (Longitudes)
  Lons<-ws_raw[seq_along(ws_raw) %%2 == 1]
  #set up df to be lon/lat
  raw_df<-data.frame(Lons,Lats);names(raw_df)<-c('X','Y')
  #make the df into an sf object
  sf_shed <- raw_df %>%
    st_as_sf(coords = c("X", "Y"), crs = 4269) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
  #combine all the sf objects into one file
  sf_shed$siteId<-out_xy$siteId[i]
  STshed_list = rbind(STshed_list,sf_shed)

  message(paste('watershed number ', i, ' of ',nrow(out_xy),' delineated and saved!'))
  }else{
    #if not, then jump to the next iteration.
    #FYI- "next" cannot go into trycatch because it is technically not
    #inside the for loop and has nothing to "next" to.
    #if next does not live in the loop itself, throw it into an
    #if/else statement instead.
    next
  }
}
#plot it to see if it makes sense
#basically check for little squares or triangles for a shed.
#also look for really geometric boundaries, like a triangle removed from a larger
#polygon.
mapview(STshed_list,col.regions='red')
#manually inspect for really weird sheds, like a triangle or a square
#and omit them from the big list.
#use another method to delineate, like rivnet.
ops<-STshed_list[STshed_list$siteId %in% c(45177,45292,45097,45104,45095),]
p<-STshed_list[which(STshed_list$siteId%in%c(45177,45292,45097,45104,45095)==F),]
mapview(p,col.regions='yellow')
STshed_list<-p
mapview(STissue_points)

#use another method, like rivnet for these and the "nonStStats" sheds
leftovers<-out_xy[out_xy$siteId %in% p$siteId==F | out_xy$siteId %in% ops$siteId,]
#shed_list$siteid<-out_xy$siteId

st_write(p,dsn=paste0(genpath,'AIM_2023_leftovers_streamstats','.shp'),append = F)
st_write(leftovers,dsn=paste0(genpath,'AIM_2023_leftovers_issuepoints_needsheds','.shp'),append = F)
write.csv(nonStStats,paste0(genpath,'AIM_2023_leftovers_nonStreamStats','.csv'),row.names=F)
