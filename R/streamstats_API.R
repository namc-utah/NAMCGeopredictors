#streamstats API
#using the streamstats package in R
#because accessing the url directly does not work anymore.
#HOWEVER
#the streamstats package returns an ugly, unworkable file
#called a "wastershed" file. We need to
#massage those data into something more workable
#like an sf or shp file.
#Also, the R package is much faster than the webiste
#and you won't get in trouble for sending too many points in
#for delineation!
library(NAMCr)
library(mapview)
library(tidyverse)
library(sf)
#read in the sample info from INSTAR
#assume that these are NOT in
#WY or NV. If so, use nhd script!!
points2process<-query(
  api_endpoint = "samples",
  args = list(boxId = boxId))
#base path for reading in files
pred_geometry_base_path="C://Users//andrew.caudillo//Box//NAMC//"
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
                                                                                   'MT'))))))))
points2process<-points2process[!duplicated(points2process$siteId),]
#making the above oject an sf object
points2process= sf::st_as_sf(points2process,coords=c("siteLongitude","siteLatitude"),crs=4269)
#set buffer distance for snapping etc.
if (inherits(points2process, "sf")) n = nrow(points2process)
if (inherits(x, "sfc")) n = length(x)
max_dist=400
buffer_size=200

#snapping the points to streamstats flowgrids
# for each point in the points2process sf object get the streams and then snap the points to those lines
out = do.call(c,lapply(seq(n), function(i) {
  # Step 6a Get state specific StreamStats lines to snap to but only lines within a 200 meter buffer of the point to save memory and time
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

  #Step 6b loop through snapping each point to the lines
  # this part:
  # 1. loops through every piece of data (every point)
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


##### Step 7 format the ouput #####
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

#Step 8) Use the API for streamstats via
#their R package.
#remember that the output is not user friendly!
#so we will manipulate the output into something more
#familiar.

#just getting coords from out_xy
snapt<-as.data.frame(st_coordinates(out_xy))
#make an empty list with a crs so we can
#add sheds to it.
shed_list<-st_sfc(crs=4269)

for(i in 1:nrow(out_xy)){
  print(i)
  #essentially making the url that streamstats creates
  ws<-streamstats::delineateWatershed(xlocation = snapt$X[i],
                                      ylocation = snapt$Y[i],
                                      includefeatures = 'true',includeparameters = 'false',
                                      includeflowtypes = 'false',
                                      crs=4269,rcode = out_xy$STATE_ABBR[i])
  #isolate just the geometry/coordinates
  o<-ws$featurecollection[[2]]$feature$features[[1]]$geometry$coordinates
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
  #combine all the sf objects into one nasty file
  shed_list = rbind(shed_list,sf_shed)
  print(paste('watershed number ', i, ' of ',nrow(out_xy),' delineated and saved!'))
}
mapview(shed_list)

#write out shapefile to merge with mastersheds

shed_list$siteid<-out_xy$siteId

st_write(shed_list,dsn='C://Users//andrew.caudillo//Box//NAMC//GIS//Watersheds//nhdPlusTools//nv_sheds_231101.shp',append = F)
