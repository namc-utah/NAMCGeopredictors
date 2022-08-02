##### Step 1 config file #####
#Run config file to load needed packages, boxid or projectId, and file paths


##### Step 2 Get coordinates and siteIds from database for box or project #####
if (exists("boxId")){
def_samples=NAMCr::query("samples",include = c("sampleId", "siteId", "siteLatitude","siteLongitude"),boxId=boxId)
}else {def_samples=NAMCr::query("samples",include = c("sampleId", "siteId", "siteLatitude","siteLongitude","waterbodyName"),projectId=projectId)
}
#convert to geospatial sf object
points= sf::st_as_sf(def_samples,coords=c("siteLongitude","siteLatitude"),crs=4269)
mapview(points)


##### Step 3 subset only points to watersheds that have not already been delineated #####
#get list of pointIds to query the mastersheds layer by
siteIds=unlist(unique(points$siteId))
#read in only watersheds from the mastersheds layer that are in the current box or project
existingWatersheds=sf::st_make_valid(sf::st_read(watershed_file_path, query=sprintf('SELECT * FROM %s WHERE siteId in(%s)',watershed_layer_name, inLOOP(substr(siteIds, 1, 10)))))
#view the watersheds on a map
mapview(existingWatersheds)
#subset points to siteIds not in the matersheds layer
points_subset=subset(points,!(siteId %in% existingWatersheds$siteId))


##### Step 4 Figure out which streamstats states coordinates fall into #####
# watersheds need to be submitted in batches by state to stream stats BUT
# streamstats coverage does not exactly match to political state boundaries due to watershed boundaries so we have created a polygon layer that matches streamstats state coverages
streamStatsState=st_read(paste0(pred_geometry_base_path, "GIS/StreamStatsGrids/StreamStatsState.shp"))
#attribute sites to delineate with this stateboundary layer
pointsState=st_intersection(points_subset,streamStatsState)
#view a unique list of states represented in the dataset
unique(pointsState$STATE_ABBR)

##### Step 5 generate two sets of points, points that need to be delineated manually, and those that can be delineated using StreamStats ######
#exclude points in the following states because they will need to manually delineated... WY and NV stream stats grids have not been completed
#and Jennifer could not get a polyline file to be created from the NM raster
points2process=subset(pointsState,!(STATE_ABBR %in% c('WY','NV','NM')))
#write out a shapefile for points that need to be manually delineated. Follow instructions in "C:\Users\jenni\Box\NAMC (Trip Armstrong)\GIS\Watersheds\HowToGuides.docx" to manually delineate these points using ArcGIS
#eventually use reticulate to call arcpy and delineate watersheds here
st_write(subset(pointsState,STATE_ABBR %in% c('WY','NV','NM')),paste0(pred_geometry_base_path,'GIS/Watersheds/ManualDelineations/',Sys.Date(),".shp"),append=FALSE)


##### Step 6 loop over points to snap points to stream stats polylines #####
# must snap points to stream stats polylines and not NHD to get proper watersheds delineated that match the DEM layers instead of topographic maps (NHD)
if (inherits(points2process, "sf")) n = nrow(points2process)
if (inherits(x, "sfc")) n = length(x)
#input max distance in meters that the point could be snapped. this is a function of the buffer size as well
#should not likely tweak these
max_dist=400
buffer_size=200
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
out_xy=cbind(out_xy,st_drop_geometry(points2process[,c('siteId','STATE_ABBR','waterbodyName')]),st_coordinates(points2process))
out_xy=dplyr::rename(out_xy,OrgLAT=Y)
out_xy=dplyr::rename(out_xy,OrgLONG=X)
#calculate the distance between the snapped point and the orignial point in meters
out_xy$distm=round(acos(sin(as.numeric(out_xy$OrgLAT)*3.141593/180)*sin(as.numeric(out_xy$SnpLAT)*3.141593/180) + cos(as.numeric(out_xy$OrgLAT)*3.141593/180)*cos(as.numeric(out_xy$SnpLAT)*3.141593/180)*cos(as.numeric(out_xy$SnpLONG)*3.141593/180-as.numeric(out_xy$OrgLONG)*3.141593/180)) * 6371000,digits=0)
#order points by siteId so that we get the points back from Streamstats in a consistent order
out_xy=out_xy[order(out_xy$siteId),]



##### Step 8 QC the output ####
#zoom in to an points that were moved more than 50 meters  and compare original (red)  and snapped point locations (blue)
mapview(out_xy)+
  mapview(points2process,col.regions="red")


#### Step 9 read out individual shapefiles for each state and QC further using QGIS, the NHD, and waterbody name if needed.
# Also need to QC points that were not moved at all because no stream stats line was close
states=unique(out_xy$STATE_ABBR)

for (s in 1:length(states)){
out_xysub=subset(out_xy,STATE_ABBR==states[s])
st_write(out_xysub,paste0(pred_geometry_base_path,'GIS/Watersheds/SnappedPointsSentStreamStats/',states[s],"_",Sys.Date(),".shp"),append=FALSE)

}

##### Step 10 Send to stream stats in batches of 200 points by state (one batch at a time!!- wait till we get results back from one batch before submitting another) #####
#https://streamstatsags.cr.usgs.gov/ss_bp/
#select get points back in same coordinate system as input NOT source layers

#When you first submit a request, they’ll send you an email saying how many people are in the queue ahead of you.
#The number is a little arbitrary because you don’t know if the requests ahead of you are for 3 points each or 200 points each,
#so the number in the queue doesn’t exactly tell you how long it’ll take. When you request is
#processed, you’ll get another email with a file with the watershed results. This can take anywhere from an hour to 3 days.



#### Step 11 QC watersheds we get back from stream stats
# Place output from stream stats in "GIS\Watersheds\StreamStatsOutput"
#read in streamstats output (watershed layer only)
shed <- st_read(streamStatsOutputFilePath, layer = "GlobalWatershed")
#read in corresponding points sent to streamcat
points<-st_read(SnappedPointsSentStreamStats)
#join back in siteId identifiers
sheds<- cbind(shed,points)
#check watersheds on map to make sure no weird watersheds
mapview(sheds)


#### Step 12 add watersheds to mastersheds layer
#read in mastersheds layer
mastersheds=st_read(watershed_file_path)
#subset columns to only include siteID and the geometry
shedsToAdd=subset(sheds[,c('siteId','geometry')])
#append new watersheds to mastersheds
mastershedsnew=rbind.fill(mastersheds,shedsToAdd)
#overwrite existing mastersheds layer
st_write(mastershedsnew,watershed_file_path)
