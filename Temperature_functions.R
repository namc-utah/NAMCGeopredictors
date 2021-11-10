####################

#   Temperature     #

####################

#################################################
### These are VERY LARGE vector datasets --- I am inclined to query what is needed directly from disk 
### as opposed to load the entire vectors in memory
### Regular version of a function that works with an object loaded to memory
# pred_fns$SUMMER<-function(points2process,predictor_geometry, ...){
#   crs2use<-crs(predictor_geometry)
#   AOItrans<-st_transform(points2process, crs2use)
#   AOI_Buffer<-st_join(AOItrans, predictor_geometry, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
#   media<-AOI_Buffer$summer
#   return(media)
# }

#### Super fast version - loads into memory ONLY WHAT is strictly necessary
pred_fns$SUMMER<-function(points2process,predictor_geometry,geometry_input_path, ...){
   AOItrans<-st_transform(points2process, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>% 
    st_geometry() %>% # convert to sfc
    st_buffer(150) %>% # buffer 150 meters
    st_as_text() # convert to well known text
  SUMMER.vec<-st_read(geometry_input_path, wkt_filter = AOItrans_wkt)
AOI_Buffer<-st_join(AOItrans, SUMMER.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
media<-AOI_Buffer$summer
return(media[1,1])
}
#################################################

#################################################
### These are VERY LARGE vector datasets --- I am inclined to query what is needed directly from disk 
### as opposed to load the entire vectors in memory
### Regular version of a function that works with an object loaded to memory
# pred_fns$WINTER<-function(points2process,predictor_geometry, ...){
#    crs2use<-crs(predictor_geometry)
#   AOItrans<-st_transform(points2process, crs2use)
#   AOI_Buffer<-st_join(AOItrans, predictor_geometry, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
#   media<-AOI_Buffer$summer
#   return(media)
# }

#### Super fast version - loads into memory ONLY WHAT is strictly necessary
pred_fns$WINTER<-function(points2process,predictor_geometry, geometry_input_path,...){
   AOItrans<-st_transform(points2process, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>% 
    st_geometry() %>% # convert to sfc
    st_buffer(150) %>% # buffer 150 meters
    st_as_text() # convert to well known text
  WINTER.vec<-st_read(geometry_input_path, wkt_filter = AOItrans_wkt)
AOI_Buffer<-st_join(AOItrans, WINTER.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
media<-AOI_Buffer$winter
return(media[1,1])
}
#################################################


pred_fns$temp<-function(points2process,predictor_geometry, ...){
   myvars <- "temp_Cx10"
  Pred_Input_All_USGS.vec <- predictor_geometry[myvars]
  crs2use<-crs(Pred_Input_All_USGS.vec)
  points2process<-st_transform(points2process, crs = crs2use)
  #Pred_Input_All_USGS.vec.WGS<-st_transform(Pred_Input_All_USGS.vec, crs = 4326)
  media<-st_intersection(points2process, Pred_Input_All_USGS.vec)%>%pull(temp_Cx10)
  return(media[1,1])
}


pred_fns$Tmax_PT<-function(points2process,predictor_geometry, ...){
   media<-raster::extract(predictor_geometry,points2process)/10
  return(media[1,1])
}


