####################

#   Temperature     #

####################

#################################################
### These are VERY LARGE vector datasets --- I am inclined to query what is needed directly from disk 
### as opposed to load the entire vectors in memory
### Regular version of a function that works with an object loaded to memory
SUMMER<-function(points2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(points2process)
  crs2use<-crs(predictor_geometry)
  AOItrans<-st_transform(validgeometry, crs2use)
  AOI_Buffer<-st_join(AOItrans, predictor_geometry, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
  media<-AOI_Buffer$summer
  return(media)
}

#### Super fast version - loads into memory ONLY WHAT is strictly necessary
SUMMER<-function(points2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(points2process)
  AOItrans<-st_transform(validgeometry, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>% 
    st_geometry() %>% # convert to sfc
    st_buffer(150) %>% # buffer 150 meters
    st_as_text() # convert to well known text
  SUMMER.vec<-st_read(), wkt_filter = AOItrans_wkt)
AOI_Buffer<-st_join(AOItrans, SUMMER.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
media<-AOI_Buffer$summer
return(media)
}
#################################################

#################################################
### These are VERY LARGE vector datasets --- I am inclined to query what is needed directly from disk 
### as opposed to load the entire vectors in memory
### Regular version of a function that works with an object loaded to memory
WINTER<-function(points2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(points2process)
  crs2use<-crs(predictor_geometry)
  AOItrans<-st_transform(validgeometry, crs2use)
  AOI_Buffer<-st_join(AOItrans, predictor_geometry, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
  media<-AOI_Buffer$summer
  return(media)
}

#### Super fast version - loads into memory ONLY WHAT is strictly necessary
WINTER<-function(points2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(points2process)
  AOItrans<-st_transform(validgeometry, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>% 
    st_geometry() %>% # convert to sfc
    st_buffer(150) %>% # buffer 150 meters
    st_as_text() # convert to well known text
  WINTER.vec<-st_read(), wkt_filter = AOItrans_wkt)
AOI_Buffer<-st_join(AOItrans, WINTER.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
media<-AOI_Buffer$winter
return(media)
}
#################################################


temp<-function(points2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(points2process)
  myvars <- "temp_Cx10"
  Pred_Input_All_USGS.vec <- predictor_geometry[myvars]
  crs2use<-crs(Pred_Input_All_USGS.vec)
  validgeometry<-st_transform(validgeometry, crs = crs2use)
  #Pred_Input_All_USGS.vec.WGS<-st_transform(Pred_Input_All_USGS.vec, crs = 4326)
  media<-st_intersection(validgeometry, Pred_Input_All_USGS.vec)%>%pull(temp_Cx10)
  return(media)
}


Tmax_PT<-function(points2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(predictor_geometry,validgeometry)/10
  return(media)
}


