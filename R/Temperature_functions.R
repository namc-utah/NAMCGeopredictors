pred_fns=ifelse(exists("pred_fns"),pred_fns, list())

####################

#   Temperature     #

####################

#################################################
### These are VERY LARGE vector datasets --- I am inclined to query what is needed directly from disk
### as opposed to load the entire vectors in memory
### Regular version of a function that works with an object loaded to memory
# pred_fns$SUMMER<-function(points2process,predictor_geometry, ...){
#   crs2use<-crs(predictor_geometry)
#   AOItrans<-sf::st_transform(points2process, crs2use)
#   AOI_Buffer<-sf::st_join(AOItrans, predictor_geometry, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
#   media<-AOI_Buffer$summer
#   return(media)
# }

#### Super fast version - loads into memory ONLY WHAT is strictly necessary
#' Summer stream temp at the point (CO MMI specific)
#'
#' @param points2process
#' @param predictor_geometry
#' @param geometry_input_path
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
pred_fns$SUMMER<-function(points2process,predictor_geometry,geometry_input_path, ...){
   AOItrans<- sf::st_transform(points2process, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>%
    sf::st_geometry() %>% # convert to sfc
    sf::st_buffer(150) %>% # buffer 150 meters
    sf::st_as_text() # convert to well known text
  SUMMER.vec<-sf::st_read(geometry_input_path, wkt_filter = AOItrans_wkt)
AOI_Buffer<-sf::st_join(AOItrans, SUMMER.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
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
#' Winter stream temp at the point (CO MMI specific but only needed for old model/ OE)
#'
#' @param points2process
#' @param predictor_geometry
#' @param geometry_input_path
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
pred_fns$WINTER<-function(points2process,predictor_geometry, geometry_input_path,...){
   AOItrans<-sf::st_transform(points2process, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>%
    sf::st_geometry() %>% # convert to sfc
    sf::st_buffer(150) %>% # buffer 150 meters
    sf::st_as_text() # convert to well known text
  WINTER.vec<-sf::st_read(geometry_input_path, wkt_filter = AOItrans_wkt)
AOI_Buffer<-sf::st_join(AOItrans, WINTER.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
media<-AOI_Buffer$winter
return(media[1,1])
}
#################################################


#' Temperature at the point (Oregon specific vector layer)
#'
#' @param points2process
#' @param predictor_geometry
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
pred_fns$temp<-function(points2process,predictor_geometry, ...){
   myvars <- "temp_Cx10"
  Pred_Input_All_USGS.vec <- predictor_geometry[myvars]
  crs2use<-crs(Pred_Input_All_USGS.vec) # is this the raster or sf function.... it is a vector
  points2process<-sf::st_transform(points2process, crs = crs2use)
  #Pred_Input_All_USGS.vec.WGS<-st_transform(Pred_Input_All_USGS.vec, crs = 4326)
  media<-sf::st_intersection(points2process, Pred_Input_All_USGS.vec)%>%pull(temp_Cx10)
  return(media[1,1])
}


#' Temperature max at the point (AND divide by 10)
#'
#' @param points2process
#' @param predictor_geometry
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
pred_fns$Tmax_PT<-function(points2process,predictor_geometry, ...){
   media<-raster::extract(predictor_geometry,points2process)/10
  return(media[1,1])
}


