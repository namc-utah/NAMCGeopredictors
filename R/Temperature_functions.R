####################

#   Temperature     #

####################

#################################################
### These are VERY LARGE vector datasets --- I am inclined to query what is needed directly from disk
### as opposed to load the entire vectors in memory
### Regular version of a function that works with an object loaded to memory
# SUMMER<-function(point2process,predictor_geometry, ...){
#   crs2use<-crs(predictor_geometry)
#   AOItrans<-sf::st_transform(point2process, crs2use)
#   AOI_Buffer<-sf::st_join(AOItrans, predictor_geometry, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
#   media<-AOI_Buffer$summer
#   return(media)
# }

#### Super fast version - loads into memory ONLY WHAT is strictly necessary
#' Summer stream temp at the point (CO MMI specific)
#'
#' @param point2process
#' @param predictor_geometry
#' @param geometry_input_path
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
SUMMER<-function(point2process,predictor_geometry,geometry_input_path, ...){
   AOItrans<- sf::st_transform(point2process, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>%
    sf::st_geometry() %>% # convert to sfc
    sf::st_buffer(150) %>% # buffer 150 meters
    sf::st_as_text() # convert to well known text
  SUMMER.vec<-sf::st_read(geometry_input_path, wkt_filter = AOItrans_wkt)
AOI_Buffer<-sf::st_join(AOItrans, SUMMER.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
media<-AOI_Buffer$summer
return(media)
}
#################################################

#' #################################################
#' ### commented out because old CO model, predictor no longer used
#' #' These are VERY LARGE vector datasets --- I am inclined to query what is needed directly from disk
#' #### Super fast version - loads into memory ONLY WHAT is strictly necessary
#' #' Winter stream temp at the point (CO MMI specific but only needed for old model/ OE)
#' #'
#' #' @param point2process
#' #' @param predictor_geometry
#' #' @param geometry_input_path
#' #' @param ...
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' WINTER<-function(point2process,predictor_geometry, geometry_input_path,...){
#'    AOItrans<-sf::st_transform(point2process, 5070) # must use the same EPSG as in the shapefile
#'   AOItrans_wkt <- AOItrans %>%
#'     sf::st_geometry() %>% # convert to sfc
#'     sf::st_buffer(150) %>% # buffer 150 meters
#'     sf::st_as_text() # convert to well known text
#'   WINTER.vec<-sf::st_read(geometry_input_path, wkt_filter = AOItrans_wkt)
#' AOI_Buffer<-sf::st_join(AOItrans, WINTER.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
#' media<-AOI_Buffer$winter
#' return(media)
#' }
#' #################################################
#'

#' Temperature at the point (Oregon specific vector layer)
#'
#' @param point2process
#' @param predictor_geometry
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
temp<-function(point2process,predictor_geometry, ...){
  point2process<-sf::st_transform(point2process, st_crs(predictor_geometry))
  media<-sf::st_intersection(point2process, predictor_geometry)%>%pull(temp_Cx10)
  return(media)
}


#' Temperature max at the point (AND divide by 10)
#'
#' @param point2process
#' @param predictor_geometry
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
Tmax_PT<-function(point2process,predictor_geometry, ...){
   media<-raster::extract(predictor_geometry,sf::as_Spatial(point2process))/10
  return(media)
}


#' Temperature max in the watershed (AND divide by 10)
#'
#' @param polygon2process
#' @param predictor_geometry
#' @param predictor_name
#' @param ...
#' @return
#' @export
#'
#' @examples

Tmax_WS<-function(polygon2process,predictor_name,predictor_geometry,...){
  media<-extract_watershed_mean(polygon2process,predictor_name,predictor_geometry)/10
}
