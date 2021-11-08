####################

#     Geometry     #

####################


##### lat ####
#' Latitude of the point
#' This function returns the Y coordinate of the point in decimal degrees
#' The st_coordinates function returns the second column [,2] which is the latitude
#' @param points2process 
#'
#' @return this functions returns one value which is the latitude of the point
#' @export
#'
#' @examples
pred_fns$lat<-function(points2process){
  validgeometry<-geojsonio::geojson_sf(points2process)
  media<-sf::st_coordinates(validgeometry)[,2]
  return(media)
}


#### long ####
pred_fns$long<-function(points2process){
  validgeometry<-geojsonio::geojson_sf(points2process)
  media<-sf::st_coordinates(validgeometry)[,1]
  return(media)
}


#### watersheds ####

#' Area of the watershed in sq km
#' It obtains the area in square kilometers --> drop_units(st_area(validgeometry)/1000000) <-- for the watershed
#' @param polygon2process this is a geojson string for the watershed
#' The geojson is converted to an object of type sf -->validgeometry<-geojson_sf(polygon2process)<--
#' @return this functions returns one value which is the area of the watershed
#' @export
#'
#' @examples
pred_fns$WSA_SQKM<-function(polygon2process){
  validgeometry<-geojsonio::geojson_sf(polygon2process)
  media<-drop_units(st_area(validgeometry)/1000000)
  return(media)
}

pred_fns$LOG_WSA_SQKM<-function(polygon2process){
  media<-log10(WSA_SQKM(polygon2process))
  return(media)
}

