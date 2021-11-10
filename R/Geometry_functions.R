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
   media<-sf::st_coordinates(points2process)[,2]
  return(media[1,1])
}


#### long ####
pred_fns$long<-function(points2process){
   media<-sf::st_coordinates(points2process)[,1]
  return(media[1,1])
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
   media<-units::drop_units(sf::st_area(polygon2process)/1000000)
  return(media[1,1])
}

pred_fns$LOG_WSA_SQKM<-function(polygon2process){
  media<-log10(WSA_SQKM(polygon2process))
  return(media[1,1])
}

