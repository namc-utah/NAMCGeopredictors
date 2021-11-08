##### lat ####

Lat_Dec<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-st_coordinates(validgeometry)[,2]
  return(media)
}

New_Lat<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-st_coordinates(validgeometry)[,2]
  return(media)
}

#' Latitude of the point
#' This function returns the Y coordinate of the point in decimal degrees
#' The st_coordinates function returns the second column [,2] which is the latitude
#' @param points2process 
#'
#' @return this functions returns one value which is the latitude of the point
#' @export
#'
#' @examples
DD_LAT_Y<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-sf::st_coordinates(validgeometry)[,2]
  return(media)
}




#### long ####
long<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-st_coordinates(validgeometry)[,1]
  return(media)
}

Lon_Dec<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-st_coordinates(validgeometry)[,1]
  return(media)
}


New_Long<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-st_coordinates(validgeometry)[,1]
  return(media)
}



#### watersheds ####

SQ_KM<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-drop_units(st_area(validgeometry)/1000000)
  return(media)
}

WSA_SQKM<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-drop_units(st_area(validgeometry)/1000000)
  return(media)
}

#' Area of the watershed in sq km
#' It obtains the area in square kilometers --> drop_units(st_area(validgeometry)/1000000) <-- for the watershed
#' @param polygon2process this is a geojson string for the watershed
#' The geojson is converted to an object of type sf -->validgeometry<-geojson_sf(polygon2process)<--
#' @return this functions returns one value which is the area of the watershed
#' @export
#'
#' @examples
AREA_SQKM<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-units::drop_units(sf::st_area(validgeometry)/1000000)
  return(media)
}

### different ###

LOG_KM2<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-log10(drop_units(st_area(validgeometry)/1000000))
  return(media)
}

