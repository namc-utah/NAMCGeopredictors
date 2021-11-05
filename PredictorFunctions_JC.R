source("Config.R")
source('precip.R')
pred_fns = new.env( parent = emptyenv() )        

pred_fns$extract_watershed_mean <- function(polygon2process, predictor_name, predictor_geometry){
  sfobject<-geojsonio::geojson_sf(polygon2process)
  validgeometry<-sf::st_make_valid(sfobject)
  validgeometry[[predictor_name]]<-exactextractr:exact_extract(predictor_geometry,validgeometry,'mean')
  media<-as.data.frame(validgeometry[[predictor_name]])
  colnames(media)<-predictor_name
  return(media)
}

#' simple point value extraction
#'
#' @param point2process 
#' @param predictor_name 
#' @param predictor_geometry
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
pred_fns$extract_point_value <- function(point2process, predictor_name, predictor_geometry, ...){
  validgeometry<-geojsonio::geojson_sf(point2process)
  media<-raster::extract(predictor_geometry,validgeometry)
  return(media)
}

# SFRL<-function(point2process, predictor_name, predictor_geometry, ...){
#   validgeometry<-geojson_sf(points2process)
#   validgeometry<-st_transform(validgeometry, 5070)
#   biovar<-"LAST_COUNT"
#   WYBio<-predictor_geometry[biovar]
#   tempinter<-st_intersection(validgeometry, WYBio)
#   tempinter$predictor_name<-0
#   tempinter$predictor_name[tempinter$LAST_COUNT == "S WY FH & LARAMIE RANGE"]<-1
#   media<-tempinter$predictor_name
#   return(media)
# }
# 

