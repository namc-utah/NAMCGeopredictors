source("Config.R")

pred_fns = new.env( parent = emptyenv() )        

source("")





#' simple watershed mean, raster used is specified in the database under the geometry_file_path
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
pred_fns$extract_watershed_mean <- function(polygon2process, predictor_name, predictor_geometry, ...){
  sfobject<-geojsonio::geojson_sf(polygon2process)
  validgeometry<-sf::st_make_valid(sfobject)
  validgeometry[[predictor_name]]<-exactextractr:exact_extract(predictor_geometry,validgeometry,'mean')
  media<-as.data.frame(validgeometry[[predictor_name]])
  colnames(media)<-predictor_name
  return(media)
}

#' simple point value extraction, raster used is specified in the database under the geometry_file_path
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


####################

#       Day        #

####################

DOY <- function(JulianDate) {
  media = JulianDate
  return(media)
}

