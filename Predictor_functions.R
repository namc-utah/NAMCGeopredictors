source("Config.R")

pred_fns = new.env( parent = emptyenv() )        

source("Geometry_functions.R")
source("Elevation_functions.R")
source("Ecoregion_functions.R")
source("Vegetation_functions.R")
source("Precipitation_functions.R")
source("Temperature_functions.R")
source("Hydrology_functions.R")
source("Slope_functions.R")


#####################

# Generic functions #

#####################

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
  polygon2process[[predictor_name]]<-exactextractr:exact_extract(predictor_geometry,polygon2process,'mean')
  media<-as.data.frame(polygon2process[[predictor_name]])
  colnames(media)<-predictor_name
  return(media[[predictor_name]])
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
pred_fns$extract_point_value <- function(point2process, predictor_geometry, ...){
    media<-raster::extract(predictor_geometry,point2process)
  return(media[1,1])
}




###### Day  ######   
pred_fns$DOY <- function(JulianDate,...) {
  media = JulianDate
  return(media[1,1])
}
