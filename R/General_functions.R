pred_fns=ifelse(exists("pred_fns"),pred_fns, list())

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
#' get Julian day and change column name to DOY
#'
#' @param JulianDate
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
pred_fns$DOY <- function(JulianDate,...) {
  media = JulianDate
  return(media[1,1])
}
