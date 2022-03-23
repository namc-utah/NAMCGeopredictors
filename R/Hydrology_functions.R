####################

#   Hydrology      #

####################

#' Area of the largest waterbody in the watershed
#'
#' @param polygon2process
#' @param predictor_geometry
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
Wb_mx_area<-function(polygon2process,predictor_geometry, ...){
  polygon2process<-sf::st_transform(polygon2process, 5070)
    bodies<-sf::st_intersection(predictor_geometry, polygon2process)
  bodies$AreaSqKm<-units::drop_units(sf::st_area(bodies)/1000000)
  media<-ifelse(is.infinite(max(bodies$AreaSqKm)),0,max(bodies$AreaSqKm))
  return(media)
}


#' Ground Water Index across the watershed
#'
#' @param polygon2process
#' @param predictor_geometry
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
GW_P_Sp_Mx<-function(polygon2process,predictor_geometry, ...){
   media<-exactextractr::exact_extract(predictor_geometry,polygon2process,'max')
  return(media)
}
