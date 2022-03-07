###################

#   Geology       #

####################

#' Percent alfi soils in the watershed
#'
#' @param polygon2process
#' @param predictor_geometry
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
Pct_Alfi<-function(polygon2process,predictor_geometry, ...){
  polygon2process$AREAHA<-units::drop_units(st_area(polygon2process)/10000)
  polygon2process$Pct_Alfi_01<-exactextractr::exact_extract(predictor_geometry,polygon2process,'sum')
  polygon2process$Pct_Alfi<-(polygon2process$Pct_Alfi_01*25/polygon2process$AREAHA)*100
  media<-polygon2process$Pct_Alfi
  return(media)
}
