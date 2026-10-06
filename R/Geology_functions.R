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

#' Percent of different components of soils in the watershed
#'
#' @param polygon2process
#' @param predictor_geometry
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
soils <- function(polygon2process,predictor_geometry, ...){
  media <- terra::extract(predictor_geometry, polygon2process, fun=mean, ID=T, touches = T, na.rm =T)
  return(media[,2])}



#' percent of the watershed with glaciers
#'
#' @param polygon2process
#' @param predictor_geometry
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

glaciers<-function(polygon2process,predictor_geometry, ...){
  shed_terra <- terra::vect(polygon2process)
  Glaciers <- terra::project(terra::vect(predictor_geometry), terra::crs(shed_terra))
#Glaciers <- terra::aggregate(predictor_geometry)
shed_terra$PropGlacier <- 0
  CroppedWatershed <- terra::crop(shed_terra, Glaciers)

  if(length(CroppedWatershed)==1){
    GlacierKM <- terra::expanse(CroppedWatershed, unit="km")
    GlacierProp <- GlacierKM/terra::expanse(shed_terra, unit="km")
  } else {
    GlacierProp <- 0
  }
  return(GlacierProp)
}
