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
  return(media[1,1])
}

#' Percentage of watershed that is Sedimentary geology type (geology type number 5)
#'
#' @param polygon2process
#' @param predictor_geometry
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
PCT_SEDIM<-function(polygon2process,predictor_geometry, ...){
    crs2use<-crs(predictor_geometry) # raster or sf
    polygon2process<-sf::st_transform(polygon2process,crs=crs2use)
  PCT_SEDIM.vec<-sf::st_make_valid(predictor_geometry)
  geo01<-sf::st_intersection(PCT_SEDIM.vec, polygon2process)
  geo02<-sf::st_cast(geo01, "POLYGON")
  geo03<-geo02 %>%
    dplyr::mutate(AREA_SQKM = units::drop_units(sf::st_area(geo02)/1000000))%>%# update the AREA for subsequent calculations
    dplyr::mutate(PORC = ifelse(GEOnum == 5, round(AREA_SQKM/sum(AREA_SQKM)*100,2),0))
  geo03<-geo03%>%
    filter(GEOLOGY=='Sedimentary')
  media<-sum(geo03$PORC)
  return(media[1,1])
}
