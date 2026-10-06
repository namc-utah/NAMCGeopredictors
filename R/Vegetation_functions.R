####################

#       Veg        #

####################

#' Percent of Alru landcover in the watershed
#' This functions takes the GIS_Stats/Vegetation/Data/alru_dom dataset
#' First it obtains the area in hectares --> drop_units(st_area(polygon2process)/10000) <-- for the watershed
#' Then it obtains zonal statistics - the number of pixels of alru_dom -->exact_extract(alru_dom.ras,polygon2process,'count') <--
#' Because each pixel is 30m it calculates the area of alru_dom by multiplying the count * 0.09 and then divides by the area to
#' obtain percentage
#'
#' @param polygon2process this is a geojson string for the watershed
#' @param predictor_geometry
#' @param ...
#' The geojson is converted to an object of type sf -->polygon2process<-geojson_sf(polygon2process)<--
#' @return this functions returns one value which is the percentage of alru_dom
#' @export
#'
#' @examples
alru_dom<-function(polygon2process,predictor_geometry, ...){
  polygon2process$AREAHA<-units::drop_units(st_area(polygon2process)/10000)
  polygon2process$alru_dom_01<-exactextractr::exact_extract(predictor_geometry,polygon2process,'count')
  media<-(polygon2process$alru_dom_01*0.09/polygon2process$AREAHA)*100
  return(media)
}


#' Percent of Evergreen landcover in the watershed
#' dataset used GIS_Stats/Vegetation/Data/evergr
#' The function first obtains the watershed area in hectares drop_units(st_area(polygon2process)/10000)
#' It then uses the exact_extract function to obtain zonal statistics for the raster, the sum of pixels within the watershed
#' The sum is obtained because all the pixels have a value of 1. Then in converts the sum of pixels to area
#' by multiplying 0.09 (30 x 30 m pixels equal 900 m2 equal 0.09 hectares). This value is then divided by the
#' area of the watershed and the percent (in decimal units is extracted)
#'
#' @param polygon2process
#' @param predictor_geometry
#' @param ...
#'
#' @return a single value of percent of evergreen landcover in the watershed in decimal values
#' @export
#'
#' @examples
Evergr_ave<-function(polygon2process,predictor_geometry, ...){
  polygon2process$AREAHA<-units::drop_units(st_area(polygon2process)/10000)
  polygon2process$Evergr_ave_01<-exactextractr::exact_extract(predictor_geometry,polygon2process,'sum')
  polygon2process$Evergr_ave<-(polygon2process$Evergr_ave_01*0.09/polygon2process$AREAHA)
  media<-polygon2process$Evergr_ave
  return(media)
}


#' Percent of landcover in the watershed
#'
#' @param polygon2process
#' @param predictor_geometry
#' @param predictor_name
#' @param ...
#'
#' @return a single value of percent of evergreen landcover in the watershed in decimal values
#' @export
#'
NLCD<-function(polygon2process,predictor_geometry, predictor_name,...){
  shed_terra <- terra::vect(polygon2process)
  shed_terra <- terra::project(shed_terra, terra::crs(predictor_geometry))
  WatershedCover <- terra::crop(predictor_geometry,shed_terra, mask = T)
  WatershedCover <- freq(WatershedCover)
  TotalArea <- sum(WatershedCover$count)
  WatershedCover$Proportion <- WatershedCover$count/TotalArea
  WatershedCover$Proportion[is.na(WatershedCover$Proportion)] <- 0
    if(predictor_name=='Proportion_Dwarf_Shrub'){
  media=subset(WatershedCover,value=='Dwarf Shrub')
} else if(predictor_name=='Proportion_Open_Water'){
  media=subset(WatershedCover,value=='Open Water')
}else if(predictor_name=='Prop_Perennial_Ice_Snow'){
  media=subset(WatershedCover,value=='Perennial Ice/Snow')
}else{}
  return(media[,4])
}

