source("Config.R")

pred_fns = new.env( parent = emptyenv() )        

pred_fns$extract_wshed_predictor <- function(polygon2process, predictor_name, predictor_raster, formula_type){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry[[predictor_name]]<-exact_extract(predictor_raster,validgeometry, formula_type)
  media<-as.data.frame(validgeometry[[predictor_name]])
  colnames(media)<-predictor_name
  return(media)
}

#' Title
#'
#' @param point2process 
#' @param predictor_name 
#' @param predictor_raster 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
pred_fns$extract_point_predictor <- function(point2process, predictor_name, predictor_raster, ...){
  validgeometry<-geojson_sf(point2process)
  media<-raster::extract(predictor_raster,validgeometry)
  return(media)
}


pred_fns$extract_temporal_predictor <- function(polygon2process, predictor_name, predictor_raster, formula_type){


}

# 
# BDH_AVE<-function(polygon2process){
#   validgeometry<-geojsonio::geojson_sf(polygon2process)
#   media<-exactextractr::exact_extract(BDH_AVE.ras,validgeometry,'mean')
#   return(media)
# }
# 
# CaO_Mean<-function(polygon2process){
#   validgeometry<-geojson_sf(polygon2process)
#   media<-exact_extract(CaO_Mean.ras,validgeometry,'mean')
#   return(media)
# }
# 
# KFCT_AVE<-function(polygon2process){
#   validgeometry<-geojson_sf(polygon2process)
#   media<-exact_extract(KFCT_AVE.ras,validgeometry,'mean')
#   return(media)
# }
# 
# LPREM_mean<-function(polygon2process){
#   validgeometry<-geojson_sf(polygon2process)
#   media<-exact_extract(LPREM_mean.ras,validgeometry,'mean')
#   return(media)
# }
# 
# PRMH_AVE<-function(polygon2process){
#   validgeometry<-geojson_sf(polygon2process)
#   media<-exact_extract(PRMH_AVE.ras,validgeometry,'mean')
#   return(media)
# }
# 
# S_Mean<-function(polygon2process){
#   validgeometry<-geojson_sf(polygon2process)
#   media<-exact_extract(S_Mean.ras,validgeometry,'mean')
#   return(media)
# }
# 
# SumAve_P<-function(polygon2process){
#   validgeometry<-geojson_sf(polygon2process)
#   media<-exact_extract(SumAve_P.ras,validgeometry,'mean')
#   return(media)
# }
# 
# TMAX_WS<-function(polygon2process){
#   validgeometry<-geojson_sf(polygon2process)
#   media<-exact_extract(TMAX_WS.ras,validgeometry,'mean')
#   return(media)
# }
# 
# XWD_WS<-function(polygon2process){
#   sfobject<-geojson_sf(polygon2process)
#   validgeometry<-st_make_valid(sfobject)
#   validgeometry$XWD_WS<-exact_extract(XWD_WS.ras,validgeometry,'mean')
#   media<-as.data.frame(validgeometry$XWD_WS)
#   colnames(media)<-"XWD_WS"
#   return(media)
# }
# 
# AtmCa<-function(points2process, predictor_name){
#   validgeometry<-geojson_sf(points2process)
#   media<-raster::extract(AtmCa.ras,validgeometry)
#   return(media)
# }
# 
# AtmMg<-function(points2process){
#   validgeometry<-geojson_sf(points2process)
#   media<-raster::extract(AtmMg.ras,validgeometry)
#   return(media)
# }
# 
# AtmSO4<-function(points2process){
#   validgeometry<-geojson_sf(points2process)
#   media<-raster::extract(AtmSO4.ras,validgeometry)
#   return(media)
# }
# 
# EVI_MaxAve<-function(polygon2process){
#   validgeometry<-geojson_sf(polygon2process)
#   media<-exact_extract(EVI_MaxAve.ras,validgeometry,'mean')
#   return(media)
# }
# 
# LST32AVE<-function(polygon2process){
#   validgeometry<-geojson_sf(polygon2process)
#   media<-exact_extract(LST32AVE.ras,validgeometry,'mean')
#   return(media)
# }
# 
# MAXWD_WS<-function(polygon2process){
#   validgeometry<-geojson_sf(polygon2process)
#   media<-exact_extract(MAXWD_WS.ras,validgeometry,'mean')
#   return(media)
# }
# 
# MEANP_WS<-function(polygon2process){
#   validgeometry<-geojson_sf(polygon2process)
#   media<-exact_extract(MEANP_WS.ras,validgeometry,'mean')
#   return(media)
# }
# 
# MgO_Mean<-function(polygon2process){
#   validgeometry<-geojson_sf(polygon2process)
#   media<-exact_extract(MgO_Mean.ras,validgeometry,'mean')
#   return(media)
# }
# 
# MINP_WS<-function(polygon2process){
#   validgeometry<-geojson_sf(polygon2process)
#   media<-exact_extract(MINP_WS.ras,validgeometry,'mean')
#   return(media)
# }
# 
# MINWD_WS<-function(polygon2process){
#   validgeometry<-geojson_sf(polygon2process)
#   media<-exact_extract(MINWD_WS.ras,validgeometry,'mean')
#   return(media)
# }
# 
# UCS_Mean<-function(polygon2process){
#   validgeometry<-geojson_sf(polygon2process)
#   media<-exact_extract(UCS_Mean.ras,validgeometry,'mean')
#   return(media)
# }
