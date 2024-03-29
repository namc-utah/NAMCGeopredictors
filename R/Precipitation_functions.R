####################

#   Precipitation   #

####################
#' Log 10 precipitation at the point, used for PIBO and old CO model
#'
#' @param point2process
#' @param ...
#' @param predictor_geometry
#'
#' @description  can be mean log precipitation at the point too if input raster is a mean
#'
#' @return
#' @export
#'
#' @examples
LOG_PRECIP_SITE<-function(point2process,predictor_geometry, ...){
   media<-log10(raster::extract(predictor_geometry,point2process))
  return(media)
}

#' PRISM 2 month prior moving average precipitation for the watershed
#'
#' @param polygon2process
#' @param CurrentYear
#' @param JulianDate
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
PPT_2MoAvg<-function(polygon2process, CurrentYear, JulianDate,...){
  curYear.2month<-CurrentYear
  # Obtain a GEE image that has the monthly precipitation for those months where sample can occur -- in this case from February to November
  prism.1<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-01-01"), paste0(curYear.2month,"-01-31")))$select('ppt')
  prism.2<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-02-01"), paste0(curYear.2month,"-02-28")))$select('ppt')
  prism.3<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-03-01"), paste0(curYear.2month,"-03-31")))$select('ppt')
  prism.4<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-04-01"), paste0(curYear.2month,"-04-30")))$select('ppt')
  prism.5<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-05-01"), paste0(curYear.2month,"-05-31")))$select('ppt')
  prism.6<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-06-01"), paste0(curYear.2month,"-06-30")))$select('ppt')
  prism.7<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-07-01"), paste0(curYear.2month,"-07-31")))$select('ppt')
  prism.8<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-08-01"), paste0(curYear.2month,"-08-31")))$select('ppt')
  prism.9<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-09-01"), paste0(curYear.2month,"-09-30")))$select('ppt')
  prism.10<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-10-01"), paste0(curYear.2month,"-10-31")))$select('ppt')
  prism.11<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-11-01"), paste0(curYear.2month,"-11-30")))$select('ppt')
  juliandate<-JulianDate
  month.cur<-as.Date(juliandate-1, origin = paste0(curYear.2month,"-01-01")) # Transform to a YYYY-MM-DD format
  monthy.cur<-as.numeric(substr(month.cur, 6, 7)) # Estimate the CURRENT month number based on the YYYY-MM-DD format
  monthy.pre<-monthy.cur-1 # Estimate the PREVIOUS month number based on the YYYY-MM-DD format
  xx<-eval(parse(text = paste0("prism.",monthy.cur))) # Evaluations that are required so that a variable is recognized as such
  xxx<-eval(parse(text = paste0("prism.",monthy.pre)))# Evaluations that are required so that a variable is recognized as such
  pcp.extraction.cur<-rgee::ee_extract(xx, polygon2process[["_ogr_geometry_"]], fun = ee$Reducer$mean(), scale=50)%>% as_tibble() # Compute pcp for CURRENT month
  pcp.extraction.pre<-rgee::ee_extract(xxx, polygon2process[["_ogr_geometry_"]], fun = ee$Reducer$mean(), scale=50)%>% as_tibble()# Compute pcp for PREVIOUS month
  polygon2process$PPT_2MoAvg<-unlist((pcp.extraction.pre[1,2]+pcp.extraction.cur[1,2])/2)*100 # Obtain average and multiply by 100 so it is similar to Olson
  media<-polygon2process$PPT_2MoAvg
  return(media)
}

#' PRISM prior year (May-April) cumulative precipitation at the point
#'
#' @param point2process
#' @param CurrentYear
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
PPT_ACCUM<-function(point2process, CurrentYear,...){
   prevYear1<-CurrentYear-1
  prevYear0<-prevYear1-1
  WaterYearStart<-paste0(prevYear0,"-05-01")
  WaterYearEnd<-paste0(prevYear1,"-04-30")
  prism.accum0<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(WaterYearStart, WaterYearEnd))$select('ppt')
  prism.accum.precip<-prism.accum0$sum()
  media<-rgee::ee_extract(prism.accum.precip, point2process, fun = ee$Reducer$mean(), scale=50)
  return(media)
}


#' Precipitation from vector layer (OR- WCCP model)
#'
#' @param point2process
#' @param predictor_geometry
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
precip<-function(point2process,predictor_geometry, ...){
  point2process<-sf::st_transform(point2process, st_crs(predictor_geometry))
  media<-sf::st_intersection(point2process, predictor_geometry) %>% dplyr::pull(precip_mm)
  return(media)
}

#' #' PRISM calendar year average precipitation at the watershed (CO OE/ old MMI)
#' #' No longer an actively used predictor
#' #' @param point2process
#' #' @param CurrentYear
#' #' @param ...
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' PRCPSHORTWS<-function(point2process, CurrentYear,...){
#'   WaterYearStart<-paste0(CurrentYear,"-01-01")
#'   WaterYearEnd<-paste0(CurrentYear,"-12-31")
#'   prism.accum0<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(WaterYearStart, WaterYearEnd))$select('ppt')
#'   prism.accum.precip<-prism.accum0$sum()
#'   media<-ee_extract(prism.accum.precip, point2process, fun = ee$Reducer$mean(), scale=4000)
#'   return(media)
#' }
