# simple watershed mean functions

#' Soil Water Capacity across the watershed
#' This functions takes the GIS_Stats/Soils/Data/awc dataset
#' it extracts zonal statistics mean for the watershed  -->exact_extract(AWC_soil.ras,validgeometry,'mean') <--
#' @param polygon2process this is a geojson string for the watershed
#' The geojson is converted to an object of type sf -->validgeometry<-geojson_sf(polygon2process)<--
#' @return this functions returns one value which is the mean of raster cells in the watershed
#' @export
#'
#' @examples
AWC_soil<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(AWC_soil.ras,validgeometry,'mean')
  return(media)
}

BDH_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exactextractr::exact_extract(BDH_AVE.ras,validgeometry,'mean')
  return(media)
}

BFI_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exactextractr::exact_extract(BFI_WS.ras,validgeometry,'mean')
  return(media)
}

CaO_Mean<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(CaO_Mean.ras,validgeometry,'mean')
  return(media)
}

Db3rdbar_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(Db3rdbar.ras,validgeometry,'mean')
  return(media)
}

#' Mean of Evi_ave in the watershed
#' dataset used GIS_Stats/Vegetation/Data/evi_ave
#' The exact_extract function is used to compute zonal statistics "mean" of the variable of interest
#' within the spatial domain of the watershed
#' @param polygon2process 
#'
#' @return a single value which is the mean value of EVI in the watershed
#' @export
#'
#' @examples
EVI_AveAve<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(EVI_AveAve.ras,validgeometry,'mean')
  return(media)
}

EVI_MAX_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(EVI_MAX_AVE.ras,validgeometry,'mean')
  return(media)
}

EVI_MaxAve<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(EVI_MaxAve.ras,validgeometry,'mean')
  return(media)
}

FST32F_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(FST32F_AVE.ras,validgeometry,'mean')
  return(media)
}

HYDR_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(HYDR_WS.ras,validgeometry,'mean')
  return(media)
}

KFACT<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(Kfact.ras,validgeometry,'mean')
  return(media)
}

KFCT_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(KFCT_AVE.ras,validgeometry,'mean')
  return(media)
}

KFCT_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(KFCT_AVE.ras,validgeometry,'mean')
  return(media)
}

LPREM_mean<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(LPREM_mean.ras,validgeometry,'mean')
  return(media)
}

LST32AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(LST32AVE.ras,validgeometry,'mean')
  return(media)
}

MAXP_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(MAXP_AVE.ras,validgeometry,'mean')
  return(media)
}

MAXWD_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(MAXWD_AVE.ras,validgeometry,'mean')
  return(media)
}

MAXWD_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(MAXWD_WS.ras,validgeometry,'mean')
  return(media)
}

MEANP_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(MEANP_AVE.ras,validgeometry,'mean')
  return(media)
}

MEANP_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(MEANP_WS.ras,validgeometry,'mean')
  return(media)
}

MgO_Mean<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(MgO_Mean.ras,validgeometry,'mean')
  return(media)
}

MINP_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(MINP_WS.ras,validgeometry,'mean')
  return(media)
}

MINWD_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(MINWD_WS.ras,validgeometry,'mean')
  return(media)
}

N_MEAN<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(N_MEAN.ras,validgeometry,'mean')
  return(media)
}

P_MEAN<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(P_MEAN.ras,validgeometry,'mean')
  return(media)
}

Pmax_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(Pmax_WS.ras,validgeometry,'mean')
  return(media)
}


Pmin_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(PMIN_WS.ras,validgeometry,'mean')
  return(media)
}

PRMH_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(PRMH_AVE.ras,validgeometry,'mean')
  return(media)
}

RH_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(RH_AVE.ras,validgeometry,'mean')
  return(media)
}

RH_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(RH_AVE.ras,validgeometry,'mean')
  return(media)
}

S_Mean<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(S_Mean.ras,validgeometry,'mean')
  return(media)
}

SOC<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(SOC.ras,validgeometry,'mean')
  return(media)
}

SumAve_P<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(SumAve_P.ras,validgeometry,'mean')
  return(media)
}

TMAX_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(Tmax_WS.ras,validgeometry,'mean')
  return(media)
}

TMEAN_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(TMEAN_AVE.ras,validgeometry,'mean')
  return(media)
}


TMEAN_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(TMEAN_WS.ras,validgeometry,'mean')
  return(media)
}

TMIN_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(TMIN_AVE.ras,validgeometry,'mean')
  return(media)
}

TMIN_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(TMIN_WS.ras,validgeometry,'mean')
  return(media)
}

TP_Mean<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(TP_Mean.ras,validgeometry,'mean')
  return(media)
}

UCS_Mean<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(UCS_Mean.ras,validgeometry,'mean')
  return(media)
}

Vol_ave<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(Vol_ave.ras,validgeometry,'mean')
  return(media)
}

WDmax_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(WDmax_WS.ras,validgeometry,'mean')
  return(media)
}

XWD_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(XWD_WS.ras,validgeometry,'mean')
  return(media)
}
