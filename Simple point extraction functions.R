# simple point extraction functions

#' Atmospheric calcium at the point
#' This functions takes the GIS_Stats/Atmos/Data/atm_ca dataset
#' it extracts the value at the point  -->raster::extract(AtmCa.ras,validgeometry) <--
#' @param points2process this is a geojson string for the pourpoint
#' The geojson is converted to an object of type sf -->validgeometry<-geojson_sf(points2process)<--
#' @return this functions returns one value which is the Raster value at the point
#' @export
#'
#' @examples
AtmCa<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(AtmCa.ras,validgeometry)
  return(media)
}

#' Atmospheric Mg at the point
#' This functions takes the GIS_Stats/Atmos/Data/atm_mg dataset
#' it extracts the value at the point  -->raster::extract(AtmMg.ras,validgeometry) <--
#' @param points2process this is a geojson string for the pourpoint
#' The geojson is converted to an object of type sf -->validgeometry<-geojson_sf(points2process)<--
#' @return this functions returns one value which is the Raster value at the point
#' @export
#'
#' @examples
AtmMg<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(AtmMg.ras,validgeometry)
  return(media)
}

#' Atmospheric Na at the point
#' This functions takes the GIS_Stats/Atmos/Data/atm_na dataset
#' it extracts the value at the point  -->raster::extract(AtmNa.ras,validgeometry) <--
#' @param points2process this is a geojson string for the pourpoint
#' The geojson is converted to an object of type sf -->validgeometry<-geojson_sf(points2process)<--
#' @return this functions returns one value which is the Raster value at the point
#' @export
#'
#' @examples
AtmNa<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(AtmNa.ras,validgeometry)
  return(media)
}

#' Atmospheric NO3 at the point
#' This functions takes the GIS_Stats/Atmos/Data/atm_no3 dataset
#' it extracts the value at the point  -->raster::extract(AtmNO3.ras,validgeometry) <--
#' @param points2process this is a geojson string for the pourpoint
#' The geojson is converted to an object of type sf -->validgeometry<-geojson_sf(points2process)<--
#' @return this functions returns one value which is the Raster value at the point
#' @export
#'
#' @examples
AtmNO3<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(AtmNO3.ras,validgeometry)
  return(media)
}

#' Atmospheric SO4 at the point
#' This functions takes the GIS_Stats/Atmos/Data/atm_so4 dataset
#' it extracts the value at the point  -->raster::extract(AtmSO4.ras,validgeometry) <--
#' @param points2process this is a geojson string for the pourpoint
#' The geojson is converted to an object of type sf -->validgeometry<-geojson_sf(points2process)<--
#' @return this functions returns one value which is the Raster value at the point
#' @export
#'
#' @examples
AtmSO4<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(AtmSO4.ras,validgeometry)
  return(media)
}


Pmax_PT<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(Pmax_WS.ras,validgeometry)
  return(media)
}

Pmin_PT<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(PMIN_WS.ras,validgeometry)
  return(media)
}

PT_Tmin<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(PT_Tmin.ras,validgeometry)
  return(media)
}


PPT_00_09<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(PPT_00_09.ras,validgeometry)
  return(media)
}

TEMP_00_09<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(TEMP_00_09.ras,validgeometry)
  return(media)
}

TMEAN_PT<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(TMEAN_WS.ras,validgeometry)
  return(media)
}

TMEANPT<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(TMEAN_AVE.ras,validgeometry)
  return(media)
}


