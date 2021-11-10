####################

#   Elevation      #

####################


#' Mean elevation across the watershed
#' The function requires that a Google Earth Engine GEE object be created
#' USGS_NED National Elevation Dataset. It uses rgee rgee::ee_extract function to conduct
#' zonal statistics. The resolution (pixel size to use) can be changed if desired by
#' modifying scale=. Now it is using a 90x90 m pixel size
#' @param polygon2process 
#'
#' @return a single value the mean elevation value for the watershed
#' @export
#'
#' @examples
pred_fns$ELVmean_WS<-function(polygon2process,USGS_NED,...){
  media<-rgee::ee_extract(USGS_NED, polygon2process, fun = ee$Reducer$mean(), scale=90)
  return(media[1,1])
}

#' Watershed mean elevation divided by 100
#' 
#' @param polygon2process 
#'
#' @return
#' @export
#'
#' @examples
pred_fns$ELVmean_WS_100<-function(polygon2process,USGS_NED,...){
  media<-ELVmean_WS(polygon2process,USGS_NED)/100
  return(media[1,1])
}


pred_fns$ELVmax_WS<-function(polygon2process,USGS_NED,...){
   media<-rgee::ee_extract(USGS_NED, polygon2process, fun = ee$Reducer$max(), scale=90)
  return(media[1,1])
}

#' Average of min elevation in the watershed
#' The function requires that a Google Earth Engine GEE object be created
#' USGS_NED National Elevation Dataset. It uses rgee rgee::ee_extract function to conduct
#' zonal statistics. The resolution (pixel size to use) can be changed if desired by
#' modifying scale=. Now it is using a 90x90 m pixel size
#' @param polygon2process 
#'
#' @return a single value the minimum elevation value for the watershed
#' @export
#'
#' @examples
pred_fns$ELVmin_WS<-function(polygon2process,USGS_NED,...){
   media<-rgee::ee_extract(USGS_NED, polygon2process, fun = ee$Reducer$min(), scale=90)
  return(media[1,1])
}


#' Range between max and min elevations
#' The function requires that a Google Earth Engine GEE object be created
#' USGS_NED National Elevation Dataset. It uses rgee rgee::ee_extract function to conduct
#' zonal statistics. The resolution (pixel size to use) can be changed if desired by
#' modifying scale=. Now it is using a 90x90 m pixel size.
#' The function first obtains the max elevation in the watershed, and then the minimum and finally
#' it obtains the difference between the two values
#' @param polygon2process 
#'
#' @return a single value, the elevation range in the watershed
#' @export
#'
#' @examples
pred_fns$ELEV_RANGE<-function(polygon2process,USGS_NED,...){
   max<-rgee::ee_extract(USGS_NED, polygon2process, fun = ee$Reducer$max(), scale=90)
  min<-rgee::ee_extract(USGS_NED, polygon2process, fun = ee$Reducer$min(), scale=90)
  media<-max-min
  return(media[1,1])
}

#' Elevation of the point
#' The function requires that a Google Earth Engine GEE object be created
#' USGS_NED National Elevation Dataset. It uses rgee rgee::ee_extract function to conduct
#' zonal statistics. The resolution (pixel size to use) can be changed if desired by
#' modifying scale=. Now it is using a 90x90 m pixel size.
#' The rgee function rgee::ee_extract is used here without fun = ee$Reducer$min,mean,max()
#' argument since it only needs the information at the point
#' @param points2process 
#'
#' @return a single value which is the elevation at the point
#' @export
#'
#' @examples
pred_fns$ELEV_SITE<-function(points2process,USGS_NED,...){
  media<-rgee::ee_extract(USGS_NED, points2process, scale=90)/10 
  return(media[1,1])
}

#' Square root of elevation at the point
#' The function requires that a Google Earth Engine GEE object be created
#' USGS_NED National Elevation Dataset. It uses rgee rgee::ee_extract function to conduct
#' zonal statistics. The resolution (pixel size to use) can be changed if desired by
#' modifying scale=. Now it is using a 90x90 m pixel size.
#' the function first extracts the elevation at the point. This elevation is then divided by
#' 10 and then the square root is extracted
#' @param points2process 
#'
#' @return a single value which is the square root of the elevation at the point
#' @export
#'
#' @examples
pred_fns$ELEV_SITE_SQRT<-function(points2process,USGS_NED,...){
   elevation<-rgee::ee_extract(USGS_NED, points2process, scale=90)
  media<-sqrt((elevation/10))
  return(media[1,1])
}


#' Elevation coefficient of variation at the point
#' The function requires that a Google Earth Engine GEE object be created
#' USGS_NED National Elevation Dataset. It uses rgee rgee::ee_extract function to conduct
#' zonal statistics. The resolution (pixel size to use) can be changed if desired by
#' modifying scale=. Now it is using a 90x90 m pixel size.
#' The function first makes sure that the point has a CRS information in meters
#' because the st_buffer function does not work well with decimal degrees
#' it then creates a area of influence AOI around the point of 150 meters (buffer). 
#' Extracts the mean and the standard deviation of the elevation within this AOI, and then
#' it calculates the coefficient of variation
#' @param points2process 
#'
#' @return a single value, the coefficient of variation of elevation in an 150 m buffer around the point
#' @export
#'
#' @examples
pred_fns$ELEV_SITE_CV<-function(points2process,USGS_NED,...){
  AOI<-sf::st_buffer(st_transform(points2process, 6703),150)
  elev.mean<-rgee::ee_extract(USGS_NED, AOI, fun = ee$Reducer$mean(), scale=90)
  elev.stdev<-rgee::ee_extract(USGS_NED, AOI, fun = ee$Reducer$stdDev(), scale=90)
  media<-elev.stdev/elev.mean
  return(media[1,1])
}



#################################################
### These are VERY LARGE vector datasets --- I am inclined to query what is needed directly from disk 
### as opposed to load the entire vectors in memory
### Regular version of a function that works with an object loaded to memory
# Spatial join to get the square root of topo at the point (the line shapefile is already square rooted)
# pred_fns$SQRT_TOPO<-function(points2process,predictor_geometry, ...){
#   crs2use<-crs(predictor_geometry)
#   AOItrans<-st_transform(points2process, crs2use)
#   AOI_Buffer<-st_join(AOItrans, predictor_geometry, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
#   media<-AOI_Buffer$TOPOCV
#   return(media)
# }

#### Super fast version - loads into memory ONLY WHAT is strictly necessary
pred_fns$SQRT_TOPO<-function(points2process,predictor_geometry,geometry_input_path, ...){
  AOItrans<-sf::st_transform(points2process, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>% 
    sf::st_geometry() %>% # convert to sfc
    sf::st_buffer(150) %>% # buffer 150 meters
    sf::st_as_text() # convert to well known text
  SQRT_TOPO.vec<-sf::st_read(geometry_input_path, wkt_filter = AOItrans_wkt)
AOI_Buffer<-sf::st_join(AOItrans, SQRT_TOPO.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
media<-AOI_Buffer$TOPOCV
return(media[1,1])
}
#################################################


