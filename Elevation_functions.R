####################

#   Elevation      #

####################


#' Mean elevation across the watershed
#' The function requires that a Google Earth Engine GEE object be created
#' USGS_NED National Elevation Dataset. It uses rgee ee_extract function to conduct
#' zonal statistics. The resolution (pixel size to use) can be changed if desired by
#' modifying scale=. Now it is using a 90x90 m pixel size
#' @param polygon2process 
#'
#' @return a single value the mean elevation value for the watershed
#' @export
#'
#' @examples
pred_fns$ELVmean_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-ee_extract(USGS_NED, validgeometry, fun = ee$Reducer$mean(), scale=90)
  return(media)
}

#' Watershed mean elevation divided by 100
#' 
#' @param polygon2process 
#'
#' @return
#' @export
#'
#' @examples
pred_fns$ELVmean_WS_100<-function(polygon2process){
  media<-ELVmean_WS(polygon2process)/100
  return(media)
}


ELVmax_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-ee_extract(USGS_NED, validgeometry, fun = ee$Reducer$max(), scale=90)
  return(media)
}

#' Average of min elevation in the watershed
#' The function requires that a Google Earth Engine GEE object be created
#' USGS_NED National Elevation Dataset. It uses rgee ee_extract function to conduct
#' zonal statistics. The resolution (pixel size to use) can be changed if desired by
#' modifying scale=. Now it is using a 90x90 m pixel size
#' @param polygon2process 
#'
#' @return a single value the minimum elevation value for the watershed
#' @export
#'
#' @examples
pred_fns$ELVmin_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-ee_extract(USGS_NED, validgeometry, fun = ee$Reducer$min(), scale=90)
  return(media)
}


#' Range between max and min elevations
#' The function requires that a Google Earth Engine GEE object be created
#' USGS_NED National Elevation Dataset. It uses rgee ee_extract function to conduct
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
pred_fns$ELEV_RANGE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  max<-ee_extract(USGS_NED, validgeometry, fun = ee$Reducer$max(), scale=90)
  min<-ee_extract(USGS_NED, validgeometry, fun = ee$Reducer$min(), scale=90)
  media<-max-min
  return(media)
}

#' Elevation of the point
#' The function requires that a Google Earth Engine GEE object be created
#' USGS_NED National Elevation Dataset. It uses rgee ee_extract function to conduct
#' zonal statistics. The resolution (pixel size to use) can be changed if desired by
#' modifying scale=. Now it is using a 90x90 m pixel size.
#' The rgee function ee_extract is used here without fun = ee$Reducer$min,mean,max()
#' argument since it only needs the information at the point
#' @param points2process 
#'
#' @return a single value which is the elevation at the point
#' @export
#'
#' @examples
pred_fns$ELEV_SITE<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-ee_extract(USGS_NED, validgeometry, scale=90)/10 
  return(media)
}

#' Square root of elevation at the point
#' The function requires that a Google Earth Engine GEE object be created
#' USGS_NED National Elevation Dataset. It uses rgee ee_extract function to conduct
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
pred_fns$ELEV_SITE_SQRT<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  elevation<-ee_extract(USGS_NED, validgeometry, scale=90)
  media<-sqrt((elevation/10))
  return(media)
}


#' Elevation coefficient of variation at the point
#' The function requires that a Google Earth Engine GEE object be created
#' USGS_NED National Elevation Dataset. It uses rgee ee_extract function to conduct
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
pred_fns$ELEV_SITE_CV<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  AOI<-st_buffer(st_transform(validgeometry, 6703),150)
  elev.mean<-ee_extract(USGS_NED, AOI, fun = ee$Reducer$mean(), scale=90)
  elev.stdev<-ee_extract(USGS_NED, AOI, fun = ee$Reducer$stdDev(), scale=90)
  media<-elev.stdev/elev.mean
  return(media)
}



#################################################
### These are VERY LARGE vector datasets --- I am inclined to query what is needed directly from disk 
### as opposed to load the entire vectors in memory
### Regular version of a function that works with an object loaded to memory
# Spatial join to get the square root of topo at the point (the line shapefile is already square rooted)
pred_fns$SQRT_TOPO<-function(points2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(points2process)
  crs2use<-crs(predictor_geometry)
  AOItrans<-st_transform(validgeometry, crs2use)
  AOI_Buffer<-st_join(AOItrans, predictor_geometry, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
  media<-AOI_Buffer$TOPOCV
  return(media)
}

#### Super fast version - loads into memory ONLY WHAT is strictly necessary
pred_fns$SQRT_TOPO<-function(points2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(points2process)
  AOItrans<-st_transform(validgeometry, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>% 
    st_geometry() %>% # convert to sfc
    st_buffer(150) %>% # buffer 150 meters
    st_as_text() # convert to well known text
  SQRT_TOPO.vec<-st_read(), wkt_filter = AOItrans_wkt)
AOI_Buffer<-st_join(AOItrans, SQRT_TOPO.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
media<-AOI_Buffer$TOPOCV
return(media)
}
#################################################


