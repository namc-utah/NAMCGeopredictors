####################
elev_trashbin<-tempdir()
#   Elevation      #

#' Mean elevation across the watershed
#' z value of 11 equates to a raster cell size of 30 m
#' @param polygon2process
#' @param ...
#'
#' @return a single value the mean elevation value for the watershed
#' @export
#'
#' @examples
ELVmean_WS<-function(polygon2process,...){
  poly_rast<-elevatr::get_elev_raster(polygon2process,z=11)
  media<-terra::extract(x=poly_rast,y=polygon2process,fun=mean)
  return(media[1,1])
  unlink(paste0(elev_trashbin,'/*'))
}

#' #' Watershed mean elevation divided by 100
#' #' old CO model but not even used in that
#' #' @param polygon2process
#' #' @param USGS_NED
#' #' @param ...
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' ELVmean_WS_100<-function(polygon2process[["_ogr_geometry_"]],USGS_NED,...){
#'   media<-ELVmean_WS(polygon2process,USGS_NED)/100
#'   return(media[1,2])
#' }


#' Watershed max elevation
#' z value of 11 equates to a raster cell size of 30 m#'
#' @param polygon2process
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ELVmax_WS<-function(polygon2process,...){
  poly_rast<-elevatr::get_elev_raster(location=polygon2process,z=11)
  media<-terra::extract(x=poly_rast,y=polygon2process,fun=max)
  return(media[1,1])
  unlink(paste0(elev_trashbin,'/*'))
}

#' Average of min elevation in the watershed
#' z value of 11 equates to a raster cell size of 30 m
#' @param polygon2process
#' @param ...
#'
#' @return a single value the minimum elevation value for the watershed
#' @export
#'
#' @examples
ELVmin_WS<-function(polygon2process,...){
  poly_rast<-elevatr::get_elev_raster(polygon2process,z=11)
  media<-terra::extract(x=poly_rast,y=polygon2process,fun=min)
  return(media[1,1])
  unlink(paste0(elev_trashbin,'/*'))
}


#' Range between max and min elevations
#' The function first obtains the max elevation in the watershed, and then the minimum and finally
#' it obtains the difference between the two values
#' z value of 11 equates to a raster cell size of 30 m
#'
#' @param polygon2process
#' @param ...
#'
#' @return a single value, the elevation range in the watershed
#' @export
#'
#' @examples
ELEV_RANGE<-function(polygon2process,...){
  poly_rast<-elevatr::get_elev_raster(polygon2process,z=11)
  max<-terra::extract(x=poly_rast,y=polygon2process,fun=max)
  min<-terra::extract(x=poly_rast,y=polygon2process,fun=min)
  media<-max-min
  #return(c(polygon2process$siteId,media))
  return(media[1,1])
  unlink(paste0(elev_trashbin,'/*'))
}
#' Range between max and min elevations
#' The function first obtains the max elevation in the watershed, and then the minimum and finally
#' it obtains the difference between the two values
#' z value of 11 equates to a raster cell size of 30 m
#'
#' @param polygon2process
#' @param ...
#'
#' @return a single value, the elevation range in the watershed
#' @export
#'
#' @examples
VALLEY_ELEV_RANGE<-function(polygon2process,...){
  poly_rast<-elevatr::get_elev_raster(polygon2process,z=11)
  max<-terra::extract(x=poly_rast,y=polygon2process,fun=max)
  min<-terra::extract(x=poly_rast,y=polygon2process,fun=min)
  media<-max-min
  #return(c(polygon2process$siteId,media))
  return(media[1,1])
  unlink(paste0(elev_trashbin,'/*'))
}

#' Range between max and min elevations
#' The function first obtains the max elevation in the watershed, and then the minimum and finally
#' it obtains the difference between the two values
#' z value of 11 equates to a raster cell size of 30 m
#'
#' @param point2process
#' @param
#' @param ...
#'
#' @return a single value, the elevation range in the watershed
#' @export
#'
#' @examples
Buffer1250ElevRange<-function(point2process,...){
  point2process=sf::st_transform(point2process,crs=5070)
  polygon2process=sf::st_buffer(point2process,1250)
  poly_rast<-elevatr::get_elev_raster(polygon2process,z=11)
  max<-terra::extract(x=poly_rast,y=polygon2process,fun=max)
  min<-terra::extract(x=poly_rast,y=polygon2process,fun=min)
  media<-max-min
  #return(c(polygon2process$siteId,media))
  return(media[1,1])
  unlink(paste0(elev_trashbin,'/*'))
}
#' Elevation of the point
#' z value of 11 equates to a raster cell size of 30 m
#'
#' @param point2process
#' @param ...
#'
#' @return a single value which is the elevation at the point
#' @export
#'
#' @examples
ELEV_SITE<-function(x,...){
  media<-get_elev_point(point2process)$elevation
  # neither CO MMI nor CSCI use /10 transformation
  #return(c(def_sites_sample$siteId,media))

  return(media)
  unlink(paste0(elev_trashbin,'/*'))
}

#' Square root of elevation at the point
#' z value of 11 equates to a raster cell size of 30 m
#' the function first extracts the elevation at the point. This elevation is then divided by
#' 10 and then the square root is extracted
#'
#' @param point2process
#' @param ...
#'
#' @return a single value which is the square root of the elevation at the point
#' @export
#'
#' @examples
ELEV_SITE_SQRT<-function(x,...){
  point_rast<-get_elev_point(point2process,src='aws')$elevation
  media<-sqrt(point_rast)
  #return(c(def_sites_sample$siteId,media))
  return(media)
}


#' Elevation coefficient of variation at the point
#' The function first makes sure that the point has a CRS information in meters
#' because the st_buffer function does not work well with decimal degrees
#' it then creates a area of influence AOI around the point of 150 meters (buffer).
#' Extracts the mean and the standard deviation of the elevation within this AOI, and then
#' it calculates the coefficient of variation
#' z value of 11 equates to a raster cell size of 30 m
#'
#' @param point2process
#' @param ...
#'
#' @return a single value, the coefficient of variation of elevation in an 150 m buffer around the point
#' @export
#'
#' @examples
ELEV_SITE_CV<-function(x,...){
  AOI<-sf::st_buffer(st_transform(point2process, 6703),150)
  elev<-get_elev_raster(AOI,z=11)
  elev.mean<-terra::extract(elev,AOI,fun=mean)
  elev.stdev<-terra::extract(elev,AOI,fun=sd)
  media<-elev.stdev/elev.mean
  #return(c(def_sites_sample$siteId,media))
  return(media[1,1])
}



#' #################################################
#' ### commented out because (OLD CO OE or MMI), predictor no longer used
#' These are VERY LARGE vector datasets --- I am inclined to query what is needed directly from disk
#' ### as opposed to load the entire vectors in memory
#' ### Regular version of a function that works with an object loaded to memory
#' # Spatial join to get the square root of topo at the point (the line shapefile is already square rooted)
#' #### Super fast version - loads into memory ONLY WHAT is strictly necessary
#' #' Square root of topo from the line shapefile
#' #'
#' #' @param point2process
#' #' @param predictor_geometry
#' #' @param geometry_input_path
#' #' @param ...
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' SQRT_TOPO<-function(point2process,predictor_geometry,geometry_input_path, ...){
#'   AOItrans<-sf::st_transform(point2process, 5070) # must use the same EPSG as in the shapefile
#'   AOItrans_wkt <- AOItrans %>%
#'     sf::st_geometry() %>% # convert to sfc
#'     sf::st_buffer(150) %>% # buffer 150 meters
#'     sf::st_as_text() # convert to well known text
#'   SQRT_TOPO.vec<-sf::st_read(geometry_input_path, wkt_filter = AOItrans_wkt)
#' AOI_Buffer<-sf::st_join(AOItrans, SQRT_TOPO.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
#' media<-AOI_Buffer$TOPOCV
#' return(media)
#' }
#' #################################################


