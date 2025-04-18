####################
slope_bin<-tempdir()
#   Slope          #

####################

#' NHD Plus slope value taken from the nearest stream segment
#' st_transform changes the CRS of the point to meters using 5070 Albers Contiguous US, now we have the point in meter (projection)
#' Using the new object for the point in meters, a well-known text (WKT) string will be created to query the required vector predictor
#' this WKT can be used as an argument in st_read to query a big vector shapefile or geopackages and just bring into memory the AOI
#' i.e. like a bounding without overwhelming R
#' Buffer the point by 200m, interest with NHD streams, extract SLOPE value
#' Jennifer's notes- maxdist=500 meters needs reexamined. The original python code used 200 meters. really we should be using COMID and joining to that!!
#'
#' @param point2process
#' @param geometry_input_path
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
NHDSLOPE<-function(point2process,geometry_input_path,...){
    AOItrans<-sf::st_transform(point2process, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>%
    sf::st_geometry() %>% # convert to sfc
    sf::st_buffer(200) %>% # buffer 200 meters
    sf::st_as_text() # convert to well known text
  NHDSLOPE.vec<-sf::st_read(geometry_input_path, wkt_filter = AOItrans_wkt)
  AOI_Buffer<-sf::st_join(AOItrans, NHDSLOPE.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
  media<-AOI_Buffer$SLOPE
  return(media)
  unlink(paste0(slope_bin,'/*'))
}

#' NHD Plus stream order and slope value taken from the nearest stream segment
#' st_transform changes the CRS of the point to meters using 5070 Albers Contiguous US, now we have the point in meter (projection)
#' Using the new object for the point in meters, a well-known text (WKT) string will be created to query the required vector predictor
#' this WKT can be used as an argument in st_read to query a big vector shapefile or geopackages and just bring into memory the AOI
#' i.e. like a bounding without overwhelming R
#' Buffer the point by 200m, interest with NHD streams, extract SLOPE value
#' Jennifer's notes- maxdist=500 meters needs reexamined. The original python code used 200 meters. really we should be using COMID and joining to that!!
#'
#' @param point2process
#' @param geometry_input_path
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
NHDStreamOrder<-function(point2process,geometry_input_path,...){
  AOItrans<-sf::st_transform(point2process, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>%
    sf::st_geometry() %>% # convert to sfc
    sf::st_buffer(200) %>% # buffer 200 meters
    sf::st_as_text() # convert to well known text
  NHDSLOPE.vec<-sf::st_read(geometry_input_path, wkt_filter = AOItrans_wkt)
  AOI_Buffer<-sf::st_join(AOItrans, NHDSLOPE.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
  media<-AOI_Buffer[,c("SLOPE","StreamOr_1")]
  return(media)
  unlink(paste0(slope_bin,'/*'))
}


#' #' Watershed slope using flow length- NV MMI model
#' #' @description total elevation change in the watershed/ arcgis flow length
#' #' @param polygon2process
#' #' @param geometry_input_path
#' #' @param USGS_NED
#' #' @param ...
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' Slope_WS<-function(polygon2process,geometry_input_path,USGS_NED,...){
#'   arc=import("arcpy")
#'   #import the spatial analyst arc py module/ library to get needed functions
#'   sa=reticulate::import("arcpy.sa")
#'   #geometry_input_path="Z:/GIS/Delineation/NewPythonMethod/Rasters/NVmod/NVFLD8.tif"
#'   # watershed slope = rise/ run
#'   ### rise= max watershed elevation - min watershed elevation ###
#'   # call elevation functions to get min and max watershed elevations
#'
#'
#'   ### run= flow length as determined by ArcGIS from DEM flow length raster ###
#'   polygon2processtrans<-sf::st_transform(polygon2process, 5072)# transforming to CRS of NV D8 point Flow Direction
#'   #write buffer of watershed out as a shapefile
#'   sf::write_sf(polygon2processtrans,"polygon2processtrans.shp")#write it out to your project working directory!
#'
#'   # clip the NV flow accumulation grid to the watershed boundary using the spatial analyst extract by mask tool and save to local app data. File path of the new file created will be stored in the extract2 object
#'   clippedFlowRaster=sa$ExtractByMask(geometry_input_path,'polygon2processtrans.shp')
#'   #calculate flow length using the spatial analyst FlowLength tool and save to local app data. File path of the new file created will be stored in the extract2 object
#'   ShedFlowL=sa$FlowLength(clippedFlowRaster,'UPSTREAM')# previous python code had downstream but not sure why!!
#'   FlowLength=raster::raster(paste0(ShedFlowL))
#'   max_flow_length=raster::maxValue(FlowLength) # code previously multiplied by 10 and then divided by 100. make sure this output number is in proper units (m)
#'   max_watershed_elevation=ELVmax_WS(polygon2process,USGS_NED)
#'   min_watershed_elevation=ELVmin_WS(polygon2process,USGS_NED)
#'   media=(max_watershed_elevation[1,2]-min_watershed_elevation[1,2])/max_flow_length
#'    return(media)
#' }
#'



#' #' Average Watershed slope
#' #' @description slope of each cell in a DEM and then averaged across the watershed
#' #' @param polygon2process
#' #' @param USGS_NED
#' #' @param ...
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#'  slpavg <- function(polygon2process,USGS_NED,...) {
#'   slopegee<-ee$Terrain$slope(USGS_NED) # slope
#'   slopegee.perc<- slopegee$divide(180)$multiply(3.14159)$tan()$multiply(1)$rename("percent")#Slope percent
#'   media<-rgee::ee_extract(slopegee.perc,polygon2process[["_ogr_geometry_"]],fun = ee$Reducer$mean(),scale = 30)
#'   return(media[1,2])
#'  }
#'
 #' Average Watershed slope
 #' @description slope of each cell in a DEM and then averaged across the watershed
 #' @param polygon2process
 #' @param ...
 #'
 #' @return
 #' @export
 #'
 #' @examples
 slpavg <- function(polygon2process,...) {
   poly_rast<-elevatr::get_elev_raster(location=polygon2process,z=11)
   slope_poly_rast<-terra::terrain(poly_rast,v="slope",neighbors=8,unit="degrees")
    percentslope_poly_rast<-tan(slope_poly_rast$slope/180*pi)*100
   media<-terra::extract(x=percentslope_poly_rast,y=polygon2process,fun=mean)
  return(media[1,1])
   unlink(paste0(slope_bin,'/*'))
 }

 #' Average Watershed slope
 #' @description slope of each cell in a DEM and then averaged across the watershed
 #' @param polygon2process
 #' @param ...
 #'
 #' @return
 #' @export
 #'
 #' @examples
Buffer250Slope <- function(point2process,...) {
  point2process=sf::st_transform(point2process,crs=5070)
  polygon2process=sf::st_buffer(point2process,250)
   poly_rast<-elevatr::get_elev_raster(location=polygon2process,z=11)
   slope_poly_rast<-terra::terrain(poly_rast,v="slope",neighbors=8,unit="degrees")
   percentslope_poly_rast<-tan(slope_poly_rast$slope/180*pi)*100
   media<-terra::extract(x=percentslope_poly_rast,y=polygon2process,fun=mean)
   return(media[1,1])
   unlink(paste0(slope_bin,'/*'))
 }
