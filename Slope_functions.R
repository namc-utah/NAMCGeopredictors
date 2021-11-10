####################

#   Slope          #

####################

#' NHD Plus slope value taken from the nearest stream segment
#' st_transform changes the CRS of the point to meters using 5070 Albers Contiguous US, now we have the point in meter (projection)
#' Using the new object for the point in meters, a well-known text (WKT) string will be created to query the required vector predictor
#' this WKT can be used as an argument in st_read to query a big vector shapefile or geopackages and just bring into memory the AOI
#' i.e. like a bounding without overwhelming R
#' Buffer the point by 200m, interest with NHD streams, extract SLOPE value
#' Jennifer's notes- maxdist=500 meters needs reexamined. The original python code used 200 meters. really we should be using COMID and joining to that!!
#' @param points2process 
#'
#' @return
#' @export
#'
#' @examples
pred_fns$NHDSLOPE<-function(points2process,geometry_input_path,...){
    AOItrans<-sf::st_transform(points2process, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>% 
    sf::st_geometry() %>% # convert to sfc
    sf::st_buffer(200) %>% # buffer 200 meters
    sf::st_as_text() # convert to well known text
  NHDSLOPE.vec<-sf::st_read(geometry_input_path, wkt_filter = AOItrans_wkt)
  AOI_Buffer<-sf::st_join(AOItrans, NHDSLOPE.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
  media<-AOI_Buffer$SLOPE
  return(media[1,1])
}


# still in development


# pred_fns$Slope_WS<-function(polygon2process,...){
#     validgeobuf<-st_buffer(st_transform(polygon2process, 5072), 300) # transforming to CRS of NV D8 point Flow Direction
#   write_sf(st_transform(polygon2process, 5072), here("wat.shp"))
#   write_sf(validgeobuf,here("buffer_wat.shp"))
#   inputD8<-here("NVMod/NVFLD8.tif")
#   outputD8<-here("NVMod/NVFLD8_crop3.tif")
#   reproD8<-gdalUtils::gdalwarp(srcfile =outputD8, dstfile = destD8, t_srs = '+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs')
#   outputFL<-here("NVMod/NVFL_crop6.tif")
#   inputwat<-here("wat.shp")
#   inputpoly2crop<-here("buffer_wat.shp")
#   destD8<-here("NVMod/NVFLD8_cropCRS.tif")
#   destD8flow<-here("NVMod/NVFLD8Flow_crop.tif")
#   destD8stream<-here("NVMod/NVFLD8Stream_crop.tif")
#   #whitebox::wbt_clip_raster_to_polygon(input = inputD8, polygons = inputpoly2crop, output = outputD8) # clip the DF8 to the watershed bounds
#   whitebox::wbt_d8_flow_accumulation(outputD8, destD8flow, out_type="cells", pntr = TRUE, esri_pntr = TRUE) #
#   whitebox::wbt_extract_streams(destD8flow, destD8stream, threshold=5000.0)
#   whitebox::wbt_downslope_flowpath_length(d8_pntr = outputD8,output = outputFL, watersheds = inputwat )
#   media<-exact_extract(SOC.ras,polygon2process,'mean')
#   return(media)
# }
# 
# mask<-st_read(inputpoly2crop)
# ptm <- proc.time()
# writeRaster(raster::crop(raster::mask(rastrillo, mask),extent(mask)),datatype='INT1U',overwrite=TRUE,filename = here("NVMod/NVFLD8_crop3.tif"))
# proc.time() - ptm
# 
# pred_fns$slpavg <- function(polygon2process,USGS_NED,...) {
# slopegee<-ee$Terrain$slope(USGS_NED) # slope
# slopegee.perc<- slopegee$divide(180)$multiply(3.14159)$tan()$multiply(1)$rename("percent")#Slope percent
#   zones.Albers.3 <- zones.Albers.2[, c(1:4)]
#   #Wsheds.att.OLD$PPT_ACCUM<-NA
#   zones.Albers.3$SlopeGEE <- NA
#   
#   for (i in 1:nrow(zones.Albers.3)) {
#     tryCatch({
#       #if an error is found then it is printed, but the loop does not break and continues with the next iteration
#       objecto <- zones.Albers.3[i, ] # Take the first feature
#       slope.water <-
#         ee_extract(slopegee.perc,
#                    objecto,
#                    fun = ee$Reducer$mean(),
#                    scale = 30) %>% as_tibble()
#       print(slope.water)
#       slope.water <- pull(slope.water)
#       zones.Albers.3[[6]][i] <- slope.water
#     }, error = function(e) {
#       cat("ERROR :", conditionMessage(e), "\n")
#     })
#   }
# }