ee_Initialize()
USGS_NED = ee$Image("USGS/NED")$select("elevation")


#' Watershed slope using flow length- NV MMI model
#' @description total elevation change in the watershed/ arcgis flow length
#' @param polygon2process
#' @param geometry_input_path
#' @param USGS_NED
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
Slope_WS<-function(polygon2process,USGS_NED,...){
  arc=import("arcpy")
  #import the spatial analyst arc py module/ library to get needed functions
  sa=reticulate::import("arcpy.sa")
  geometry_input_path="Z:/GIS/Delineation/NewPythonMethod/Rasters/NVmod/NVFLD8.tif"
  # watershed slope = rise/ run
  ### rise= max watershed elevation - min watershed elevation ###
  # call elevation functions to get min and max watershed elevations
  ### run= flow length as determined by ArcGIS from DEM flow length raster ###
  polygon2processtrans<-sf::st_transform(polygon2process, 5072)# transforming to CRS of NV D8 point Flow Direction
  #write buffer of watershed out as a shapefile
  sf::write_sf(polygon2processtrans,"polygon2processtrans.shp")#write it out to your project working directory!

  # clip the NV flow accumulation grid to the watershed boundary using the spatial analyst extract by mask tool and save to local app data. File path of the new file created will be stored in the extract2 object
  clippedFlowRaster=sa$ExtractByMask(geometry_input_path,'polygon2processtrans.shp')
  #calculate flow length using the spatial analyst FlowLength tool and save to local app data. File path of the new file created will be stored in the extract2 object
  ShedFlowL=sa$FlowLength(clippedFlowRaster,'UPSTREAM')# previous python code had downstream but not sure why!!
  FlowLength=raster::raster(paste0(ShedFlowL))
  max_flow_length=raster::maxValue(FlowLength) # code previously multiplied by 10 and then divided by 100. make sure this output number is in proper units (m)
   max_watershed_elevation=ELVmax_WS(polygon2process,USGS_NED)
  min_watershed_elevation=ELVmin_WS(polygon2process,USGS_NED)
  media=(max_watershed_elevation-min_watershed_elevation)/max_flow_length
  return(media)
}



def_samples = NAMCr::query(
  api_endpoint = "samples",
  include = c("sampleId", "siteId", "sampleDate"),
  boxId=2770,

)
# getting watershed
siteIds=unlist(unique(def_samples$siteId))
def_sites=list()
# for each site in def_predictors get site coordinates and comid from database
# store as a list of lists referenced by "x" plus the siteId
for (t in 1:length(siteIds)){
  if(t==1){
    def_sites= unlist(NAMCr::query(
      api_endpoint = "siteInfo",
      include = c("siteId", "siteName", "usState", "location","waterbodyCode"),
      siteId = siteIds[t]
    ))
  } else {def_sites1= unlist(NAMCr::query(
    api_endpoint = "siteInfo",
    include = c("siteId", "siteName", "usState", "location","waterbodyCode"),
    siteId = siteIds[t]
  ))
  def_sites=as.data.frame(rbind(def_sites,def_sites1))
  }

}


# get watersheds in from the mastersheds shapefile/geodatabase on box
def_watersheds=sf::st_make_valid(sf::st_read(watershed_file_path, query=sprintf('SELECT * FROM %s WHERE siteId in(%s)',watershed_layer_name, inLOOP(substr(siteIds, 1, 10)))))




slopes=list()
for (s in 1:nrow(def_watersheds)){
  slopes[[s]]=Slope_WS(polygon2process=def_watersheds[s,3],USGS_NED)
}
slopesdf=as.data.frame(cbind(slopes))

def_sites=cbind(def_sites,slopesdf)
def_sites$siteId=as.numeric(def_sites$siteId)
join=left_join(def_samples,def_sites,by='siteId')
join$Slope_WS=unlist(join$slopes)
calculatedPredictors2=join[,c(1,9)]
