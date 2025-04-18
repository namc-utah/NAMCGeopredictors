#run config
#set up rgee and arcpy spatial analyst
#may have to log in to arc pro first!!! log in via web named user (not enterprise account) courtwrightj01
#may need to enable spatial analyst license within arc pro licenses panel
#may need to save NV flow raster locally and open in gis once to generate pyramids
# Define a function to run sa$FlowLength with a progress bar


flow_length_with_progress <- function(in_raster, direction) {

  result <- sa$FlowLength(in_flow_direction_raster = in_raster, direction_measurement = direction)
  cat("sa$FlowLength completed\n")
  return(result)
}

#clippedFlowRaster=sa$ExtractByMask('C:/Users/jenni/OneDrive - USU/Documents/geospatial data/elevation/NVFLD8.tif','C:/Users/jenni/OneDrive - USU/Desktop/NAMCGeopredictors/polygon2processtrans.shp')
#clippedFlowRaster=sa$ExtractByMask('C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC//GIS//GIS_Stats//Nevada//elevation//NVFLD8.tif',)

######## get watersheds ###################
def_samples = NAMCr::query(
  api_endpoint = "samples",
  boxId=

  )
# getting watershed
siteIds=unlist(unique(def_samples$siteId))
siteIds<-siteIds[siteIds %in% c(5245,14432,30640,14433,42677,42695,
                                42889,42903,42904,42909,43205,43206,
                                22799,43212,43233,43235,43236,43237,
                                43241,43243,43263,43254,43245,43239,
                                43234,43216,43258,43252,43250,43249,
                                43248,43247,43244,43215,43209,43262,
                                30661,30651,5242,10743,14437,20817,
                                42911,43260,43242,14266,43197,43207,
                                43210,43227,43238,11083,42686,42887,
                                43202,43213,43214,43226,43228,43229,
                                43230,43240,43246,43251,43256,43261)==F]#14521)==F]
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

###################################################
#raster::raster('C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC//GIS//GIS_Stats//Nevada//elevation//NVFLD8.tif')
########### calculate slope #####################
slopes=list()
for (s in 1:nrow(def_watersheds)){
  message(s)
  message(siteIds[s])
  polygon2process=def_watersheds[s,2]
  #polygon2process=def_watersheds[1,2]
  polygon2processtrans<-sf::st_transform(polygon2process, 5072)# transforming to CRS of NV D8 point Flow Direction
  #write buffer of watershed out as a shapefile
  sf::write_sf(polygon2processtrans,paste0(DEM_trashbin,'//polygon2processtrans.shp'))#write it out to your project working directory!
  # clip the NV flow accumulation grid to the watershed boundary using the spatial analyst extract by mask tool and save to local app data. File path of the new file created will be stored in the extract2 object
  clippedFlowRaster=sa$ExtractByMask(in_raster='C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC WATS Department Files//GIS//GIS_Stats//Nevada//elevation//NVFLD8.tif',in_mask_data=paste0(DEM_trashbin,'//polygon2processtrans.shp'))
  message('Raster has been clipped')
  #calculate flow length using the spatial analyst FlowLength tool and save to local app data. File path of the new file created will be stored in the extract2 object
  message('starting flow length calculation')
  #ShedFlowL=sa$FlowLength(clippedFlowRaster,'UPSTREAM')# previous python code had downstream but not sure why!!
  start <- Sys.time()
  ShedFlowL <- flow_length_with_progress(in_raster = clippedFlowRaster, direction = 'UPSTREAM')
  end <- Sys.time()


  message(paste('Total elapsed time is', end - start))
  if (is.null(ShedFlowL)) {
    message("Flow length calculation skipped due to timeout.")
    next  # Skip to the next iteration
  }
  message('flow length complete! Now calculating...')
  FlowLength=raster::raster(paste0(ShedFlowL))
  max_flow_length=raster::maxValue(FlowLength)
  message('max done!')# code previously multiplied by 10 and then divided by 100. make sure this output number is in proper units (m)
  max_watershed_elevation=ELVmax_WS(polygon2process)
  message('max elev done!')
  min_watershed_elevation=ELVmin_WS(polygon2process)
  message('min elev done!')
  slopes[[s]]=(max_watershed_elevation-min_watershed_elevation)/max_flow_length
  print(paste0("processed_",s))
  print(slopes[[s]])
  unlink(paste0(DEM_trashbin,'/*'))#clean out garbage after each iteration to save space
}


slopesdf=as.data.frame(cbind(slopes))
###########################################

##### join to siteID and sampleID
def_sites=cbind(def_sites,slopesdf)
def_sites$siteId=as.numeric(def_sites$siteId)
join=dplyr::left_join(def_samples,def_sites,by='siteId')
join[join=='NULL']<-0
join$Slope_WS=unlist(join$slopes)
#create dataframe named for saving into database then run save code in calculate predictors

join<-data.frame(siteId=siteIds,
                 predictorId=128,
                 value=0)

if(0){
calculatedPredictors2<-data.frame(
  siteId=join$siteId,
  predictorId=128,
  value = join$Slope_WS
)

for(i in 1:nrow(join)){
NAMCr::save(
  api_endpoint = "setSitePredictorValue",
  siteId = join$siteId[i],
  predictorId = 128 ,
  value = join$Slope_WS[i]
)
  message('saved slope!')
  }
}
sldf=as.data.frame(sl)


#doesnt work for some reason!!!
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
#' Slope_WS<-function(polygon2process,USGS_NED,...){
#'   arc=import("arcpy")
#'   #import the spatial analyst arc py module/ library to get needed functions
#'   sa=reticulate::import("arcpy.sa")
#'   geometry_input_path="C:/Users/jenni/OneDrive - USU/Documents/geospatial data/elevation/NVFLD8.tif"
#'   # watershed slope = rise/ run
#'   ### rise= max watershed elevation - min watershed elevation ###
#'   # call elevation functions to get min and max watershed elevations
#'   ### run= flow length as determined by ArcGIS from DEM flow length raster ###
#'   polygon2processtrans<-sf::st_transform(polygon2process, 5072)# transforming to CRS of NV D8 point Flow Direction
#'   #write buffer of watershed out as a shapefile
#'   sf::write_sf(polygon2processtrans,"polygon2processtrans.shp")#write it out to your project working directory!
#'   # clip the NV flow accumulation grid to the watershed boundary using the spatial analyst extract by mask tool and save to local app data. File path of the new file created will be stored in the extract2 object
#'   clippedFlowRaster=sa$ExtractByMask('C:/Users/jenni/OneDrive - USU/Documents/geospatial data/elevation/NVFLD8.tif','C:/Users/jenni/OneDrive - USU/Desktop/NAMCGeopredictors/polygon2processtrans.shp')
#'   #calculate flow length using the spatial analyst FlowLength tool and save to local app data. File path of the new file created will be stored in the extract2 object
#'   ShedFlowL=sa$FlowLength(clippedFlowRaster,'UPSTREAM')# previous python code had downstream but not sure why!!
#'   FlowLength=raster::raster(paste0(ShedFlowL))
#'   max_flow_length=raster::maxValue(FlowLength) # code previously multiplied by 10 and then divided by 100. make sure this output number is in proper units (m)
#'    max_watershed_elevation=ELVmax_WS(polygon2process,USGS_NED)
#'   min_watershed_elevation=ELVmin_WS(polygon2process,USGS_NED)
#'   media=(max_watershed_elevation-min_watershed_elevation)/max_flow_length
#'   return(media)
#' }
#'
#'
#'
#'
#'
#'
#' slopes=list()
#' for (s in 1:nrow(def_watersheds)){
#'   slopes[[s]]=Slope_WS(polygon2process=def_watersheds[1,3],USGS_NED)
#' }
#' slopesdf=as.data.frame(cbind(slopes))






