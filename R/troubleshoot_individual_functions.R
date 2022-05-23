#-----------------------------------------------------------------------
# input a sampleId and the predictor abbreviation you are troubleshooting
#------------------------------------------------------------------------
sampleId=111285
predictor_name ="SR_BIGHORNS"

#-----------------------------------------------------------------------
# run this section to get needed function inputs
# if errors occur reading in the predictor geometry or watersheds the issues are likely with those files
#-----------------------------------------------------------------------
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









#getting a list of needed predictors
def_predictors = NAMCr::query(
  api_endpoint = "samplePredictorValues",
  sampleIds = sampleId
)
def_predictors = def_predictors[def_predictors$status != "Valid",]
def_predictors = subset(def_predictors,abbreviation==predictor_name)

def_sites=as.data.frame(def_sites)

point2process =  geojsonsf::geojson_sf(def_sites$location[1])
polygon2process =  )
#choose one of these options
#option 1 raster data
predictor_geometry= raster::raster(paste0(pred_geometry_base_path,
                                         def_predictors$geometryFilePath))
#option 2 vector data
predictor_geometry=sf::st_read(paste0(pred_geometry_base_path,
                                      def_predictors$geometryFilePath))
                      predictor_geometry=sf::st_make_valid(predictor_geometry)
JulianDate = lubridate::yday(def_samples$sampleDate[1])
CurrentYear = lubridate::year(def_samples$sampleDate[1])
geometry_input_path <-"C:/Users/jenni/Box/NAMC (Trip Armstrong)/GIS/GIS_Stats/CONUS/streams/NHD_West_str_ord.shp"

#------------------------------------------------------------------------------
# run the problematic indicator function line by line to see what lines are giving issues
#------------------------------------------------------------------------------
# to find the function name see the def_predictors calculationScript column- ideally in the UI or predictor endpoint
# to find which script the function is in use the predictorType column- ideally in the UI or predictor endpoint





