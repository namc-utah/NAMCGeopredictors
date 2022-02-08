#

def_samples = NAMCr::query(
  api_endpoint = "samples",
  include = c("sampleId", "siteId", "sampleDate"),
  sampleIds = sampleId,

)
# getting watershed
def_sites = NAMCr::query(
  api_endpoint = "siteInfo",
  include = c("siteId", "siteName", "usState", "location", "catchment"),
  siteId = def_samples$siteId
)
#getting a list of needed predictors
def_predictors = NAMCr::query(
  api_endpoint = "samplePredictorValues",
  include = c(
    #"boxid",
    #"sampleId",
    #"sampleDate",
    #"siteId",
    "predictorId",
    "status",
    "abbreviation",
    "predictorValue",
    "calculationScript",
    "isTemporal"
    #,"geometry_file_path",
    #"is_gee"

  ),
  sampleId = sampleId
  #modelId = modelId
)

def_predictors = def_predictors[def_predictors$status != "current",]



point2process =  geojsonsf::geojson_sf(def_sites$location[1])
polygon2process =  sf::st_make_valid(geojsonsf::geojson_sf(def_sites$catchment[1]))


pred_fns[["extract_watershed_mean"]](
  polygon2process = polygon2process ,
  #point2process =  geojsonsf::geojson_sf(def_sites$location[1]) ,
  predictor_name = 'Tmax_WS',
  predictor_geometry = pred_geometries[['Tmax_WS']]
  #geometry_input_path <-
  #  paste0(pred_geometry_base_path, predictor$geometry_file_path),
  #CurrentYear = lubridate::year(def_samples$sampleDate[1]),
  #JulianDate = lubridate::yday(def_samples$sampleDate[1]),
  #USGS_NED=USGS_NED
)

def_predictors=subset(def_predictors,abbreviation=="Tmax_WS")
def_predictors=def_predictors[1,]
eval(parse(text=paste0('ER13')))

