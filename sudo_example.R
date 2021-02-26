requireNamespace("NAMCr")

# The below outline works on a samples driven process. There is no
# looping over samples.
# 
# Process requires high level input of SampleId and ModelId or they 
# need to be determined via other means (i.e. model geofencing Or 
# models assigned to Projects etc.)
# 
# ---------------------------------------------------------------
# Determine sample / site to process
# ---------------------------------------------------------------
# 
def_samples = NAMCr::query(
  api_endpoint = "samples",
  include = c("sampleId","siteId"),
  sampleId = c(...),
  boxId = c(...),
  ...
)

def_sites = NAMCr::query(
  api_endpoint = "sites",
  include = c("predictorId","abbreviation","usState","location","catchment"),
  siteId = def_samples$siteId
)

  
# ---------------------------------------------------------------
# Query for the model / predictor definitions
# ---------------------------------------------------------------
#
def_models = NAMCr::query(
  api_endpoint = "models",
  include = c("predictorId","abbreviation",""),
  modelId = uniqu(def_sites_models$modelId  )
)

#**** Need missing API endpoint to join models to predictors
# def_models_predictors = NAMCr::query(...)

def_predictors = NAMCr::query(
  api_endpoint = "predictors",
  include = c("predictorId","abbreviation","calculationScript","raster_name","formula_name"),
  modelId = unique( def_models_predictors$modelId )
)



# ---------------------------------------------------------------
# Query for previously calculated predictor values
# ---------------------------------------------------------------
#
def_sitePredictorValues = NAMCr::query(
  api_endpoint = "sitePredictorValues",
  include = c("predictorId"),
  siteId = def_samples$siteId
)
# Removed already calculated predictors from list to process
def_predictors = def_predictors %>% 
  filter( !(predictorId %in% def_sitePredictorValues$predictorId) )


# ---------------------------------------------------------------
# Store rasters in a list variable to enable referencing by name
# ---------------------------------------------------------------
# 
pred_rasters = list(
  RH_WS.ras = raster("/Users/alexhernandez/Desktop/BUG_BLM/ZonalTest/GIS_Stats01/Climate/Data/rhmean_usgs/w001001.adf"),
  PMIN_WS.ras<-raster("/Users/alexhernandez/Desktop/BUG_BLM/ZonalTest/GIS_Stats01/Climate/Data/pmin_usgs/w001001.adf"),
  .......
)
# the above can be replaced by something like this once raster names are stored in database
pred_rasters = list()
# Base path will likely be set by some configuration in the future
raster_base_path = "/Users/alexhernandez/Desktop/BUG_BLM/ZonalTest/GIS_Stats01/Climate/Data/pmin_usgs/"

unique_rasters = unique( def_predictors$raster_name )

for(uRaster in unique_rasters){
  pred_rasters[[ uRaster ]] = raster( paste0(raster_base_path, def_predictors[[ uRaster ]]) )
}



#********* View "Predictor function definitions" near bottom of this script
# to understand what is being done in the predictor calcs below
#*************************************************************************


# ---------------------------------------------------------------
# Loop through predictors
# ---------------------------------------------------------------
#
# uses environment[[ function_name ]]() syntax to call each predictor function
# Data needs to be in json format
media = c()
for(iPred in seq_len( nrow(def_predictors) )){

  media[iPred] = jsonlite::toJSON(
    pred_fns[[ def_predictors$calculationScript[iPred] ]](
      polygon2process = geojson_sf( def_sites$catchment[1] ),
      predictor_name = def_predictors$abbreviation[ iPred ],
      predictor_raster = pred_rasters[[ def_predictors$raster_name[ iPred ] ]],
      formula_type = def_predictors$formula_type[ iPred ]
    )
  )
}



# ---------------------------------------------------------------
# Save predictors
# ---------------------------------------------------------------
#
# Uses logical indexing to decipher between temporal and non
NAMCr::save(
  api_endpoint = "newSamplePredictorValue",
  sampleId = def_samples$sampleId[1],
  predictorId = def_predictors$predictorId,
  data = media[  def_predictors$isTemporal ]
)
NAMCr::save(
  api_endpoint = "newSitePredictorValue",
  siteId = def_samples$siteId[1],
  predictorId = def_predictors$predictorId,
  data = media[ !def_predictors$isTemporal ]
)



# ----------------------------------------------------------------------------------
# Predictor function definitions ***************************************************
# ----------------------------------------------------------------------------------
#

# Define environment for predictor function 
# This will keep global environment clean and also allow functions to be called by name
pred_fns = new.env( parent = emptyenv() )                                               # This line needs to be before "Loop through predictors"
# This will source all files in a sub-directory named "predictor_fns". If all functions
# are in that sub-directory this makes it easy to distinguish them
sapply( list.files( "./predictor_fns", full.names = TRUE), source )                     # This line needs to be before "Loop through predictors"


# Functions like below can be placed into separate R files have functions grouped by type

# Suggest making one function for predictors that perform the same function
# Also add a variable to all functions to pass in the name of the predictor (i.e. in example below 'RH_WS' would be passed in for predictor_name)
# Take function below:
pred_fns$RH_WS<-function(polygon2process){
  validgeometry<-st_make_valid(polygon2process)
  validgeometry$RH_WS<-exact_extract(RH_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$RH_WS)
  colnames(media)<-"RH_WS"
  return(media)
}

# and convert it to this:
# Where predictor_name, predictor_raster and formula type all come from the predictors API query
pred_fns$basic_mean_fn = function(polygon2process, predictor_name, predictor_raster, formula_type){
  validgeometry<-st_make_valid(polygon2process)
  validgeometry[[predictor_name]]<-exact_extract(pred_raster,validgeometry,formula_type)
  media<-as.data.frame(validgeometry[[predictor_name]])
  colnames(media)<-predictor_name
  return(media)
}

# this can be converted likewise:
pred_fns$mean_NED_fn = function(polygon2process, predictor_name, predictor_raster, formula_type){
  validgeometry<-st_make_valid(polygon2process)
  validgeometry[[predictor_name]]<-NA
  ptm <- proc.time()
  for (i in 1:nrow(validgeometry)){
    tryCatch({ #if an error is found then it is printed, but the loop does not break and continues with the next iteration
      objecto<-validgeometry[i,] # Take the first feature
      elmean<-ee_extract(predictor_raster, objecto, fun = ee$Reducer[[formula_type]](), scale=30)%>% as_tibble()
      validgeometry[[4]][i]<-elmean
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
  }
  proc.time() - ptm
  validgeometry[[predictor_name]]<-unlist(validgeometry[[predictor_name]])
  media<-as.data.frame(validgeometry[[predictor_name]])
  colnames(media)<-predictor_name
  return(media)
}


# With the update above we would then store 'basic_mean_fn' in the database 
# predictors table under the 'calculationScript' field for all predictors that
# follow its calculation pattern.
# Likewise predictor_raster's associated filename is in the database and we would
# need a new predictor database field to hold the formula_type (i.e. mean, min, max, etc) 
# 
# 

