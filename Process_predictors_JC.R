requireNamespace("NAMCr")
source("Config.R")
source("PredictorFunctions.R")



###### function to run all samples in the database at once. API endpoint still needs developed
process_predictors = function(){
  def_samples = NAMCr::query(
    api_endpoint = "samples2process",
    include = c("sampleId")
  )
  for(sample in def_samples){
    process_sample_predictors(sample$sampleId)
  }
}


####### run predictors for a box
process_box_predictors = function(boxId) {
  def_boxes = NAMCr::query(
    api_endpoint = "samples",
    include = c("sampleId"),
    boxIds = boxId
  )
  
  for (i in nrow(def_boxes)) {
    process_sample_predictors(def_boxes[i,])
  }
}


####### run predictors for one sample at a time 
process_sample_predictors = function(sampleId, config=config){

# ---------------------------------------------------------------
# get needed inputs from the database
# ---------------------------------------------------------------
  # getting sample info including date
   def_samples = NAMCr::query(
    api_endpoint = "samples",
    include = c("sampleId","siteId","sampleDate"),
    sampleIds = sampleId, 
    
  )
  # getting watershed
  def_sites = NAMCr::query(
    api_endpoint = "siteInfo",
    include = c("siteId","siteName","usState","location","catchment"),
    siteId = def_samples$siteId[1]
  )
  # getting a list of needed predictors
  def_predictors = NAMCr::query(
    api_endpoint = "samplePredictorValues",
    include = c("predictorId", "status", "abbreviation","calculationScript", "isTemporal"),
    sampleId = def_samples$sampleId[1]
  )
  
  def_predictors = def_predictors[def_predictors$status != "Valid", ]
  
  
# ---------------------------------------------------------------
# Store predictor geometries in a list variable to enable referencing by name
# ---------------------------------------------------------------

  pred_rasters = list()
  for(uPredictor in def_predictors){
    if ( !grepl(".shp",config[[def_predictors$abbreviation]])){
      pred_geometries[[ uPredictor$abbreviation ]] = raster(paste0(config$pred_geometry_base_path, config[[def_predictors$abbreviation]]))
    } else {
      pred_geometries[[ uPredictor$abbreviation ]] = st_read(paste0(config$pred_geometry_base_path, config[[def_predictors$abbreviation]]))
      pred_geometries[[ uPredictor$abbreviation ]] = st_make_valid( pred_geometries[[ uPredictor$abbreviation ]]) # Fix invalid polygon geometries
    } 
    
  }
  
# ---------------------------------------------------------------
# Loop through predictors
# ---------------------------------------------------------------
#
# uses environment[[ function_name ]]() syntax to call each predictor function
# Data needs to be in json format
  
  for(iPred in seq_len( nrow(def_predictors) )){
    
    predictor_value = jsonlite::toJSON(
      pred_fns[[ def_predictors$calculationScript[iPred] ]](
        polygon2process =  def_sites$catchment[1] ,
        point2process =  def_sites$location[1] ,
        predictor_name = def_predictors$abbreviation[ iPred ],
        predictor_geometry = pred_geometries[[ def_predictors$abbreviation[ iPred ] ]],
        CurrentYear = lubridate::year(def_sites$sampleDate),
        JulianDate = lubridate::yday(def_sites$sampleDate)
             )
    )

# ---------------------------------------------------------------
# Save predictors
# ---------------------------------------------------------------
    if(def_predictors$isTemporal[iPred]){
      NAMCr::save(
        api_endpoint = "newSamplePredictorValue",
        sampleId = def_samples$sampleId[1],
        predictorId = def_predictors$predictorId,
        value = predictor_value
      )
    }else{
      NAMCr::save(
        api_endpoint = "newSitePredictorValue",
        siteId = def_samples$siteId[1],
        predictorId = def_predictors$predictorId,
        value = predictor_value
      )
    }
  }
}


# ----------------------------------
# Alternative ways of getting 

# def_sites_models = NAMCr::query(
#   api_endpoint = "siteModels",
#   include = c("siteId", "modelId"),
#   siteId = def_samples$siteId
# )
# 
# # ---------------------------------------------------------------
# # Query for the model / predictor definitions
# # ---------------------------------------------------------------
# #
# def_models = NAMCr::query(
#   api_endpoint = "models",
#   include = c("modelId","abbreviation",""),
#   
# )
# 
# def_models = def_models[ def_models$modelId %in% unique(def_sites_models$modelId), ]
# #**** Need missing API endpoint to join models to predictors
# # def_models_predictors = NAMCr::query(...)
# 
# #modify "predictors" endpoint to accept multiple model Ids
# def_predictors = NAMCr::query(
#   api_endpoint = "predictors",
#   include = c("predictorId","abbreviation","calculationScript"),
#   modelId = unique( def_models_predictors$modelId )
# )
# 
# 
# 
# # ---------------------------------------------------------------
# # Query for previously calculated predictor values
# # ---------------------------------------------------------------
# #
# # If state is dirty allow the script to recalculate site predictors
# if( def_samples$predictorState[1] != "dirty" ) {
#   def_sitePredictorValues = NAMCr::query(
#     api_endpoint = "sitePredictorValues",
#     include = c("predictorId"),
#     siteId = def_samples$siteId
#   )
#   # Removed already calculated predictors from list to process
#   def_predictors = def_predictors %>% 
#     filter( !(predictorId %in% def_sitePredictorValues$predictorId) )
# }


