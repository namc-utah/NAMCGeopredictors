
###### function to run all samples in the database at once. API endpoint still needs developed
#' run all predictors for all samples at once
#'
#' @return
#' @export
#'
#' @examples
process_predictors = function() {
  logger = Logger$new(
    logPath = "./",
    fileName = "",
    enabled = TRUE,
    consoleOutput = TRUE,
    appendDate = TRUE
  )
  logger$startLog()


  def_samples = NAMCr::query(api_endpoint = "samples2process",
                             include = c("sampleId"))
  for (sample in def_samples) {
    process_sample_predictors(sample$sampleId)
  }
  logger$stopLog()
}


####### run predictors for a box
#' predictors for each box
#'
#' @param boxId
#'
#' @return
#' @export
#'
#' @examples
process_box_predictors = function(boxId) {
  logger = Logger$new(
    logPath = "/",
    fileName = "",
    enabled = TRUE,
    consoleOutput = TRUE,
    appendDate = TRUE
  )
  logger$startLog()

  def_boxes = NAMCr::query(
    api_endpoint = "samples",
    include = c("sampleId"),
    boxIds = boxId
  )

  for (i in seq_len(nrow(def_boxes))) {
    process_sample_predictors(def_boxes$sampleId[i])
  }

  by(def_boxes, seq_len(nrow(def_boxes)), function(sample) {
    process_sample_predictors(sample$sampleId)
  })

  logger$stopLog()
}


####### run predictors for one sample at a time
#' Process sample predictors
#' @description
#' @details saving each predictor for each sample one at a time in the database
#'
#' @param sampleId
#' @param config
#'
#' @return none
#' @export
#'
#' @examples
process_sample_predictors = function(sampleId, config = config) {
  tryCatch({
    # ---------------------------------------------------------------
    # get needed inputs from the database
    # ---------------------------------------------------------------
    # getting sample info including date
    def_samples = NAMCr::query(
      api_endpoint = "samples",
      include = c("sampleId", "siteId", "sampleDate"),
      sampleIds = sampleId,

    )
    # getting watershed
    def_sites = NAMCr::query(
      api_endpoint = "siteInfo",
      include = c("siteId", "siteName", "usState", "location", "catchment"),
      siteIds = def_samples$siteId
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
        "isTemporal",
        #"geometry_file_path",
        #"is.rgee"

      ),
      sampleId = sampleId
    )

    def_predictors = def_predictors[def_predictors$status != "current",]

    preddb=read.csv(temp_predictor_metadata)
    preddb=preddb[,c("abbreviation","geometry_file_path","is_gee")]
    def_predictors=dplyr::left_join(def_predictors,preddb,by="abbreviation")

    # ---------------------------------------------------------------
    # Store predictor geometries (raster, vector, or google earth engine) in a list variable to enable referencing by name
    # ---------------------------------------------------------------
    if (any(def_predictors$is.gee)) {
      ee_Initialize()
      USGS_NED = ee$Image("USGS/NED")$select("elevation")
    } else {
      USGS_NED = NA
      }


    pred_geometries = list()

    by(def_predictors, seq_len(nrow(def_predictors)), function(predictor) {
      tryCatch({
        # change "" to is.na once end points are fixed to have this included
        if (predictor$geometry_file_path=="") {
          pred_geometries[[predictor$abbreviation]] = NA
        } else if (!grepl(".shp", predictor$geometry_file_path)) {
          pred_geometries[[predictor$abbreviation]] = raster::raster(paste0(
            pred_geometry_base_path,
            predictor$geometry_file_path
          ))
        } else {
          pred_geometries[[predictor$abbreviation]] = sf::st_read(paste0(
            pred_geometry_base_path,
            predictor$geometry_file_path
          ))
          pred_geometries[[predictor$abbreviation]] = sf::st_make_valid(pred_geometries[[predictor$abbreviation]]) # Fix invalid polygon geometries
        }
#
#         # ---------------------------------------------------------------
#         # Loop through samples
#         # ---------------------------------------------------------------
#         by(def_predictors, seqlen(nrow(def_predictors)), function(sample) {
#
#         def_sites = NAMCr::query(
#             api_endpoint = "siteInfo",
#             include = c("siteId", "siteName", "usState", "location", "catchment","waterbodyCode","sampleId"),
#             sampleId =
#           )
#
#

      # ---------------------------------------------------------------
      # Calculate predictors
      # ---------------------------------------------------------------
      #
      # uses environment[[ function_name ]]() syntax to call each predictor function
      # Data needs to be in json format
        if( is.na(def_sites$catchment[1])==FALSE) {
        polygon2process = sf::st_make_valid(geojsonsf::geojson_sf(def_sites$catchment[1]))
        } else {polygon2process = NA
        }


      predictor_value = eval(parse(text=paste0(predictor$calculationScript)))(
        polygon2process = polygon2process ,
        point2process =  geojsonsf::geojson_sf(def_sites$location[1]) ,
        predictor_name = predictor$abbreviation,
        predictor_geometry = pred_geometries[[paste0(predictor$abbreviation)]],
        geometry_input_path <-
          paste0(pred_geometry_base_path, predictor$geometry_file_path),
        CurrentYear = lubridate::year(def_samples$sampleDate[1]),
        JulianDate = lubridate::yday(def_samples$sampleDate[1]),
        USGS_NED=USGS_NED
        )


      # ---------------------------------------------------------------
      # Save predictors
      # ---------------------------------------------------------------
      if (predictor$isTemporal) {
        NAMCr::save(
          api_endpoint = "setSamplePredictorValue",
          sampleId = def_samples$sampleId[1],
          predictorId = predictor$predictorId,
          value = predictor_value
        )
      } else{
        NAMCr::save(
          api_endpoint = "setSitePredictorValue",
          siteId = def_samples$siteId[1],
          predictorId = predictor$predictorId,
          value = predictor_value
        )
      }
      }, error=function(e){
        cat(paste0("\n\tPREDICTOR ERROR: ",predictor$abbreviation,"\n"))
        str(e,indent.str = "   "); cat("\n")
      })
    })
  }, error = function(e) {
    cat(paste0("\n\tSAMPLE ERROR: ",sampleId,"\n"))
    str(e,indent.str = "   "); cat("\n")
  })
}


# -----------------------------------------------------------------------
# -----------------------------------------------------------------------
# Alternative ways of getting a list of predictors needed for each sample

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


