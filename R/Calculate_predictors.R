#sink("sink-examp.txt",type="message")
#close()
#'
#' ####### Calculate predictors for a box or project
#' #' Calculate predictors
#' #' @description
#' #' @details saving each predictor for each sample one at a time in the database
#' #'
#' #' @param boxId if boxId is absent function will use projectId
#' #' @param projectId only used if boxId is absent
#' #' @param pred_geometry_base_path
#' #' @param SQLite_file_path
#' #'
#' #' @return none
#' #' @export
#' #'
#' #' @examples
#' calculate_predictors = function(boxId,projectId, pred_geometry_base_path,SQLite_file_path) {
    # ---------------------------------------------------------------
    # get existing predictor values and which predictor values need calculated based on which models are associated with each sample
    # ---------------------------------------------------------------
    # getting sample info including date
    if (exists("boxId")){
      def_samples=NAMCr::query("samples",include = c("sampleId", "siteId", "sampleDate"),boxId=boxId)
    }else {def_samples=NAMCr::query("samples",include = c("sampleId", "siteId", "sampleDate"),projectId=projectId)
    }

    # getting a list of samples and predictor values from the database
    def_predictors = NAMCr::query(
      api_endpoint = "samplePredictorValues",
      sampleIds = def_samples$sampleId
      #modelId = modelId
    )
    #subset this list to only samples/predictors that need calculated

     #def_predictors = def_predictors[def_predictors$status != "Valid",]

    modelpred=NAMCr::query("predictors",modelId=modelId)
    def_predictors=subset(def_predictors,predictorId %in% modelpred$predictorId)

    # ---------------------------------------------------------------
    # Get the coordinates and COMID for each sample by looping over the siteInfo end point.
    # Get watersheds for each sample by pulling in watersheds by siteId from the mastersheds file on box
    # ---------------------------------------------------------------
    # get list of sites to loop over
    siteIds=unlist(unique(def_predictors$siteId))
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


    # ---------------------------------------------------------------
    # Get unique list of predictors  that need calculated
    # ---------------------------------------------------------------
    # subset samplePredictor values data frame to only include predictor pertinent info
    # this ideally would just be a call to the predictor endpoint based on predictorIds in the above data frame
    predictors=def_predictors[,c("predictorId","abbreviation","isGee","geometryFilePath","calculationScript","isTemporal")]
    #aggregate data frame to have one row per predictor
    predictors=dplyr::distinct(predictors)


    # ---------------------------------------------------------------
    # Store predictor geometries (raster, vector, or google earth engine) in a list variable to enable referencing by name
    # ---------------------------------------------------------------
    # load in google earth engine elevation layer for any elevation or slope predictors, really should be using source here instead but not in samplePredictorValues endpoint
    if (any(def_predictors$isGee=="true")) {
      ee_Initialize()
      USGS_NED = ee$Image("USGS/NED")$select("elevation")
    } else {
      USGS_NED = NA
      }

    #create empty list to store geometries in
    pred_geometries = list()
    #create list of predictors to loop through
    predlist=unlist(unique(predictors$abbreviation))

    # loop through each predictor in the predictors data frame to load in all needed predictor geometries
    for (p in 1:length(predlist)) {
      tryCatch({
        # change "" to is.na once end points are fixed to have this included
        if (predictors$abbreviation[p]=="SUMMER"|predictors$abbreviation[p]=="NHDSLOPE"|predictors$abbreviation[p]=="Slope_WS"|predictors$abbreviation[p]=="WINTER"|predictors$abbreviation[p]=="pct_46003"){
          pred_geometries[[predictors$abbreviation[p]]] = NA
        } else if (is.na(predictors$geometryFilePath[p]) == TRUE|predictors$geometryFilePath[p]=="") {
          pred_geometries[[predictors$abbreviation[p]]] = NA
        } else if (!grepl(".shp", predictors$geometryFilePath[p])) {
          pred_geometries[[predictors$abbreviation[p]]] = raster::raster(paste0(pred_geometry_base_path,
                                                                                predictors$geometryFilePath[p]))
        } else {
          pred_geometries[[predictors$abbreviation[p]]] = sf::st_read(paste0(pred_geometry_base_path,
                                                                             predictors$geometryFilePath[p]))
          pred_geometries[[predictors$abbreviation[p]]] = sf::st_make_valid(pred_geometries[[predictors$abbreviation[p]]]) # Fix invalid polygon geometries
        }
      },  error = function(e) {
        cat(paste0("\n\tERROR READING IN PREDICTOR GEOMETRY: ",predictors$abbreviation[p],"\n",
                   pred_geometry_base_path,predictors$geometryFilePath[p]),"\n")
        str(e, indent.str = "   ")
        cat("\n")
      })
    }

    # ---------------------------------------------------------------
    # Calculate predictors
    # ---------------------------------------------------------------
    #subset the predictor values to be calculated to only one predictor at a time
    calculatedPredictorslist=list()
    for (p in 1:length(predlist)){
      tryCatch({
      samples=subset(def_predictors,abbreviation==predictors$abbreviation[p])
      predictor_value=list()
          # for each sample that needs a given predictor calculated
          for (s in 1:nrow(samples)){
            tryCatch({
            # subset the site information for only this sample
            def_sites_sample=subset(def_sites,siteId==samples[s,"siteId"])
            def_watersheds_sample=subset(def_watersheds, siteId==samples[s,"siteId"])
            # Data needs to be in json format
              if( nrow(def_watersheds_sample)>0) {
              polygon2process = def_watersheds_sample
              } else {polygon2process = NA
             print(paste0("siteId=",samples[s,"siteId"]," sampleId=",samples$sampleId[s]," watershed needs delineated"))
               }
            # uses eval() to call each predictor function by name
              predictor_value[[s]] = eval(parse(text=paste0(samples$calculationScript[s])))(
                                polygon2process = polygon2process ,
                                point2process =  geojsonsf::geojson_sf(def_sites_sample$location) ,
                                predictor_name = samples$abbreviation[s],
                                predictor_geometry = pred_geometries[[paste0(samples$abbreviation[s])]],
                                COMIDs=def_sites_sample$waterbodyCode,
                                geometry_input_path <-
                                  paste0(pred_geometry_base_path, samples$geometryFilePath[s]),
                                CurrentYear = lubridate::year(samples$sampleDate[s]),
                                JulianDate = lubridate::yday(samples$sampleDate[s]),
                                USGS_NED=USGS_NED,
                                SQLite_file_path=SQLite_file_path
                                )
            calculatedPredictorslist[[paste0(samples$abbreviation[s])]][[paste0(samples$sampleId[s])]]<-unlist(predictor_value[[s]])
              }, error = function(e) {
              cat(paste0("\n\tERROR calculating: ",samples$abbreviation[s]," ",samples$sampleId[s],"\n"))
              str(e,indent.str = "   "); cat("\n")
            })
            }
      }, error = function(e) {
        cat(paste0("\n\tERROR calculating: ",predictors$abbreviation[p],"\n"))
        str(e,indent.str = "   "); cat("\n")
      })
    }
    calculatedPredictors<-as.data.frame(data.table::rbindlist(calculatedPredictorslist,idcol=TRUE,fill=TRUE))
    row=colnames(calculatedPredictors)[-1]
    calculatedPredictors2=data.table::transpose(calculatedPredictors,make.names=".id")
    calculatedPredictors2$sampleId<-as.numeric(row)
    write.csv(calculatedPredictors2,paste0("modelId_",modelId,"_preds_",Sys.Date(),'.csv'))



    # ---------------------------------------------------------------
    # Save predictors
    # ---------------------------------------------------------------


    #read in csv with just sampleId and predictors, sampleId should be the first column
    #pivot the data
    predp=reshape2::melt(calculatedPredictors2,id.vars=c("sampleId"),variable.name="abbreviation")
    #removeNAs
    predp=subset(predp,is.na(predp$value)==FALSE)

    #get predictorIds from the database
    predictorlist=NAMCr::query("predictors")
    #join predictor ids to the predictor values
    predp=dplyr::left_join(predp,predictorlist,by="abbreviation")

    #get site ids from the database for these sampleIds
    sampleIds=unique(predp$sampleId)
    samples=NAMCr::query("samples",sampleIds=sampleIds)
    #join siteIds to the predictor values
    predp=dplyr::left_join(predp,samples, by="sampleId")

    #subset to only needed columns
    predsfinal=subset(predp,is.na(siteId)==FALSE,select=c("sampleId","siteId","predictorId","abbreviation","value","isTemporal"))

    #compare these values to values already in the database
    predictorValues=NAMCr::query("samplePredictorValues",sampleIds=unique(predsfinal$sampleId))
    predictorValues=predictorValues[,c("sampleId","predictorId","predictorValue","qaqcDate","predictorValueUpdatedDate","status")]
    predsfinal=dplyr::left_join(predsfinal,predictorValues,by=c("sampleId","predictorId"))
    #subset to only include samples/predictors not already in the database
    if(overwrite=='N'){
    predsfinal=subset(predsfinal,status!="Valid")
    } else{}
    #save each row in the database
    for (i in 1:nrow(predsfinal)){
      tryCatch({
        if (predsfinal$isTemporal[i]==TRUE) {
          NAMCr::save(
            api_endpoint = "setSamplePredictorValue",
            sampleId = predsfinal$sampleId[i],
            predictorId = predsfinal$predictorId[i],
            value = predsfinal$value[i]
          )
        } else{
          NAMCr::save(
            api_endpoint = "setSitePredictorValue",
            siteId = predsfinal$siteId[i],
            predictorId = predsfinal$predictorId[i],
            value = predsfinal$value[i]
          )
        }
      }, error = function(e) {
        cat(paste0("\n\tERROR saving: ",predsfinal$sampleId[i]," ",predsfinal$predictorId[i],"\n"))
        str(e,indent.str = "   "); cat("\n")

      })
    }


    # ---------------------------------------------------------------
    # QC results
    # ---------------------------------------------------------------
    #load in predictors that were just saved
    testpredictorValues=NAMCr::query("samplePredictorValues",sampleIds=unique(def_samples$sampleId))
    testpredictorValues=subset(testpredictorValues,status=="Valid")
    testpredictorValues$Type="Test"

    # load in reference data predictors for the model of interest
    referencesites=NAMCr::query("modelSamples",modelId=modelId)
    referencepred=NAMCr::query("samplePredictorValues",sampleIds=unique(referencesites$sampleId))
    referencepred=subset(referencepred,status=="Valid")
    referencepred$Type="Ref"
    all_preds=rbind(testpredictorValues,referencepred)
    all_preds=subset(all_preds,predictorId %in% modelpred$predictorId)
    all_preds$predictorValue=as.numeric(all_preds$predictorValue)

    #pivot data for boxplots
    all_preds_wider=as.data.frame(tidyr::pivot_wider(all_preds,id_cols=c("sampleId","Type"),names_from="abbreviation",values_from="predictorValue"))


    png(paste0("modelId_",modelId,"_preds_",Sys.Date(),'.png'),height=1000,width=2000,units="px")
    par(mfrow=c(3,7))
    for (i in 3:(length(all_preds_wider))) {
      boxplot(all_preds_wider[,i]~all_preds_wider$Type, main=names(all_preds_wider)[i],ylab="",xlab="",col=c(7,4))
    }
    dev.off()


    # ---------------------------------------------------------------
    # Set QC date in database if predictors look good
    # ---------------------------------------------------------------
    for (i in 1:nrow(testpredictorValues)){
      tryCatch({
        if (testpredictorValuesl$isTemporal[i]==TRUE) {
          NAMCr::save(
            api_endpoint = "setSamplePredictorValue",
            sampleId = testpredictorValues$sampleId[i],
            predictorId = testpredictorValues$predictorId[i],
            qaqcDate=system.date()
          )
        } else{
          NAMCr::save(
            api_endpoint = "setSitePredictorValue",
            siteId = testpredictorValues$siteId[i],
            predictorId = testpredictorValues$predictorId[i],
            qaqcDate=system.date()
          )
        }
      }, error = function(e) {
        cat(paste0("\n\tERROR saving: ",testpredictorValues$sampleId[i]," ",testpredictorValues$predictorId[i],"\n"))
        str(e,indent.str = "   "); cat("\n")

      })
    }



    #}

#'
#' ###### function to run all samples in the database at once. API endpoint still needs developed
#' #' run all predictors for all samples at once
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' process_predictors = function() {
#'   logger = Logger$new(
#'     logPath = "./",
#'     fileName = "",
#'     enabled = TRUE,
#'     consoleOutput = TRUE,
#'     appendDate = TRUE
#'   )
#'   logger$startLog()
#'
#'
#'   def_samples = NAMCr::query(api_endpoint = "samples2process",
#'                              include = c("sampleId"))
#'   for (sample in def_samples) {
#'     process_sample_predictors(sample$sampleId)
#'   }
#'   logger$stopLog()
#' }
#'
#'
#' ####### run predictors for a box
#' #' predictors for each box
#' #'
#' #' @param boxId
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' process_box_predictors = function(boxId) {
#'   logger = Logger$new(
#'     logPath = "/",
#'     fileName = "",
#'     enabled = TRUE,
#'     consoleOutput = TRUE,
#'     appendDate = TRUE
#'   )
#'   logger$startLog()
#'
#'   def_boxes = NAMCr::query(
#'     api_endpoint = "samples",
#'     include = c("sampleId"),
#'     boxIds = boxId
#'   )
#'
#'   for (i in seq_len(nrow(def_boxes))) {
#'     process_sample_predictors(def_boxes$sampleId[i])
#'   }
#'
#'   by(def_boxes, seq_len(nrow(def_boxes)), function(sample) {
#'     process_sample_predictors(sample$sampleId)
#'   })
#'
#'   logger$stopLog()
#' }


