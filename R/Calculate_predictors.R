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
Rver<-R.Version()$version.string
Rver<-substr(Rver,11,15)
    #create an object that is just the list of sites with no sheds in MS
    #allows us to quickly look for sites without sheds, despite the error messages
#no_sheds<-list()
watershed_models=c(1,2,3,7,8,9,13,14,15,16,17,18,19,20,21,22,23)
    #small addition for OR models (10,11,and 12)
    #run this section for OR and no not run the if (exists ("boxID"))
    if(0){
    ClPl<-model10
    NBR<-model12
    MWCP<-model11

    def_samples=NAMCr::query("samples",include = c("sampleId", "siteId", "sampleDate"),sampleIds=ClPl) #change the sampleIds accordingly
    }


    if (exists("boxId")){
      def_samples=NAMCr::query("samples",include = c("sampleId", "siteId", "sampleDate"),boxId=boxId)
    }else {def_samples=NAMCr::query("samples",include = c("sampleId", "siteId", "sampleDate"),projectId=projectId)
    }

#def_samples<-def_samples[def_samples$sampleId < 210554,]
    # getting a list of samples and predictor values from the database
if(Rver=='4.3.2'){
  p_list<-list()
for(jj in 1:nrow(def_samples)){
def_predictors = NAMCr::query(
      api_endpoint = "samplePredictorValues",
      sampleIds = def_samples$sampleId[jj]
      #modelIds = modelId
    )
p_list[[jj]]<-def_predictors
}
def_predictors<- do.call(rbind,p_list)
}else{
  def_predictors = NAMCr::query(
    api_endpoint = "samplePredictorValues",
    sampleIds = def_samples$sampleId
  )
}

    #subset this list to only samples/predictors that need calculated

    modelpred=NAMCr::query("predictors",modelId=modelId)

    def_predictors=subset(def_predictors,predictorId %in% modelpred$predictorId)

     #def_predictors = def_predictors[def_predictors$status != "Valid",]


    #This little section will look for COMIDs in a set.
    #if no COMID exists for a site, a list will be populated
    #from which we can either 1) retrieve a COMID via the following loop
    #or ask AIM for it, if it is an AIM site.
    #(the code will return  a COMID,
    #but AIM should be the ones giving us their COMIDs for COC reasons etc.)
    #Once you are given a list of COMIDs, you can either manually add them to sites
    #Via INSTAR
    #or ask North Arrow Research (via GitHub) to bulk import them.
    #General advice: if there are more than 10 site without a COMID,
    #ask NAR. Don't waste more time manually inputting info than
    #is needed.
    #Typically, AIM will provide all COMIDS, so any small set of samples
    #without COMIDs is usually ~10 samples. And most small clients
    #don't even want an index score. Proceed as you see fit.

    #Step 1) checking for sites without COMIDs
if(exists("boxId")){
    comid_check = NAMCr::query(
      api_endpoint = "sites",
      args = list(boxIds=boxId))
}else{
    comid_check = NAMCr::query(
      api_endpoint = "sites",
      args = list(projectIds=projectId))
}


    #subset out the empties
    #to use as a condition in Step 2
    comid_check<-comid_check[is.na(comid_check$waterbodyCode)==T,]

    #line that finds what models needs sheds
    #models$modelId[sub("\\;.*", "", models$description)=="watershed delineation required"]


    #leave this collapsed for simplicity, open the brackets
    #to edit or peek at the inner workings

    #Step 2) getting COMID, if needed
    #once this step finishes,
    #the fxn will print out the list,
    #(or just look at freshCOMIDs if the list is too large)
    #the samples and def_predictors objects will be
    #SMALLER after this step (if it runs)
    #because we are subsetting out sites that do not have COMIDs
    #you can either stop after this section, input the missing
    #COMIDs, and run again, or run two "incomplete" cycles of this whole script
    if(nrow(comid_check>=1)){
      #make a litte df
    COMID_blanks<-data.frame(comid_check$siteId,comid_check$siteName,comid_check$longitude,comid_check$latitude)
    #rename
    names(COMID_blanks)<-c('siteId','name','lon','lat')
    #neat little fxn
    #that uses streamcat and nhdplustools
    #to get a COMID for each site without one
    sc_get_comid <- function(df = NULL, xcoord = NULL,
                             ycoord=NULL, crsys=NULL) {
      if (!'sf' %in% class(df) & ((is.null(xcoord)) |
                                  (is.null(ycoord)) |
                                  (is.null(crsys)))) {
        "\nMake sure you supply parameters for xcoord, ycoord, and a crs as an epsg code."
      } else {
        df <- sf::st_as_sf(df, coords = c(xcoord, ycoord), crs = crsys, remove = FALSE)
      }

      run_for <- 1:nrow(df)
      output <- do.call(rbind, lapply(1:nrow(df), function(i){
        comid <- nhdplusTools::discover_nhdplus_id(df[i,c('geometry')])
        if (length(comid)==0L) comid <- NA else comid <- comid
        return(comid)
      }))
      output <- as.data.frame(output)
      names(output)[1] <- 'COMID'
      if (any(is.na(output$COMID))){
        missing <- which(is.na(output$COMID))
        message(cat('The following rows in the input file came back with no corresponding \nCOMIDS, likely because the sites were outside of the \nNHDPlus COMID features: ',as.character(missing)))
      }
      comids <- paste(output$COMID, collapse=',')
      return(comids)
    }


    fresh_COMIDs<-COMID_blanks
    #First convert the dataframe to a sf object
    df <- st_as_sf(fresh_COMIDs, coords = c("lon", "lat"), crs = 4269)

    comids <- sc_get_comid(df) #Get COMIDs for points
    df$COMID <- strsplit(comids, ",")[[1]] #Convert the output from a character string to a vector and append to the sf object
    fresh_COMIDs <- sfheaders::sf_to_df(df, fill = T) #COnvert sf object back to dataframe

    #subest out watershed models
    #if we are missing sheds and sheds need to be computed,
    #no point running the sites and getting errors.
   # if(modelId %in% watershed_models){
    samples<-def_samples[def_samples$siteId %in% COMID_blanks$siteId==F,]
    def_predictors<-def_predictors[def_predictors$siteId %in% COMID_blanks$siteID==F,]
    #}
    print(fresh_COMIDs[,c(1:3)])

}else{message('All sites have a COMID. Lucky you!')}


    # ---------------------------------------------------------------
    # Get the coordinates and COMID for each sample by looping over the siteInfo end point.
    # Get watersheds for each sample by pulling in watersheds by siteId from the mastersheds file on box
    # ---------------------------------------------------------------
    # get list of sites to loop over
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
      #forcing named vector to be dataframe for subsetting
      #if only 1 site, the code above forces it to named vector via unlist()
      def_sites<-data.frame(as.list(def_sites))
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


     #assigning a vector of sites that do not have sheds,
     #based on hte def_watersheds file
     if(modelId %in% watershed_models & exists("fresh_COMIDs")){
     no_sheds<-fresh_COMIDS$siteId[fresh_COMIDS$siteId %in% def_watersheds$siteId==F]
     print(paste(no_sheds, " need watersheds delineated and COMIDs",sep=''))
     }else{
       message("All sites have COMIDs and sheds!")
     }

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
    #no longer loading rgee.
    #will change this to just print a message instead?

    if (any(def_predictors$isGee=="true")) {
      message('at least one predictor requires an elevation function')
      message('all previous rgee elevation functions are now using elevatr')
      #ee_Initialize()
      #USGS_NED = ee$Image("USGS/NED")$select("elevation")
    } else {
      message('no elevation functions are used in this set')
    }

    #create empty list to store geometries in
    pred_geometries = list()
    #create list of predictors to loop through
    predlist=unlist(unique(predictors$abbreviation))

    # loop through each predictor in the predictors data frame to load in all needed predictor geometries
    for (p in 1:length(predlist)) {
      tryCatch({
        # change "" to is.na once end points are fixed to have this included
        if (predictors$abbreviation[p]=="SUMMER"|predictors$abbreviation[p]=="NHDSLOPE"|predictors$abbreviation[p]=="Slope_WS"|predictors$abbreviation[p]=="NHDStreamOrder"|predictors$abbreviation[p]=="WINTER"|predictors$abbreviation[p]=="length_46006"|predictors$abbreviation[p]=="pct_46003"|predictors$abbreviation[p]=="A1_3"|predictors$abbreviation[p]=="A2_5"){
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
      message(p)
      tryCatch({
        samples=subset(def_predictors,abbreviation==predictors$abbreviation[p])
        predictor_value=list()
        # for each sample that needs a given predictor calculated
        for (s in 1:nrow(samples)){

          #remove the sample
          tryCatch({
            # subset the site information for only this sample
            def_sites_sample=subset(def_sites,siteId==samples[s,"siteId"])
            def_watersheds_sample=subset(def_watersheds, siteId==samples[s,"siteId"])

            # Data needs to be in json format
            #if( nrow(def_watersheds_sample)>0) {
              polygon2process = def_watersheds_sample
              point2process=geojsonsf::geojson_sf(def_sites_sample$location)
            #} else {polygon2process = NA

            #now we fill in nosheds
            #with each siteID that does not have a corresponding shed
            #in mastersheds.shp

            #if(modelId %in% watershed_models & exists("fresh_COMIDs")){
            #  print(paste0("siteId=",samples[s,"siteId"]," sampleId=",samples$sampleId[s]," watershed needs delineated"))
            #no_sheds[[s]]<-samples$siteId[s]
            #}
           #}
            # uses eval() to call each predictor function by name
            predictor_value[[s]] = eval(parse(text=paste0(samples$calculationScript[s])))(
              polygon2process = polygon2process ,
              point2process =  point2process,#geojsonsf::geojson_sf(def_sites_sample$location) ,
              predictor_name = samples$abbreviation[s],
              predictor_geometry = pred_geometries[[paste0(samples$abbreviation[s])]],
              COMIDs=def_sites_sample$waterbodyCode,
              geometry_input_path <-
                paste0(pred_geometry_base_path, samples$geometryFilePath[s]),
              CurrentYear = lubridate::year(samples$sampleDate[s]),
              JulianDate = lubridate::yday(samples$sampleDate[s]),
              USGS_NED='elevatr_tile',
              SQLite_file_path=SQLite_file_path
            )
            calculatedPredictorslist[[paste0(samples$abbreviation[s])]][[paste0(samples$sampleId[s])]]<-unlist(predictor_value[[s]])
            }, error = function(e) {
            cat(paste0("\n\tERROR calculating: ",samples$abbreviation[s]," ",samples$sampleId[s],"\n"))
            str(e,indent.str = "   "); cat("\n")
            if(nrow(def_sites_sample) < 1)
              message(paste0(s,' something went wrong with creating the site locations. Check start of loop'))
          })
          #clear that temp path after every site has been processed to avoid
          #clogging up the disc space.
          unlink(paste0(DEM_trashbin,'/*'))
        }
      }, error = function(e) {
        cat(paste0("\n\tERROR calculating: ",predictors$abbreviation[p],"\n"))
        str(e,indent.str = "   "); cat("\n")

      })
    }
    #force the list into a dataframe that you can use later
    #for shed delineation
    #no_sheds<-do.call(rbind,no_sheds)

    calculatedPredictors<-as.data.frame(data.table::rbindlist(calculatedPredictorslist,idcol=TRUE,fill=TRUE))
    row=colnames(calculatedPredictors)[-1]
    calculatedPredictors2=data.table::transpose(calculatedPredictors,make.names=".id")
    calculatedPredictors2$sampleId<-as.numeric(row)
    write.csv(calculatedPredictors2,paste0("modelId_",modelId,"_preds_",Sys.Date(),'.csv'))


calculatedPredictors2
    # ---------------------------------------------------------------
    # Save predictors
    # ---------------------------------------------------------------


    #read in csv with just sampleId and predictors, sampleId should be the first column
    #pivot the data
    predp=reshape2::melt(calculatedPredictors2,id.vars=c("sampleId"),variable.name="abbreviation")
    #removeNAs
    predp=subset(predp,is.na(predp$value)==FALSE)

    #get predictorIds from the database
    #save this as a static file and update periodically?
    #r 4.3.2 does not work with this query
    predictorlist<-read.csv(paste0(predictor_list_path,'generalpredictors.csv'))
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
    if(Rver=='4.3.2'){
      predictorValueList<-list()
      for(y in 1:nrow(predsfinal)){
        pv<-NAMCr::query('samplePredictorValues',
                         sampleIds=predsfinal$sampleId[y])
        predictorValueList[[y]]<-pv
      }
      predictorValues=do.call(rbind,predictorValueList)
    }else{
    predictorValues=NAMCr::query("samplePredictorValues",sampleIds=unique(predsfinal$sampleId))
    }

    predictorValues=predictorValues[,c("sampleId","predictorId","predictorValue","qaqcDate","predictorValueUpdatedDate","status")]
    predsfinal=dplyr::left_join(predsfinal,predictorValues,by=c("sampleId","predictorId"))
    #subset to only include samples/predictors not already in the database
    if(overwrite=='N'){
    predsfinal=subset(predsfinal,status!="Valid")
    } else{}
    #duplicated predictor values?? Every site gets many of the same predictor
    #will follow up with this later, but for now, we just need preds to get calculated and saved.
    #worst case, this code will do nothing.

    predsfinal$burn<-paste(predsfinal$sampleId,predsfinal$abbreviation)
    predsfinal<-predsfinal[!duplicated(predsfinal$burn),]

    if(nrow(predsfinal)>0){
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
    }else{
  message('predictors are already saved in the database.')
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
    # Set QC date in database if predictors look good #code currently not working because you cant save on column at a time, qc date must be saved with the predictor value itself the way the end point is set up!
    # ---------------------------------------------------------------
    for (i in 1:nrow(testpredictorValues)){
      tryCatch({
        if (testpredictorValues$isTemporal[i]==TRUE) {
          NAMCr::save(
            api_endpoint = "setSamplePredictorValue",
            sampleId = testpredictorValues$sampleId[i],
            predictorId = testpredictorValues$predictorId[i],
            qaqcDate=paste0(Sys.Date())
          )
        } else{
          NAMCr::save(
            api_endpoint = "setSitePredictorValue",
            siteId = testpredictorValues$siteId[i],
            predictorId = testpredictorValues$predictorId[i],
            qaqcDate=paste0(Sys.Date())
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

mastersheds[mastersheds$siteId=="44899",]
