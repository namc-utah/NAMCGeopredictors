#read in csv with just sampleId and predictors, sampleId should be the first column
pred=read.csv("C:/Users/jenni/Box/NAMC (Trip Armstrong)/OE_Modeling/Geospatial predictors/UT_2021_pred.csv")
#pivot the data
predp=tidyr::pivot_longer(pred,cols=c(2:ncol(pred)), names_to="abbreviation")

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
predsfinal=subset(predsfinal,status!="Valid")

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

#verify that it worked
predictorValues=NAMCr::query("samplePredictorValues",sampleIds=unique(predsfinal$sampleId))
