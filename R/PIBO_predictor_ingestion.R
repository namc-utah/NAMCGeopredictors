#PIBO predictor ingestion script
#PIBO will calculate their own predictors and send them with the sample metadata
#This script will ingest those predictors into NAMC's database

#read in the data

PIBO_dat<-read.csv('C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC//Research Projects//PIBO//PIBO_historic_predictors_ingestion.csv')
PIBO_dat<-PIBO_dat[,-c(1,2)]
PIBO<-NAMCr::query('samples',sampleIds=PIBO_dat$sampleId)
#read in the predictors that may already have values
#this will happen if we have an existing site
PIBOpreds<-NAMCr::query('samplePredictorValues',sampleIds=PIBO$sampleId,modelIds=9)
#subset only the missing predictors, since they are not in the database yet.
PIBO_samps_nopreds<-PIBO[PIBO$sampleId %in% PIBOpreds$sampleId[PIBOpreds$status!='Valid'],]
PIBOpreds_nopreds<-PIBOpreds[PIBOpreds$sampleId %in% PIBO_samps_nopreds$sampleId & PIBOpreds$status!='Valid',]

#the file that PIBO sends. This should be straight from the metadata sheet they give NAMC
#which is in the order of the samples endpoint
PIBO_pred_imp<-read.csv('C://Users//andrew.caudillo//Box//NAMC//Research Projects//PIBO//PIBOpreds_240321.csv')
PIBO_pred_imp <-PIBO_dat
#assign sampleId
PIBO_pred_imp$sampleId<-c(173138,173148,173160, 173162,173163,173167,185306)#PIBO$sampleId
#again, only subset the samples that do not have existing predictors
PIBO_pred_imp<-PIBO_pred_imp[PIBO_pred_imp$sampleId %in% PIBO_samps_nopreds$sampleId,]
#melt it into long format so we can for loop the saving
PIBO_pred_melted<-as.data.frame(reshape2::melt(PIBO_pred_imp,id='sampleId'))
#join with the original samples query so we get siteId
PIBO_pred_melted<-plyr::join(PIBO_pred_melted,PIBO[,c('sampleId','siteId')],by='sampleId')
#give the predictorId to the the melted data for NAMCr ingestion
PIBO_pred_melted$predictorId<-ifelse(
  PIBO_pred_melted$variable=='TMAX',135,
  ifelse(PIBO_pred_melted$variable=='LOG_LT_PPT',
         136,137))
#saving the predictors
for(i in 1:nrow(PIBO_pred_melted)){
NAMCr::save(
  api_endpoint = "setSitePredictorValue",
  siteId = PIBO_pred_melted$siteId[i],
  predictorId = PIBO_pred_melted$predictorId[i],
  value = PIBO_pred_melted$value[i]
)
  message(paste('predictor ',i,' of ',nrow(PIBO_pred_melted),' done!'))
}
