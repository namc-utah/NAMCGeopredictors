source("C://Users//alex.hernandez//Documents//GitHub//NAMCGeopa//Packages_GeoInputs.R")
source("C://Users//alex.hernandez//Documents//GitHub//NAMCGeopa//Functions_x_Predictor.R")


##### Model functions
UTDEQ_model<-function(polygon2process, points2process){
  inputpolys<-polygon2process
  inputpoints<-points2process
  REACHID<-as.data.frame(geojson_sf(inputpolys)$SiteCode)
  REACHIDP<-as.data.frame(geojson_sf(inputpoints)$SiteCode)
  names(REACHID)<-"SITECODE"
  names(REACHIDP)<-"SITECODEP"
  ELVmean_WSS<-ELVmean_WS(inputpolys)
  ELVmin_WSS<-ELVmin_WS(inputpolys)
  WSA_SQKMS<-WSA_SQKM(inputpolys)
  TMAX_WSS<-TMAX_WS(inputpolys)
  TMEAN_UT_WSS<-TMEAN_UT_WS(inputpolys)
  TMIN_UT_WSS<-TMIN_UT_WS(inputpolys)
  RH_WSS<-RH_WS(inputpolys)
  MEANP_WSS<-MEANP_WS(inputpolys)
  MAXP_WSS<-MAXP_WS(inputpolys)
  MAXWD_WSS<-MAXWD_WS(inputpolys)
  FST32F_WSS<-FST32F_WS(inputpolys)
  EVI_MAX_WSS<-EVI_MAX_WS(inputpolys)
  TMAX_PTT<-TMAX_PT(inputpoints)
  TMEAN_PTT<-TMEAN_UT_PT(inputpoints)
  DD_LAT_PTT<-DD_LAT_PT(inputpoints) 
  dfpolys<-cbind(REACHID,WSA_SQKMS,ELVmean_WSS,ELVmin_WSS,RH_WSS,TMAX_WSS,TMEAN_UT_WSS,TMIN_UT_WSS,MEANP_WSS,
                 MAXP_WSS,MAXWD_WSS,FST32F_WSS,EVI_MAX_WSS)
  names(dfpolys)<-c("SITECODE","SQ_KM","ELEV_MEAN","ELEV_MIN","RH_AVE","TMAX_AVE","TMEAN_AVE","TMIN_AVE","MEANP_AVE",
                    "MAXP_AVE","MAXWD_AVE","FST32F_AVE","EVI_MAX_AVE")
  #dfpolys<-cbind(REACHID,ELVmean_WSS)
  dfpoints<-cbind(REACHIDP,TMAX_PTT,TMEAN_PTT,DD_LAT_PTT)
  names(dfpoints)<-c("SITECODEP","TMAX_PT","TMEANPT","DD_LAT_Y")
  df2render<-merge(dfpolys, dfpoints, by.x="SITECODE",by.y="SITECODEP")
  return(df2render)
}


# ptm <- proc.time()
# AIMtest<-UTDEQ_model(polygon2process = AIM2020.WGS.json,
#                       points2process = AIM2020.WGS.json.points)
# proc.time() - ptm

ptm <- proc.time()
AIMtest01<-UTDEQ_model(polygon2process = AIM2020.WGS.json.simpkeep,
                     points2process = AIM2020.WGS.json.points)
proc.time() - ptm

write.csv(AIMtest01,"C:/Temp/AIM2020/ALL_AIM2020_UTDEQ15Predictors.csv")


PIBO_model<-function(polygon2process, points2process){
  inputpolys<-polygon2process
  inputpoints<-points2process
  REACHID<-as.data.frame(geojson_sf(inputpolys)$Sample_ID)
  REACHIDP<-as.data.frame(geojson_sf(inputpoints)$Sample_ID)
  names(REACHID)<-"Sample_ID"
  names(REACHIDP)<-"Sample_IDP"
  Sq_km<-WSA_SQKM(inputpolys)
  LOG_KM2<-log10(WSA_SQKM(inputpolys))
  TMAX_PTT<-TMAX_PT(inputpoints)
  LOG_LT_PPT_PTT<-LOG_LT_PPT_PT(inputpoints)
  dfpolys<-cbind(REACHID,Sq_km,LOG_KM2)
  names(dfpolys)<-c("Sample_ID","SQ_KM","LOG_KM2")
  #dfpolys<-cbind(REACHID,ELVmean_WSS)
  dfpoints<-cbind(REACHIDP,TMAX_PTT,LOG_LT_PPT_PTT)
  names(dfpoints)<-c("Sample_IDP","TMAX_PT","LOG_LT_PPT_PT")
  df2render<-merge(dfpolys, dfpoints, by.x="Sample_ID",by.y="Sample_IDP")
  return(df2render)
}

ptm <- proc.time()
PIBOtest01<-PIBO_model(polygon2process = PIBO2020.WGS.json.simpkeep,
                       points2process = PIBO2020.WGS.json.points)
proc.time() - ptm

write.csv(PIBOtest01,"C:/Temp/PIBO/ALL_PIBO2020_Predictors.csv")
















# ptm <- proc.time()
# AIMtestELEV3<-UTDEQ_model(polygon2process = AIM2020.WGS.json075.simp,
#                      points2process = AIM2020.WGS.json.points)
# proc.time() - ptm
# 
# 
# 
# ptm <- proc.time()
# AIMtestELEVNOSIMP2<-UTDEQ_model(polygon2process = AIM2020.WGS.json.simpkeep,
#                           points2process = AIM2020.WGS.json.points)
# proc.time() - ptm


## Let's make comparisons // get a df with results from 
## merging the results from NOT applying st_makevalid to
## keeping points 0.5 and NOT simplifying
df.corrs<-merge(AIMtestELEV55, AIMtestELEVNOSIMP2, by ="SITECODE")



#### Debugging

checkjsonerrors<-geojson_sf(AIM2020.WGS.json050.simp)
checkjsonerrors_valid<-st_make_valid(checkjsonerrors)

checkjsonerrors01<-subset(checkjsonerrors, SiteCode == "CN-RV-13082")
checkjsonerrors02<-st_make_valid(subset(checkjsonerrors, SiteCode == "CN-RV-13082"))





AREMP_model<-function(polygon2process, points2process){
  inputpolys<-polygon2process
  inputpoints<-points2process
  REACHID<-as.data.frame(geojson_sf(inputpolys)$reachid)
  REACHIDP<-as.data.frame(geojson_sf(inputpoints)$reachid)
  names(REACHID)<-"REACHID"
  names(REACHIDP)<-"REACHIDP"
  ELVmean_WSS<-ELVmean_WS(inputpolys)
  ELVmin_WSS<-ELVmin_WS(inputpolys)
  ELVmax_WSS<-ELVmax_WS(inputpolys)
  WSA_SQKMS<-WSA_SQKM(inputpolys)
  KFACTS<-KFACT(inputpolys)
  PMIN_WSS<-PMIN_WS(inputpolys)
  RH_WSS<-RH_WS(inputpolys)
  TMAX_WSS<-TMAX_WS(inputpolys)
  TMEAN_WSS<-TMEAN_WS(inputpolys)
  TMEAN_PTT<-TMEAN_PT(inputpoints)
  PMIN_PTT<-PMIN_PT(inputpoints)
  dfpolys<-cbind(REACHID,WSA_SQKMS,ELVmean_WSS,ELVmin_WSS,ELVmax_WSS,KFACTS,PMIN_WSS,RH_WSS,TMAX_WSS,TMEAN_WSS)
  #dfpolys<-cbind(REACHID,KFACTS,PMIN_WSS,RH_WSS,TMAX_WSS,TMEAN_WSS)
  dfpoints<-cbind(REACHIDP,TMEAN_PTT,PMIN_PTT)
  df2render<-merge(dfpolys, dfpoints, by.x="REACHID",by.y="REACHIDP")
  return(df2render)
}


ptm <- proc.time()
AREMtest<-AREMP_model(polygon2process = AREMP2020.WGS.json.simp, points2process = AREMP2020.WGS.json.points)
proc.time() - ptm

pechereco<-ELVmean_WS(putin)

geojson_write()