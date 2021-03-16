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
  WSA_SQKMS<-WSA_SQKM(inputpolys)
  TMAX_WSS<-TMAX_WSS(inputpolys)
  TMEAN_UT_WSS<-TMEAN_UT_WS(inputpolys)
  TMIN_UT_WSS<-TMIN_UT_WS(inputpolys)
  RH_WSS<-RH_WS(inputpolys)
  MEANP_WSS<-MEANP_WS(inputpolys)
  MAXP_WSS<-MAXP_WS(inputpolys)
  MAXWD_WSS<-MAXWD_WS(inputpolys)
  FST32F_WSS<-FST32F_WS(inputpolys)
  EVI_MAX_WSS<-EVI_MAX_WS(inputpolys)
  TMEAN_WSS<-TMEAN_WS(inputpolys)
  TMAX_PTT<-TMAX_PT(inputpoints)
  TMEAN_PTT<-TMEAN_UT_PT(inputpoints)
  DD_LAT_PTT<-DD_LAT_PT(inputpoints) 
  dfpolys<-cbind(REACHID,WSA_SQKMS,ELVmean_WSS,RH_WSS,TMAX_WSS,TMEAN_WSS,TMIN_UT_WSS,MEANP_WSS,
                 MAXP_WSS,MAXWD_WSS,FST32F_WSS,EVI_MAX_WSS)
  dfpoints<-cbind(REACHIDP,TMAX_PTT,TMEAN_PTT,DD_LAT_PTT)
  df2render<-merge(dfpolys, dfpoints, by.x="SITECODE",by.y="SITECODEP")
  return(df2render)
}


ptm <- proc.time()
AIMtest<-UTDEQ_model(polygon2process = AIM2020.WGS.json.simp,
                      points2process = AIM2020.WGS.json.points)
proc.time() - ptm












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