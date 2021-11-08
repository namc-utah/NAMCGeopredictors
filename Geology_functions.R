###################

#   Geology       #

####################

pred_fns$Pct_Alfi<-function(polygon2process,predictor_geometry, ...){
  polygon2process$AREAHA<-drop_units(st_area(polygon2process)/10000)
  polygon2process$Pct_Alfi_01<-exact_extract(predictor_geometry,polygon2process,'sum')
  polygon2process$Pct_Alfi<-(polygon2process$Pct_Alfi_01*25/polygon2process$AREAHA)*100
  media<-polygon2process$Pct_Alfi
  return(media)
}

pred_fns$PCT_SEDIM<-function(polygon2process,predictor_geometry, ...){
    crs2use<-crs(predictor_geometry)
    polygon2process<-st_transform(polygon2process,crs=crs2use)
  PCT_SEDIM.vec<-st_make_valid(predictor_geometry)
  geo01<-st_intersection(PCT_SEDIM.vec, polygon2process)
  geo02<-st_cast(geo01, "POLYGON")
  geo03<-geo02 %>%
    mutate(AREA_SQKM = drop_units(st_area(geo02)/1000000))%>%# update the AREA for subsequent calculations
    mutate(PORC = ifelse(GEOnum == 5, round(AREA_SQKM/sum(AREA_SQKM)*100,2),0))
  geo03<-geo03%>%
    filter(GEOLOGY=='Sedimentary')
  media<-sum(geo03$PORC)
  return(media)
}
