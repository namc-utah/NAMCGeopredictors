###################

#   Geology       #

####################

Pct_Alfi<-function(polygon2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(polygon2process)
  validgeometry$AREAHA<-drop_units(st_area(validgeometry)/10000)
  validgeometry$Pct_Alfi_01<-exact_extract(predictor_geometry,validgeometry,'sum')
  validgeometry$Pct_Alfi<-(validgeometry$Pct_Alfi_01*25/validgeometry$AREAHA)*100
  media<-validgeometry$Pct_Alfi
  return(media)
}

PCT_SEDIM<-function(polygon2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(validgeometry)
  crs2use<-crs(predictor_geometry)
  validgeometry<-st_transform(validgeometry,crs=crs2use)
  PCT_SEDIM.vec<-st_make_valid(predictor_geometry)
  geo01<-st_intersection(PCT_SEDIM.vec, validgeometry)
  geo02<-st_cast(geo01, "POLYGON")
  geo03<-geo02 %>%
    mutate(AREA_SQKM = drop_units(st_area(geo02)/1000000))%>%# update the AREA for subsequent calculations
    mutate(PORC = ifelse(GEOnum == 5, round(AREA_SQKM/sum(AREA_SQKM)*100,2),0))
  geo03<-geo03%>%
    filter(GEOLOGY=='Sedimentary')
  media<-sum(geo03$PORC)
  return(media)
}
