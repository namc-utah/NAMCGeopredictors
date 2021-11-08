####################

#   Hydrology      #

####################

Wb_mx_area<-function(polygon2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(polygon2process)
  validgeometry<-st_transform(validgeometry, 5070)
  validgeometry2<-st_make_valid(validgeometry)
  bodies<-st_intersection(predictor_geometry, validgeometry2)
  bodies$AreaSqKm<-drop_units(st_area(bodies)/1000000)
  media<-ifelse(is.infinite(max(bodies$AreaSqKm)),0,max(bodies$AreaSqKm)) 
  return(media)
}


GW_P_Sp_Mx<-function(polygon2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(predictor_geometry,validgeometry,'max')
  return(media)
}
