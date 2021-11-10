pred_fns=ifelse(exists("pred_fns"),pred_fns, list())
####################

#   Hydrology      #

####################

pred_fns$Wb_mx_area<-function(polygon2process,predictor_geometry, ...){
  polygon2process<-sf::st_transform(polygon2process, 5070)
    bodies<-sf::st_intersection(predictor_geometry, validgeometry2)
  bodies$AreaSqKm<-units::drop_units(sf::st_area(bodies)/1000000)
  media<-ifelse(is.infinite(max(bodies$AreaSqKm)),0,max(bodies$AreaSqKm))
  return(media[1,1])
}


pred_fns$GW_P_Sp_Mx<-function(polygon2process,predictor_geometry, ...){
   media<-exactextractr::exact_extract(predictor_geometry,polygon2process,'max')
  return(media[1,1])
}
