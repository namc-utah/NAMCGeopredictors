####################

#   Hydrology      #

####################

#' Area of the largest waterbody in the watershed
#'
#' @param polygon2process
#' @param predictor_geometry
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
Wb_mx_area<-function(polygon2process,predictor_geometry, ...){
  polygon2process<-sf::st_transform(polygon2process, 5070)
    bodies<-sf::st_intersection(predictor_geometry, polygon2process)
  bodies$AreaSqKm<-units::drop_units(sf::st_area(bodies)/1000000)
  media<-ifelse(is.infinite(max(bodies$AreaSqKm)),0,max(bodies$AreaSqKm))
  return(media)
}


#' Ground Water Index across the watershed
#'
#' @param polygon2process
#' @param predictor_geometry
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
GW_P_Sp_Mx<-function(polygon2process,predictor_geometry, ...){
   media<-exactextractr::exact_extract(predictor_geometry,polygon2process,'max')
  return(media)
}



#' Drainage density of perennial streams in NHD Plus (km per 25 km radius buffer)
#' @param point2process
#' @param geometry_input_path
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
length_46006<-function(point2process,geometry_input_path,...){
  AOItrans<-sf::st_transform(point2process, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>%
    sf::st_geometry() %>% # convert to sfc
    sf::st_buffer(25000) %>% # buffer 25 km radius
    sf::st_as_text() # convert to well known text
  NHD.vec<-sf::st_zm(sf::st_read(geometry_input_path, wkt_filter = AOItrans_wkt))
  buffer=sf::st_buffer(AOItrans,25000)
  clipped=sf::st_intersection(NHD.vec,buffer)
  NHD.vec_46006<-subset(clipped,FCODE==46006)
  length=sf::st_length(NHD.vec_46006)
  media=units::drop_units(sum(length)/1000)
  return(media)
}



#' Drainage density of streams in NHD Plus (km per 25 km radius buffer)
#' @param point2process
#' @param geometry_input_path
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
sum_lengthKM<-function(point2process,geometry_input_path,...){
  AOItrans<-sf::st_transform(point2process, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>%
    sf::st_geometry() %>% # convert to sfc
    sf::st_buffer(25000) %>% # buffer 25 km radius
    sf::st_as_text() # convert to well known text
  NHD.vec<-sf::st_zm(sf::st_read(geometry_input_path, wkt_filter = AOItrans_wkt))
  buffer=sf::st_buffer(AOItrans,25000)
  clipped=sf::st_intersection(NHD.vec,buffer)
  length=sf::st_length(clipped)
  media=units::drop_units(sum(length)/1000)
  return(media)
}



#' Drainage density of intermittent streams in NHD Plus (km per 25 km radius buffer)
#' @param point2process
#' @param geometry_input_path
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
length_46003<-function(point2process,geometry_input_path,...){
  AOItrans<-sf::st_transform(point2process, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>%
    sf::st_geometry() %>% # convert to sfc
    sf::st_buffer(25000) %>% # buffer 25 km radius
    sf::st_as_text() # convert to well known text
  NHD.vec<-sf::st_zm(sf::st_read(geometry_input_path, wkt_filter = AOItrans_wkt))
  buffer=sf::st_buffer(AOItrans,25000)
  clipped=sf::st_intersection(NHD.vec,buffer)
  NHD.vec_46003<-subset(clipped,FCODE==46003)
  length=sf::st_length(NHD.vec_46003)
  media=units::drop_units(sum(length)/1000)
  return(media)
}

#' Percent of intermittent streams in NHD Plus in 25 km radius buffer
#' @param point2process
#' @param geometry_input_path
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
pct_46003<-function(point2process,geometry_input_path,...){
  AOItrans<-sf::st_transform(point2process, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>%
    sf::st_geometry() %>% # convert to sfc
    sf::st_buffer(25000) %>% # buffer 25 km radius
    sf::st_as_text() # convert to well known text
  NHD.vec<-sf::st_zm(sf::st_read(geometry_input_path, wkt_filter = AOItrans_wkt))
  buffer=sf::st_buffer(AOItrans,25000)
  clipped=sf::st_intersection(NHD.vec,buffer)
  NHD.vec_46003<-subset(clipped,FCODE==46003)
  NHD.vec_total<-subset(clipped,FCODE %in% c(46003,46006))
  length_46003=sf::st_length(NHD.vec_46003)
  length_total=sf::st_length(NHD.vec_total)
  media=units::drop_units((sum(length_46003)/sum(length_total))*100)
  return(media)
}


#' Drainage density of intermittent streams in NHD Plus (km per 25 km radius buffer)
#' @param point2process
#' @param geometry_input_path
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
A1_3<-function(point2process,geometry_input_path,...){
  AOItrans<-sf::st_transform(point2process, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>%
    sf::st_geometry() %>% # convert to sfc
    sf::st_buffer(25000) %>% # buffer 25 km radius
    sf::st_as_text() # convert to well known text
  NHD.vec<-sf::st_zm(sf::st_read(geometry_input_path, wkt_filter = AOItrans_wkt))
  buffer=sf::st_buffer(AOItrans,25000)
  clipped=sf::st_intersection(NHD.vec,buffer)
  NHD.vec_A1_3<-subset(clipped,predict3cl=='A1')
  length=sf::st_length(NHD.vec_A1_3)
  media=units::drop_units(sum(length)/1000)
  return(media)
}
#' Drainage density of intermittent streams in NHD Plus (km per 25 km radius buffer)
#' @param point2process
#' @param geometry_input_path
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
A2_5<-function(point2process,geometry_input_path,...){
  AOItrans<-sf::st_transform(point2process, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>%
    sf::st_geometry() %>% # convert to sfc
    sf::st_buffer(25000) %>% # buffer 25 km radius
    sf::st_as_text() # convert to well known text
  NHD.vec<-sf::st_zm(sf::st_read(geometry_input_path, wkt_filter = AOItrans_wkt))
  buffer=sf::st_buffer(AOItrans,25000)
  clipped=sf::st_intersection(NHD.vec,buffer)
  NHD.vec_A2_5<-subset(clipped,predict5cl=='A2')
  length=sf::st_length(NHD.vec_A2_5)
  media=units::drop_units(sum(length)/1000)
  return(media)
}

#' Drainage density of intermittent streams in NHD Plus (km per 25 km radius buffer)
#' @param SQLite_file_path
#' @param COMIDs (should only be one and NHD plus v2)
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
drainage_density<-function(SQLite_file_path,COMIDs,...){
  nldi_feature <- list(featureSource = "comid",
                       featureID = as.integer(COMIDs))
  flowline_nldi <- nhdplusTools::navigate_nldi(nldi_feature,
                                 mode = "upstreamTributaries",
                                 distance_km = 1000)
  AOI<-sf::st_geometry(flowline_nldi$UT_flowlines)
  AOItrans<-sf::st_transform(AOI,crs = 5070)
  length=sf::st_length(AOItrans)
  lengthkm=units::drop_units(sum(length)/1000)
  WsAreaSqKm =StreamCat_single_pred(SQLite_file_path,"WsAreaSqKm",COMIDs)
   media=lengthkm/WsAreaSqKm
   return(media[1,1])
}


#' Perennial drainage density of intermittent streams in NHD Plus (km per 25 km radius buffer)
#' @param SQLite_file_path
#' @param geometry_input_path
#' @param geometry_input_name
#' @param COMIDs (should only be one and NHD plus v2)
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
p_drainage_density<-function(SQLite_file_path,geometry_input_path,geometry_input_name,COMIDs,...){
  nldi_feature <- list(featureSource = "comid",
                       featureID = as.integer(COMIDs))
  flowline_nldi <- nhdplusTools::navigate_nldi(nldi_feature,
                                               mode = "upstreamTributaries",
                                               distance_km = 1000)
  COMIDS<-flowline_nldi$UT_flowlines$nhdplus_comid
  AOI=sf::st_make_valid(sf::st_read(geometry_input_path, query=sprintf('SELECT * FROM %s WHERE COMID in(%s)',geometry_input_name, inLOOP(substr(COMIDS, 1, 10)))))
  AOItrans<-sf::st_transform(AOI,crs = 5070)
  AOItransperennial=subset(AOItrans,FCODE==46006)
  length=sf::st_length(AOItransperennial)
  lengthkm=units::drop_units(sum(length)/1000)
  WsAreaSqKm =StreamCat_single_pred(SQLite_file_path,"WsAreaSqKm",COMIDs)
  media=lengthkm/WsAreaSqKm
  return(media[1,1])
}

