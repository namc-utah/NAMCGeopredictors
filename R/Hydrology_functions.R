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
length_A1_3<-function(point2process,geometry_input_path,...){
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
