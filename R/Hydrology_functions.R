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

#' Drainage density of all streams in the watershed in NHD Plus
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

#' distance downstream along the mainstem in NHD Plus
#' @param SQLite_file_path
#' @param COMIDs (should only be one and NHD plus v2)
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
distance_mainstem<-function(SQLite_file_path,COMIDs,...){
  nldi_feature <- list(featureSource = "comid",
                       featureID = as.integer(COMIDs))
  flowline_nldi <- nhdplusTools::navigate_nldi(nldi_feature,
                                               mode = "upstreamMain",
                                               distance_km = 1000)
  AOI<-sf::st_geometry(flowline_nldi$UT_flowlines)
  AOItrans<-sf::st_transform(AOI,crs = 5070)
  length=sf::st_length(AOItrans)
  lengthkm=units::drop_units(sum(length)/1000)
    media=lengthkm
  return(media[1,1])
}

#' Drainage density of only perennial streams in the watershed in NHD Plus
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

##### discharge ####
#' Get predictor from SQLite McManamay2019 database on S3
#'
#' @param SQLite_McManamay_file_path
#' @param predictor_name predictor abbreviation as it is in the database
#' @param COMIDs
#'
#' @return
#' @export
#'
#' @examples
McManamay2019 <-function(SQLite_McManamay_file_path, predictor_name,COMIDs,...) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), SQLite_McManamay_file_path)
  media = DBI::dbGetQuery(conn,sprintf("SELECT %s FROM discharge WHERE COMID in (%s)",paste0(predictor_name),inLOOP(substr(COMIDs, 1, 10))))
  return(media)
}

#' StreamPower
#'
#' @param SQLite_discharge_file_path
#' @param predictor_name predictor abbreviation as it is in the database
#' @param COMIDs
#' @param modelId
#'
#' @return
#' @export
#'
#' @examples
StreamPower<-function(SQLite_McManamay_file_path, predictor_name,COMIDs,modelId...) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), SQLite_McManamay_file_path)
  Flow_cfs = DBI::dbGetQuery(conn,sprintf("SELECT %s FROM discharge WHERE COMID in (%s)",paste0(predictor_name),inLOOP(substr(COMIDs, 1, 10))))
  def_predictors = NAMCr::query(
    api_endpoint = "samplePredictorValues",
    sampleIds = def_samples$sampleId,
    modelIds=modelId
  )
  def_predictorssub=subset(def_predictors,predictorId=582)
  media=
  return(media)
}

#' Buffer250WetlandAreaKm
#'
#' @param point2process
#' @param geometry_input_path
#' @return
#' @export
#'
#' @examples
Buffer250WetlandAreaKm<-function(point2process,geometry_input_path,...){
  AOItrans<-sf::st_transform(point2process, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>%
    sf::st_geometry() %>% # convert to sfc
    sf::st_buffer(250) %>% # buffer 250 meters
    sf::st_as_text() # convert to well known text
  NWI<-sf::st_read(geometry_input_path, layer='NWI', wkt_filter = AOItrans_wkt)
    area=sf::st_area(AOI_Buffer_subset)
  media=units::drop_units(sum(area)/1000000)
  return(media)
  unlink(paste0(slope_bin,'/*'))
}
geometry_input_path="C:/Users/jenni/Box/NAMC WATS Department Files/GIS/GIS_Stats/CONUS/hydrology/NWI.gdb"
