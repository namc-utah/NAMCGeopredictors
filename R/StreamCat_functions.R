#' concatenate list objects into an "IN" string for insertion into SQLite queries (needed for StreamCat)
#'
#' @param inSTR
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
inLOOP<- function(inSTR,...) {
  inSTR=unlist(inSTR)
  if (inSTR[1]==''){loopSTR="''"} else{
    for (i in 1:length(inSTR)){
      comma=ifelse(i==length(inSTR),'',',')
      STRl=sprintf("'%s'%s",inSTR[i],comma)
      if(i==1){loopSTR=STRl} else{loopSTR=paste(loopSTR,STRl)}
    } }
  return(loopSTR)
}

##### Stream Cat ####
#' Get single StreamCat predictor from SQLite database on S3 to put into database
#'
#' @param SQLite_file_path
#' @param predictor_name predictor abbreviation as it is in the database
#' @param COMIDs
#'
#' @return
#' @export
#'
#' @examples
StreamCat_single_pred <-function(SQLite_file_path, predictor_name,COMIDs=def_sites$COMID,...) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), SQLite_file_path)
  if (predictor_name == "Precip8110") {
    media = DBI::dbGetQuery(conn,sprintf("SELECT Precip8110Ws as Precip8110 FROM StreamCat_2016 WHERE COMID in (%s)",paste0(predictor_name),inLOOP(substr(COMIDs, 1, 10))))
  }
  else{
    media = DBI::dbGetQuery(conn,sprintf("SELECT %s FROM StreamCat_2016 WHERE COMID in (%s)",paste0(predictor_name),inLOOP(substr(COMIDs, 1, 10))))
  }
  return(media)
}

#' Average mean annual stream temp across 2018-2014
#'
#' @param SQLite_file_path
#' @param COMIDs
#'
#' @return
#' @export
#'
#' @examples
MAST_mean08091314=function(SQLite_file_path,COMIDs=def_sites$COMID,...) {
  rawtemps = DBI::dbGetQuery(conn,sprintf("SELECT MAST_2008,MAST_2009,MAST_2013,MAST_2014 FROM StreamCat_2016 WHERE COMID in (%s)",inLOOP(substr(COMIDs, 1, 10))))
  MAST_mean08091314=rowMeans(rawtemps,na.rm=FALSE)
  return(MAST_mean08091314)
}

#' Average mean summer stream temp across 2018-2014
#'
#' @param SQLite_file_path
#' @param COMIDs
#'
#' @return
#' @export
#'
#' @examples
MSST_mean08091314=function(SQLite_file_path,COMIDs=def_sites$COMID,...) {
  rawtemps = DBI::dbGetQuery(conn,sprintf("SELECT MSST_2008,MSST_2009,MSST_2013,MSST_2014 FROM StreamCat_2016 WHERE COMID in (%s)",inLOOP(substr(COMIDs, 1, 10))))
  MSST_mean08091314=rowMeans(rawtemps,na.rm=FALSE)
  return(MSST_mean08091314)
}

#' Average mean winter stream temp across 2018-2014
#'
#' @param SQLite_file_path
#' @param COMIDs
#'
#' @return
#' @export
#'
#' @examples
MWST_mean08091314=function(SQLite_file_path,COMMIDs=def_sites$COMID,...) {
  rawtemps = DBI::dbGetQuery(conn,sprintf("SELECT MWST_2008,MWST_2009,MWST_2013,MWST_2014 FROM StreamCat_2016 WHERE COMID in (%s)",inLOOP(substr(COMIDs, 1, 10))))
  MWST_mean08091314=rowMeans(rawtemps,na.rm=FALSE)
  return(MWST_mean08091314)
}

#' Get all StreamCat predictors from SQLite database on S3 plus avg temps
#'
#' @param SQLite_file_path
#' @param COMIDs
#'
#' @return
#' @export
#'
#' @examples
StreamCat_all<- function(SQLite_file_path,COMIDs=def_sites$COMID,...){
  conn<-DBI::dbConnect(RSQLite::SQLite(),SQLite_file_path)
  media=DBI::dbGetQuery(conn,sprintf("SELECT * FROM StreamCat_2016 WHERE COMID in (%s)",inLOOP(substr(COMIDs,1,10))))
  MAST_mean08091314=MAST_mean08091314(SQLite_file_path, COMIDs)
  MSST_mean08091314=MSST_mean08091314(SQLite_file_path, COMIDs)
  MWST_mean08091314=MWST_mean08091314(SQLite_file_path, COMIDs)
  media=cbind(media,MAST_mean08091314,MSST_mean08091314,MWST_mean08091314)
  return(media)
}



#Get COMIDs
#' Get COMID using nhdpulsTools
#'
#' @param boxId
#'
#' @return
#' @export
#'
#' @examples
getCOMIDs=function(boxId){
  def_sites = NAMCr::query(
    api_endpoint = "samples",
    include = c("sampleId","siteId", "siteName", "usState", "siteLocation"),
    boxId = boxId
  )

  point2process=geojsonsf::geojson_sf(def_sites$siteLocation)

  library(nhdplusTools)
  for (p in 1:nrow(point2process)){
    start_comid <- nhdplusTools::discover_nhdplus_id(point2process[p,1])
    if(p == 1){
      comids= start_comid
    }else{
      comids = rbind(comids, start_comid)
    }
  }
  def_sites$COMID=comids
return(def_sites$COMID)
  ##add saving comIDs endpoint here
  }
#
# #Alternative joins to layers on disk, MUCH slower but could be needed if want to actually get catchment polygons
#    ## Code to link points to hydrologic region
#
#   # Read in polygon of NHDPlus vector processing units (hydrologic regions)
#   # Drop points through polygons to match points with hydrologic region
#   # Loop through regions and extract COMIDs from shapefiles (takes a while)
#
#   #Read in polygons
#   vpus = sf::st_read(file.path(nhd_dir,'NHDPlusGlobalData/VPUs.shp'))
#   vpus=sf::st_transform(vpus,st_crs(point2process))
#   #Extract data from polygon based on point locations
#   pts2 = sf::st_join(point2process,vpus)
#   #Add column of region and vpu IDs
#   point2process$region_vpu = paste0(pts2$DrainageID, '_', pts2$UnitID)
#   #Find unique combinations of region and vpus IDs
#   regions = unique(point2process$region_vpu)
#   #Loop through these combinations to read in the correct region
#   #Also selects just points for this region
#
#   # This typically takes ~5-10 minutes to run...
#   start.time.0 = Sys.time()
#   for(i in 1:length(regions)){
#     print(regions[i])
#     pts_tmp = point2process[point2process$region_vpu == regions[i],]
#     #Split up unique region/vpu ID to use in path
#     reg = strsplit(regions[i], '_')[[1]][[1]]
#     vpu = strsplit(regions[i], '_')[[1]][[2]]
#     #Define path
#     cat_dir = paste0(nhd_dir,'/NHDPlus',reg,'/NHDPlus',vpu,'/NHDPlusCatchment')
#     catchment=sf::st_read(paste0(cat_dir, '/Catchment.shp'))
#     catchment=st_transform(catchment,st_crs(point2process))
#     #Extract COMID from catchment shapefile
#     pointscatchments=sf::st_join(
#       pts_tmp,catchment)
#     pts_tmp$COMID =pointscatchments$FEATUREID
#       #Combine tables into final output association table
#     if(i == 1){
#       out_df = pts_tmp
#     }else{
#       out_df = rbind(out_df, pts_tmp)
#     }
#   }
#   end.time.0 = Sys.time()
#   time.taken.0 = end.time.0 - start.time.0
#   time.taken.0
#
#   str(out_df)
#   write.csv(out_df, paste0(wd1, '/output/pts_comid.csv'), row.names=F)
#
# }

