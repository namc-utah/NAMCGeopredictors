pred_fns=ifelse(exists("pred_fns"),pred_fns, list())

#' concatenate list objects into an "IN" string for insertion into SQLite queries (needed for StreamCat)
#'
#' @param inSTR
#'
#' @return
#' @export
#'
#' @examples
pred_fns$inLOOP<- function(inSTR,...) {
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
pred_fns$StreamCat_single_pred <-function(SQLite_file_path, predictor_name, COMIDs) {
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
pred_fns$MAST_mean08091314=function(SQLite_file_path, COMIDs) {
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
pred_fns$MSST_mean08091314=function(SQLite_file_path, COMIDs) {
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
pred_fns$MWST_mean08091314=function(SQLite_file_path, COMIDs) {
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
pred_fns$StreamCat_all<- function(SQLite_file_path,COMIDs){
  conn<-DBI::dbConnect(RSQLite::SQLite(),SQLite_file_path)
  media=DBI::dbGetQuery(conn,sprintf("SELECT * FROM StreamCat_2016 WHERE COMID in (%s)",inLOOP(substr(COMIDs,1,10))))
  MAST_mean08091314=MAST_mean08091314(SQLite_file_path, COMIDs)
  MSST_mean08091314=MSST_mean08091314(SQLite_file_path, COMIDs)
  MWST_mean08091314=MWST_mean08091314(SQLite_file_path, COMIDs)
  media=cbind(media,MAST_mean08091314,MSST_mean08091314,MWST_mean08091314)
  return(media)
}
