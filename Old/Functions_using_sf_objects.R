#' Eco Region level 3 (number) of the point
#' database used GIS_Stats/Ecoregion/Data/Eco_Level_III_US.shp
#' The functions first makes sure that only one column "US_L3CODE" is present in the attribute table
#' by creating a subset myvars <- "US_L3CODE" / Eco3_PT.vec <- Eco3_PT.vec[myvars]
#' Then it transforms the Eco_Level_III_US.shp so that it shares the same CRS with the points
#' it then intersects the point with the Eco_Level_III_US.shp layer and just pulls the value for the 
#' "US_L3CODE" attribute
#' @param points2process 
#'
#' @return a single value: the ecoregion level 3 for the point
#' @export
#'
#' @examples
ECO3<-function(points2process){
  validgeometry<-(points2process)
  validgeometry<-st_transform(validgeometry,crs = 4326)
  myvars <- "US_L3CODE"
  Eco3_PT.vec <- Eco3_PT.vec[myvars]
  Eco3_PT.vec.WGS<-st_transform(Eco3_PT.vec, crs = 4326)
  media<-st_intersection(validgeometry, Eco3_PT.vec.WGS)%>%pull(US_L3CODE)
  return(media)
}


#' Eco Region level 4 of the point
#' database used GIS_Stats/Ecoregion/Data/us_eco_l4_no_st.shp
#' The functions first makes sure that only one column "US_L4CODE" is present in the attribute table
#' by creating a subset myvars <- "US_L4CODE" / Eco4_PT.vec[myvars]
#' Then it transforms the us_eco_l4_no_st.shp so that it shares the same CRS with the points
#' it then intersects the point with the Eco_Level_III_US.shp layer and just pulls the value for the 
#' "US_L4CODE" attribute
#' @param points2process 
#'
#' @return a single value: the ecoregion level 4 value for the point
#' @export
#'
#' @examples
ECO4<-function(points2process){
  validgeometry<-(points2process)
  validgeometry<-st_transform(validgeometry,crs = 4326)
  myvars <- "US_L4CODE"
  Eco4_PT.vec <- Eco4_PT.vec[myvars]
  Eco4_PT.vec.WGS<-st_transform(Eco4_PT.vec, crs = 4326)
  media<-st_intersection(validgeometry, Eco4_PT.vec.WGS)%>%pull(US_L4CODE)
  return(media)
}