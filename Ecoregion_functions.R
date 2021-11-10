####################

#   Ecoregion      #

####################


#' Assess if the point falls in an eastern Oregon ecoregion
#' database used -- GIS_Data/Metrics/Oregon/Data/OR_EastWest_Eco
#' The function first makes sure that the ecoregion is in the same CRS as the points
#' Then in intersects the point with the ecoregions layer and creates a column named "east"
#' this new column is filled with 0's. The functions then assesses if the points have the
#' attribute "East" in the "EastWest" column. If they do, then assigns a value of 1, else 0
#' @param points2process 
#'
#' @return a single value 1 or 0 - 1 if the point is the eastern Oregon ecoregion, else 0
#' @export
#'
#' @examples
pred_fns$east<-function(points2process,predictor_geometry, ...){
   EcoregionWGS<-sf::st_transform(predictor_geometry, crs = 4326)# transforming the input vector to the CRS of the geojson points
  temp01<-sf::st_intersection(points2process,EcoregionWGS)
  temp01$east<-0
  temp01$east[temp01$EastWest=="East"]<-1
  media<-temp01$east
  return(media[1,1])
}

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
pred_fns$ECO3<-function(points2process,predictor_geometry, ...){
   myvars <- "US_L3CODE"
  Eco3_PT.vec <- Eco3_PT.vec[myvars]
  Eco3_PT.vec.WGS<-sf::st_transform(predictor_geometry, crs = 4326)
  media<-sf::st_intersection(points2process, Eco3_PT.vec.WGS)%>% dplyr::pull(US_L3CODE)
  return(media[1,1])
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
pred_fns$ECO4<-function(points2process,predictor_geometry, ...){
   myvars <- "US_L4CODE"
  Eco4_PT.vec <- Eco4_PT.vec[myvars]
  Eco4_PT.vec.WGS<-sf::st_transform(predictor_geometry, crs = 4326)
  media<-sf::st_intersection(points2process, Eco4_PT.vec.WGS)%>% dplyr::pull(US_L4CODE)
  return(media[1,1])
}


#' Is the level 3 ecoregion number 23?  (Y or N)
#' databased used GIS_Stats/Ecoregion/Data/Eco_Level_III_US.shp
#' The point is first transformed to a CRS in meters
#' The functions then makes sure that only one column "US_L3CODE" is present in the attribute table
#' by creating a subset myvars <- "US_L3CODE" / Eco3_PT.vec[myvars]
#' 
#' Then it intersects the point with the Eco_Level_III_US.shp layer and just pulls the value for the 
#' "US_L3CODE" attribute. A new column "ER13" is created whereby if the intersected value is 23 then it 
#' will be populated with "Y", else it will be populated with "N"
#' @param points2process 
#'
#' @return a single value "Y" if the ecoregion of the point is 23, "N" otherwise
#' @export
#'
#' @examples
pred_fns$ER13<-function(points2process,predictor_geometry, ...){
   points2process<-st_transform(points2process, 5070)
  myvars <- "US_L3CODE"
  Eco3_PT.vec <- predictor_geometry[myvars]
  points2process$Eco3_PT01<-sf::st_intersection(points2process, Eco3_PT.vec)%>% dplyr::pull(US_L3CODE)
  points2process$ER13<- points2process %>%
    dplyr::mutate(ER13 = case_when(
      Eco3_PT01 == 23 ~ "Y",
      Eco3_PT01 != 23 ~ "N"))%>% dplyr::pull(ER13)
  media<-points2process$ER13
  return(media[1,1])
}


pred_fns$HV_UPPERPLATTE<-function(points2process,predictor_geometry, ...){
   points2process<-sf::st_transform(points2process, 5070)
  biovar<-"LAST_COUNT"
  WYBio<-predictor_geometry[biovar]
  tempinter<-sf::st_intersection(points2process, WYBio)
  tempinter$HV_UPPERPLATTE<-0
  tempinter$HV_UPPERPLATTE[tempinter$LAST_COUNT == "HIGH VALLEYS"]<-1
  media<-tempinter$HV_UPPERPLATTE
  return(media[1,1])
}


pred_fns$MRE<-function(points2process,predictor_geometry, ...){
   points2process<-sf::st_transform(points2process, 5070)
  biovar<-"LAST_COUNT"
  WYBio<-predictor_geometry[biovar]
  tempinter<-sf::st_intersection(points2process, WYBio)
  tempinter$MRE<-0
  tempinter$MRE[tempinter$LAST_COUNT == "BLACK HILLS"]<-1
  media<-tempinter$MRE
  return(media[1,1])
}

pred_fns$SFLR<-function(points2process,predictor_geometry, ...){
   points2process<-sf::st_transform(points2process, 5070)
  biovar<-"LAST_COUNT"
  WYBio<-predictor_geometry[biovar]
  tempinter<-sf::st_intersection(points2process, WYBio)
  tempinter$SFLR<-0
  tempinter$SFLR[tempinter$LAST_COUNT == "S WY FH & LARAMIE RANGE"]<-1
  media<-tempinter$SFLR
  return(media[1,1])
}

pred_fns$SR_BIGHORNS<-function(points2process,predictor_geometry, ...){
    points2process<-sf::st_transform(points2process, 5070)
  biovar<-"LAST_COUNT"
  WYBio<-predictor_geometry[biovar]
  tempinter<-sf::st_intersection(points2process, WYBio)
  tempinter$SR_BIGHORNS<-0
  tempinter$SR_BIGHORNS[tempinter$LAST_COUNT == "SOUTHERN ROCKIES"|
                          tempinter$LAST_COUNT == "BIGHORN BASIN FOOTHILLS"|
                          tempinter$LAST_COUNT == "WB - BIGHORN BASIN"]<-1
  media<-tempinter$SR_BIGHORNS
  return(media[1,1])
}
