####################

#       Day        #

####################

DOY <- function(JulianDate) {
  media = JulianDate
  return(media)
}

####################

#       Veg        #

####################

#' Percent of Alru landcover in the watershed
#' This functions takes the GIS_Stats/Vegetation/Data/alru_dom dataset
#' First it obtains the area in hectares --> drop_units(st_area(validgeometry)/10000) <-- for the watershed
#' Then it obtains zonal statistics - the number of pixels of alru_dom -->exact_extract(alru_dom.ras,validgeometry,'count') <--
#' Because each pixel is 30m it calculates the area of alru_dom by multiplying the count * 0.09 and then divides by the area to 
#' obtain percentage
#' @param polygon2process this is a geojson string for the watershed
#' The geojson is converted to an object of type sf -->validgeometry<-geojson_sf(polygon2process)<--
#' @return this functions returns one value which is the percentage of alru_dom
#' @export
#'
#' @examples
alru_dom<-function(polygon2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(polygon2process)
  validgeometry$AREAHA<-drop_units(st_area(validgeometry)/10000)
  validgeometry$alru_dom_01<-exact_extract(predictor_geometry,validgeometry,'count')
  media<-(validgeometry$alru_dom_01*0.09/validgeometry$AREAHA)*100
  return(media)
}


#' Percent of Evergreen landcover in the watershed
#' dataset used GIS_Stats/Vegetation/Data/evergr
#' The function first obtains the watershed area in hectares drop_units(st_area(validgeometry)/10000)
#' It then uses the exact_extract function to obtain zonal statistics for the raster, the sum of pixels within the watershed
#' The sum is obtained because all the pixels have a value of 1. Then in converts the sum of pixels to area
#' by multiplying 0.09 (30 x 30 m pixels equal 900 m2 equal 0.09 hectares). This value is then divided by the 
#' area of the watershed and the percent (in decimal units is extracted)
#' @param polygon2process 
#'
#' @return a single value of percent of evergreen landcover in the watershed in decimal values
#' @export
#'
#' @examples
Evergr_ave<-function(polygon2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(polygon2process)
  validgeometry$AREAHA<-drop_units(st_area(validgeometry)/10000)
  validgeometry$Evergr_ave_01<-exact_extract(predictor_geometry,validgeometry,'sum')
  validgeometry$Evergr_ave<-(validgeometry$Evergr_ave_01*0.09/validgeometry$AREAHA)
  media<-validgeometry$Evergr_ave
  return(media)
}



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
east<-function(points2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(points2process)
  EcoregionWGS<-st_transform(predictor_geometry, crs = 4326)# transforming the input vector to the CRS of the geojson points
  temp01<-st_intersection(validgeometry,EcoregionWGS)
  temp01$east<-0
  temp01$east[temp01$EastWest=="East"]<-1
  media<-temp01$east
  return(media)
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
ECO3<-function(points2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(points2process)
  myvars <- "US_L3CODE"
  Eco3_PT.vec <- Eco3_PT.vec[myvars]
  Eco3_PT.vec.WGS<-st_transform(predictor_geometry, crs = 4326)
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
ECO4<-function(points2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(points2process)
  myvars <- "US_L4CODE"
  Eco4_PT.vec <- Eco4_PT.vec[myvars]
  Eco4_PT.vec.WGS<-st_transform(predictor_geometry, crs = 4326)
  media<-st_intersection(validgeometry, Eco4_PT.vec.WGS)%>%pull(US_L4CODE)
  return(media)
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
ER13<-function(points2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(points2process)
  validgeometry<-st_transform(validgeometry, 5070)
  myvars <- "US_L3CODE"
  Eco3_PT.vec <- predictor_geometry[myvars]
  validgeometry$Eco3_PT01<-st_intersection(validgeometry, Eco3_PT.vec)%>%pull(US_L3CODE)
  validgeometry$ER13<- validgeometry %>%
    mutate(ER13 = case_when(
      Eco3_PT01 == 23 ~ "Y",
      Eco3_PT01 != 23 ~ "N"))%>%pull(ER13)
  media<-validgeometry$ER13
  return(media)
}


HV_UPPERPLATTE<-function(points2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(points2process)
  validgeometry<-st_transform(validgeometry, 5070)
  biovar<-"LAST_COUNT"
  WYBio<-predictor_geometry[biovar]
  tempinter<-st_intersection(validgeometry, WYBio)
  tempinter$HV_UPPERPLATTE<-0
  tempinter$HV_UPPERPLATTE[tempinter$LAST_COUNT == "HIGH VALLEYS"]<-1
  media<-tempinter$HV_UPPERPLATTE
  return(media)
}


MRE<-function(points2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(points2process)
  validgeometry<-st_transform(validgeometry, 5070)
  biovar<-"LAST_COUNT"
  WYBio<-predictor_geometry[biovar]
  tempinter<-st_intersection(validgeometry, WYBio)
  tempinter$MRE<-0
  tempinter$MRE[tempinter$LAST_COUNT == "BLACK HILLS"]<-1
  media<-tempinter$MRE
  return(media)
}

SFLR<-function(points2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(points2process)
  validgeometry<-st_transform(validgeometry, 5070)
  biovar<-"LAST_COUNT"
  WYBio<-predictor_geometry[biovar]
  tempinter<-st_intersection(validgeometry, WYBio)
  tempinter$SFLR<-0
  tempinter$SFLR[tempinter$LAST_COUNT == "S WY FH & LARAMIE RANGE"]<-1
  media<-tempinter$SFLR
  return(media)
}

SR_BIGHORNS<-function(points2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(points2process)
  validgeometry<-st_transform(validgeometry, 5070)
  biovar<-"LAST_COUNT"
  WYBio<-predictor_geometry[biovar]
  tempinter<-st_intersection(validgeometry, WYBio)
  tempinter$SR_BIGHORNS<-0
  tempinter$SR_BIGHORNS[tempinter$LAST_COUNT == "SOUTHERN ROCKIES"|
                          tempinter$LAST_COUNT == "BIGHORN BASIN FOOTHILLS"|
                          tempinter$LAST_COUNT == "WB - BIGHORN BASIN"]<-1
  media<-tempinter$SR_BIGHORNS
  return(media)
}



####################

#   Elevation      #

####################


#' Mean elevation across the watershed
#' The function requires that a Google Earth Engine GEE object be created
#' USGS_NED National Elevation Dataset. It uses rgee ee_extract function to conduct
#' zonal statistics. The resolution (pixel size to use) can be changed if desired by
#' modifying scale=. Now it is using a 90x90 m pixel size
#' @param polygon2process 
#'
#' @return a single value the mean elevation value for the watershed
#' @export
#'
#' @examples
ELVmean_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-ee_extract(USGS_NED, validgeometry, fun = ee$Reducer$mean(), scale=90)
  return(media)
}

#' Watershed mean elevation divided by 100
#' 
#' @param polygon2process 
#'
#' @return
#' @export
#'
#' @examples
ELVmean_WS_100<-function(polygon2process){
    media<-ELVmean_WS(polygon2process)/100
  return(media)
}


ELVmax_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-ee_extract(USGS_NED, validgeometry, fun = ee$Reducer$max(), scale=90)
  return(media)
}

#' Average of min elevation in the watershed
#' The function requires that a Google Earth Engine GEE object be created
#' USGS_NED National Elevation Dataset. It uses rgee ee_extract function to conduct
#' zonal statistics. The resolution (pixel size to use) can be changed if desired by
#' modifying scale=. Now it is using a 90x90 m pixel size
#' @param polygon2process 
#'
#' @return a single value the minimum elevation value for the watershed
#' @export
#'
#' @examples
ELVmin_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-ee_extract(USGS_NED, validgeometry, fun = ee$Reducer$min(), scale=90)
  return(media)
}


#' Range between max and min elevations
#' The function requires that a Google Earth Engine GEE object be created
#' USGS_NED National Elevation Dataset. It uses rgee ee_extract function to conduct
#' zonal statistics. The resolution (pixel size to use) can be changed if desired by
#' modifying scale=. Now it is using a 90x90 m pixel size.
#' The function first obtains the max elevation in the watershed, and then the minimum and finally
#' it obtains the difference between the two values
#' @param polygon2process 
#'
#' @return a single value, the elevation range in the watershed
#' @export
#'
#' @examples
ELEV_RANGE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  max<-ee_extract(USGS_NED, validgeometry, fun = ee$Reducer$max(), scale=90)
  min<-ee_extract(USGS_NED, validgeometry, fun = ee$Reducer$min(), scale=90)
  media<-max-min
  return(media)
}

#' Elevation of the point
#' The function requires that a Google Earth Engine GEE object be created
#' USGS_NED National Elevation Dataset. It uses rgee ee_extract function to conduct
#' zonal statistics. The resolution (pixel size to use) can be changed if desired by
#' modifying scale=. Now it is using a 90x90 m pixel size.
#' The rgee function ee_extract is used here without fun = ee$Reducer$min,mean,max()
#' argument since it only needs the information at the point
#' @param points2process 
#'
#' @return a single value which is the elevation at the point
#' @export
#'
#' @examples
ELEV_SITE<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-ee_extract(USGS_NED, validgeometry, scale=90)/10 
  return(media)
}

#' Square root of elevation at the point
#' The function requires that a Google Earth Engine GEE object be created
#' USGS_NED National Elevation Dataset. It uses rgee ee_extract function to conduct
#' zonal statistics. The resolution (pixel size to use) can be changed if desired by
#' modifying scale=. Now it is using a 90x90 m pixel size.
#' the function first extracts the elevation at the point. This elevation is then divided by
#' 10 and then the square root is extracted
#' @param points2process 
#'
#' @return a single value which is the square root of the elevation at the point
#' @export
#'
#' @examples
ELEV_SITE_SQRT<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  elevation<-ee_extract(USGS_NED, validgeometry, scale=90)
  media<-sqrt((elevation/10))
  return(media)
}


#' Elevation coefficient of variation at the point
#' The function requires that a Google Earth Engine GEE object be created
#' USGS_NED National Elevation Dataset. It uses rgee ee_extract function to conduct
#' zonal statistics. The resolution (pixel size to use) can be changed if desired by
#' modifying scale=. Now it is using a 90x90 m pixel size.
#' The function first makes sure that the point has a CRS information in meters
#' because the st_buffer function does not work well with decimal degrees
#' it then creates a area of influence AOI around the point of 150 meters (buffer). 
#' Extracts the mean and the standard deviation of the elevation within this AOI, and then
#' it calculates the coefficient of variation
#' @param points2process 
#'
#' @return a single value, the coefficient of variation of elevation in an 150 m buffer around the point
#' @export
#'
#' @examples
ELEV_SITE_CV<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  AOI<-st_buffer(st_transform(validgeometry, 6703),150)
  elev.mean<-ee_extract(USGS_NED, AOI, fun = ee$Reducer$mean(), scale=90)
  elev.stdev<-ee_extract(USGS_NED, AOI, fun = ee$Reducer$stdDev(), scale=90)
  media<-elev.stdev/elev.mean
  return(media)
}



#################################################
### These are VERY LARGE vector datasets --- I am inclined to query what is needed directly from disk 
### as opposed to load the entire vectors in memory
### Regular version of a function that works with an object loaded to memory
# Spatial join to get the square root of topo at the point (the line shapefile is already square rooted)
SQRT_TOPO<-function(points2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(points2process)
  crs2use<-crs(predictor_geometry)
  AOItrans<-st_transform(validgeometry, crs2use)
  AOI_Buffer<-st_join(AOItrans, predictor_geometry, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
  media<-AOI_Buffer$TOPOCV
  return(media)
}

#### Super fast version - loads into memory ONLY WHAT is strictly necessary
SQRT_TOPO<-function(points2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(points2process)
  AOItrans<-st_transform(validgeometry, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>% 
    st_geometry() %>% # convert to sfc
    st_buffer(150) %>% # buffer 150 meters
    st_as_text() # convert to well known text
  SQRT_TOPO.vec<-st_read(), wkt_filter = AOItrans_wkt)
  AOI_Buffer<-st_join(AOItrans, SQRT_TOPO.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
  media<-AOI_Buffer$TOPOCV
  return(media)
}
#################################################



####################

#   Temperature     #

####################

#################################################
### These are VERY LARGE vector datasets --- I am inclined to query what is needed directly from disk 
### as opposed to load the entire vectors in memory
### Regular version of a function that works with an object loaded to memory
SUMMER<-function(points2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(points2process)
  crs2use<-crs(predictor_geometry)
  AOItrans<-st_transform(validgeometry, crs2use)
  AOI_Buffer<-st_join(AOItrans, predictor_geometry, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
  media<-AOI_Buffer$summer
  return(media)
}

#### Super fast version - loads into memory ONLY WHAT is strictly necessary
SUMMER<-function(points2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(points2process)
  AOItrans<-st_transform(validgeometry, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>% 
    st_geometry() %>% # convert to sfc
    st_buffer(150) %>% # buffer 150 meters
    st_as_text() # convert to well known text
  SUMMER.vec<-st_read(), wkt_filter = AOItrans_wkt)
  AOI_Buffer<-st_join(AOItrans, SUMMER.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
  media<-AOI_Buffer$summer
  return(media)
}
#################################################

#################################################
### These are VERY LARGE vector datasets --- I am inclined to query what is needed directly from disk 
### as opposed to load the entire vectors in memory
### Regular version of a function that works with an object loaded to memory
WINTER<-function(points2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(points2process)
  crs2use<-crs(predictor_geometry)
  AOItrans<-st_transform(validgeometry, crs2use)
  AOI_Buffer<-st_join(AOItrans, predictor_geometry, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
  media<-AOI_Buffer$summer
  return(media)
}

#### Super fast version - loads into memory ONLY WHAT is strictly necessary
WINTER<-function(points2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(points2process)
  AOItrans<-st_transform(validgeometry, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>% 
    st_geometry() %>% # convert to sfc
    st_buffer(150) %>% # buffer 150 meters
    st_as_text() # convert to well known text
  WINTER.vec<-st_read(), wkt_filter = AOItrans_wkt)
  AOI_Buffer<-st_join(AOItrans, WINTER.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
  media<-AOI_Buffer$winter
  return(media)
}
#################################################


temp<-function(points2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(points2process)
  myvars <- "temp_Cx10"
  Pred_Input_All_USGS.vec <- predictor_geometry[myvars]
  crs2use<-crs(Pred_Input_All_USGS.vec)
  validgeometry<-st_transform(validgeometry, crs = crs2use)
  #Pred_Input_All_USGS.vec.WGS<-st_transform(Pred_Input_All_USGS.vec, crs = 4326)
  media<-st_intersection(validgeometry, Pred_Input_All_USGS.vec)%>%pull(temp_Cx10)
  return(media)
}


TMAX_PT<-function(points2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(predictor_geometry,validgeometry)/10
  return(media)
}




####################

#   Precipitation   #

####################
LOG_PRECIP_SITE<-function(points2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(points2process)
  media<-log10(raster::extract(predictor_geometry,validgeometry))
  return(media)
}

PPT_2MoAvg<-function(polygon2process, CurrentYear, JulianDate){
  validgeometry<-geojson_sf(polygon2process)
  curYear.2month<-CurrentYear
  # Obtain a GEE image that has the monthly precipitation for those months where sample can occur -- in this case from February to November
  prism.1<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-01-01"), paste0(curYear.2month,"-01-31")))$select('ppt')
  prism.2<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-02-01"), paste0(curYear.2month,"-02-28")))$select('ppt')
  prism.3<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-03-01"), paste0(curYear.2month,"-03-31")))$select('ppt')
  prism.4<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-04-01"), paste0(curYear.2month,"-04-30")))$select('ppt')
  prism.5<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-05-01"), paste0(curYear.2month,"-05-31")))$select('ppt')
  prism.6<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-06-01"), paste0(curYear.2month,"-06-30")))$select('ppt')
  prism.7<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-07-01"), paste0(curYear.2month,"-07-31")))$select('ppt')
  prism.8<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-08-01"), paste0(curYear.2month,"-08-31")))$select('ppt')
  prism.9<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-09-01"), paste0(curYear.2month,"-09-30")))$select('ppt')
  prism.10<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-10-01"), paste0(curYear.2month,"-10-31")))$select('ppt')
  prism.11<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-11-01"), paste0(curYear.2month,"-11-30")))$select('ppt')
  juliandate<-JulianDate
  month.cur<-as.Date(juliandate-1, origin = paste0(curYear.2month,"-01-01")) # Transform to a YYYY-MM-DD format
  monthy.cur<-as.numeric(substr(month.cur, 6, 7)) # Estimate the CURRENT month number based on the YYYY-MM-DD format
  monthy.pre<-monthy.cur-1 # Estimate the PREVIOUS month number based on the YYYY-MM-DD format
  xx<-eval(parse(text = paste0("prism.",monthy.cur))) # Evaluations that are required so that a variable is recognized as such
  xxx<-eval(parse(text = paste0("prism.",monthy.pre)))# Evaluations that are required so that a variable is recognized as such
  pcp.extraction.cur<-ee_extract(xx, validgeometry, fun = ee$Reducer$mean(), scale=50)%>% as_tibble() # Compute pcp for CURRENT month
  pcp.extraction.pre<-ee_extract(xxx, validgeometry, fun = ee$Reducer$mean(), scale=50)%>% as_tibble()# Compute pcp for PREVIOUS month
  validgeometry$PPT_2MoAvg<-unlist((pcp.extraction.pre+pcp.extraction.cur)/2)*100 # Obtain average and multiply by 100 so it is similar to Olson
  media<-validgeometry$PPT_2MoAvg
  return(media)
}

PPT_ACCUM<-function(points2process, CurrentYear){
  validgeometry<-geojson_sf(points2process)
  prevYear1<-CalendarYear-1
  prevYear0<-prevYear1-1
  WaterYearStart<-paste0(prevYear0,"-05-01")
  WaterYearEnd<-paste0(prevYear1,"-04-30")
  prism.accum0<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(WaterYearStart, WaterYearEnd))$select('ppt')
  prism.accum.precip<-prism.accum0$sum()
  media<-ee_extract(prism.accum.precip, validgeometry, fun = ee$Reducer$mean(), scale=50)
  return(media)
}


precip<-function(points2process,predictor_geometry, ...){
  validgeometry<-geojson_sf(points2process)
  myvars <- "precip_mm"
  Pred_Input_All_USGS.vec <- predictor_geometry[myvars]
  Pred_Input_All_USGS.vec.WGS<-st_transform(Pred_Input_All_USGS.vec, crs = 4326)
  media<-st_intersection(validgeometry, Pred_Input_All_USGS.vec.WGS)%>%pull(precip_mm)
  return(media)
}


####################

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




####################

#   Slope          #

####################

#' NHD Plus slope value taken from the nearest stream segment
#' st_transform changes the CRS of the point to meters using 5070 Albers Contiguous US, now we have the point in meter (projection)
#' Using the new object for the point in meters, a well-known text (WKT) string will be created to query the required vector predictor
#' this WKT can be used as an argument in st_read to query a big vector shapefile or geopackages and just bring into memory the AOI
#' i.e. like a bounding without overwhelming R
#' Buffer the point by 200m, interest with NHD streams, extract SLOPE value
#' @param points2process 
#'
#' @return
#' @export
#'
#' @examples
NHDSLOPE<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  AOItrans<-st_transform(validgeometry, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>% 
    st_geometry() %>% # convert to sfc
    st_buffer(200) %>% # buffer 200 meters
    st_as_text() # convert to well known text
  NHDSLOPE.vec<-st_read(here("Metrics/Colorado/Data","NHD_West_str_ord.shp"), wkt_filter = AOItrans_wkt)
  AOI_Buffer<-st_join(AOItrans, NHDSLOPE.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
  media<-AOI_Buffer$SLOPE
  return(media)
}



Slope_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  validgeobuf<-st_buffer(st_transform(validgeometry, 5072), 300) # transforming to CRS of NV D8 point Flow Direction
  write_sf(st_transform(validgeometry, 5072), here("wat.shp"))
  write_sf(validgeobuf,here("buffer_wat.shp"))
  inputD8<-here("NVMod/NVFLD8.tif")
  outputD8<-here("NVMod/NVFLD8_crop3.tif")
  reproD8<-gdalUtils::gdalwarp(srcfile =outputD8, dstfile = destD8, t_srs = '+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs')
  outputFL<-here("NVMod/NVFL_crop6.tif")
  inputwat<-here("wat.shp")
  inputpoly2crop<-here("buffer_wat.shp")
  destD8<-here("NVMod/NVFLD8_cropCRS.tif")
  destD8flow<-here("NVMod/NVFLD8Flow_crop.tif")
  destD8stream<-here("NVMod/NVFLD8Stream_crop.tif")
  #whitebox::wbt_clip_raster_to_polygon(input = inputD8, polygons = inputpoly2crop, output = outputD8) # clip the DF8 to the watershed bounds
  whitebox::wbt_d8_flow_accumulation(outputD8, destD8flow, out_type="cells", pntr = TRUE, esri_pntr = TRUE) #
  whitebox::wbt_extract_streams(destD8flow, destD8stream, threshold=5000.0)
  whitebox::wbt_downslope_flowpath_length(d8_pntr = outputD8,output = outputFL, watersheds = inputwat )
  media<-exact_extract(SOC.ras,validgeometry,'mean')
  return(media)
}

mask<-st_read(inputpoly2crop)
ptm <- proc.time()
writeRaster(raster::crop(raster::mask(rastrillo, mask),extent(mask)),datatype='INT1U',overwrite=TRUE,filename = here("NVMod/NVFLD8_crop3.tif"))
proc.time() - ptm


