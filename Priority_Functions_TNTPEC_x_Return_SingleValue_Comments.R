
### For all functions use the package::function sintaxis


#### list of functions for each predictor #######################
####################################################################################################
####################################################################################################
####################################################################################################

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
alru_dom<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  validgeometry$AREAHA<-drop_units(st_area(validgeometry)/10000)
  validgeometry$alru_dom_01<-exact_extract(alru_dom.ras,validgeometry,'count')
  media<-(validgeometry$alru_dom_01*0.09/validgeometry$AREAHA)*100
  return(media)
}

#' Area of the watershed in sq km
#' It obtains the area in square kilometers --> drop_units(st_area(validgeometry)/1000000) <-- for the watershed
#' @param polygon2process this is a geojson string for the watershed
#' The geojson is converted to an object of type sf -->validgeometry<-geojson_sf(polygon2process)<--
#' @return this functions returns one value which is the area of the watershed
#' @export
#'
#' @examples
AREA_SQKM<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-units::drop_units(sf::st_area(validgeometry)/1000000)
  return(media)
}

#' Atmospheric calcium at the point
#' This functions takes the GIS_Stats/Atmos/Data/atm_ca dataset
#' it extracts the value at the point  -->raster::extract(AtmCa.ras,validgeometry) <--
#' @param points2process this is a geojson string for the pourpoint
#' The geojson is converted to an object of type sf -->validgeometry<-geojson_sf(points2process)<--
#' @return this functions returns one value which is the Raster value at the point
#' @export
#'
#' @examples
AtmCa<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(AtmCa.ras,validgeometry)
  return(media)
}

#' Atmospheric Mg at the point
#' This functions takes the GIS_Stats/Atmos/Data/atm_mg dataset
#' it extracts the value at the point  -->raster::extract(AtmMg.ras,validgeometry) <--
#' @param points2process this is a geojson string for the pourpoint
#' The geojson is converted to an object of type sf -->validgeometry<-geojson_sf(points2process)<--
#' @return this functions returns one value which is the Raster value at the point
#' @export
#'
#' @examples
AtmMg<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(AtmMg.ras,validgeometry)
  return(media)
}

#' Atmospheric Na at the point
#' This functions takes the GIS_Stats/Atmos/Data/atm_na dataset
#' it extracts the value at the point  -->raster::extract(AtmNa.ras,validgeometry) <--
#' @param points2process this is a geojson string for the pourpoint
#' The geojson is converted to an object of type sf -->validgeometry<-geojson_sf(points2process)<--
#' @return this functions returns one value which is the Raster value at the point
#' @export
#'
#' @examples
AtmNa<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(AtmNa.ras,validgeometry)
  return(media)
}

#' Atmospheric NO3 at the point
#' This functions takes the GIS_Stats/Atmos/Data/atm_no3 dataset
#' it extracts the value at the point  -->raster::extract(AtmNO3.ras,validgeometry) <--
#' @param points2process this is a geojson string for the pourpoint
#' The geojson is converted to an object of type sf -->validgeometry<-geojson_sf(points2process)<--
#' @return this functions returns one value which is the Raster value at the point
#' @export
#'
#' @examples
AtmNO3<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(AtmNO3.ras,validgeometry)
  return(media)
}

#' Atmospheric SO4 at the point
#' This functions takes the GIS_Stats/Atmos/Data/atm_so4 dataset
#' it extracts the value at the point  -->raster::extract(AtmSO4.ras,validgeometry) <--
#' @param points2process this is a geojson string for the pourpoint
#' The geojson is converted to an object of type sf -->validgeometry<-geojson_sf(points2process)<--
#' @return this functions returns one value which is the Raster value at the point
#' @export
#'
#' @examples
AtmSO4<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(AtmSO4.ras,validgeometry)
  return(media)
}

#' Soil Water Capacity across the watershed
#' This functions takes the GIS_Stats/Soils/Data/awc dataset
#' it extracts zonal statistics mean for the watershed  -->exact_extract(AWC_soil.ras,validgeometry,'mean') <--
#' @param polygon2process this is a geojson string for the watershed
#' The geojson is converted to an object of type sf -->validgeometry<-geojson_sf(polygon2process)<--
#' @return this functions returns one value which is the mean of raster cells in the watershed
#' @export
#'
#' @examples
AWC_soil<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(AWC_soil.ras,validgeometry,'mean')
  return(media)
}

BDH_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exactextractr::exact_extract(BDH_AVE.ras,validgeometry,'mean')
  return(media)
}

BFI_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exactextractr::exact_extract(BFI_WS.ras,validgeometry,'mean')
  return(media)
}

CaO_Mean<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(CaO_Mean.ras,validgeometry,'mean')
  return(media)
}

Db3rdbar_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(Db3rdbar.ras,validgeometry,'mean')
  return(media)
}

#' Latitude of the point
#' This function returns the Y coordinate of the point in decimal degrees
#' The st_coordinates function returns the second column [,2] which is the latitude
#' @param points2process 
#'
#' @return
#' @export
#'
#' @examples
DD_LAT_Y<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-sf::st_coordinates(validgeometry)[,2]
  return(media)
}

east<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  EcoregionWGS<-st_transform(Ecoregion, crs = 4326)# transforming the input vector to the CRS of the geojson points
  temp01<-st_intersection(validgeometry,EcoregionWGS)
  temp01$east<-0
  temp01$east[temp01$EastWest=="East"]<-1
  media<-temp01$east
  return(media)
}

ECO3<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  myvars <- "US_L3CODE"
  Eco3_PT.vec <- Eco3_PT.vec[myvars]
  Eco3_PT.vec.WGS<-st_transform(Eco3_PT.vec, crs = 4326)
  media<-st_intersection(validgeometry, Eco3_PT.vec.WGS)%>%pull(US_L3CODE)
  return(media)
}


ECO4<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  myvars <- "US_L4CODE"
  Eco4_PT.vec <- Eco4_PT.vec[myvars]
  Eco4_PT.vec.WGS<-st_transform(Eco4_PT.vec, crs = 4326)
  media<-st_intersection(validgeometry, Eco4_PT.vec.WGS)%>%pull(US_L4CODE)
  return(media)
}

ELEV_MEAN<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-ee_extract(USGS_NED, validgeometry, fun = ee$Reducer$mean(), scale=90)
  return(media)
}

ELEV_MIN<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-ee_extract(USGS_NED, validgeometry, fun = ee$Reducer$min(), scale=90)
  return(media)
}

ELEV_RANGE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  max<-ee_extract(USGS_NED, validgeometry, fun = ee$Reducer$max(), scale=90)
  min<-ee_extract(USGS_NED, validgeometry, fun = ee$Reducer$min(), scale=90)
  media<-max-min
  return(media)
}

ELEV_SITE<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-ee_extract(USGS_NED, validgeometry, scale=90)/10 
  return(media)
}

elev_sqrt<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  elevation<-ee_extract(USGS_NED, validgeometry, scale=90)
  media<-sqrt((elevation/10))
  return(media)
}

ELEV_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-ee_extract(USGS_NED, validgeometry, fun = ee$Reducer$mean(), scale=90)/100
  return(media)
}

ELVcv_PT<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  AOI<-st_buffer(st_transform(validgeometry, 6703),150)
  elev.mean<-ee_extract(USGS_NED, AOI, fun = ee$Reducer$mean(), scale=90)
  elev.stdev<-ee_extract(USGS_NED, AOI, fun = ee$Reducer$stdDev(), scale=90)
  media<-elev.stdev/elev.mean
  return(media)
}

ELVmax_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-ee_extract(USGS_NED, validgeometry, fun = ee$Reducer$max(), scale=90)
  return(media)
}

ELVmean_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-ee_extract(USGS_NED, validgeometry, fun = ee$Reducer$mean(), scale=90)
  return(media)
}

ELVmin_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-ee_extract(USGS_NED, validgeometry, fun = ee$Reducer$min(), scale=90)
  return(media)
}

ER13<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  validgeometry<-st_transform(validgeometry, 5070)
  myvars <- "US_L3CODE"
  Eco3_PT.vec <- Eco3_PT.vec[myvars]
  validgeometry$Eco3_PT01<-st_intersection(validgeometry, Eco3_PT.vec)%>%pull(US_L3CODE)
  validgeometry$ER13<- validgeometry %>%
    mutate(ER13 = case_when(
      Eco3_PT01 == 23 ~ "Y",
      Eco3_PT01 != 23 ~ "N"))%>%pull(ER13)
  media<-validgeometry$ER13
  return(media)
}

Evergr_ave<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  validgeometry$AREAHA<-drop_units(st_area(validgeometry)/10000)
  validgeometry$Evergr_ave_01<-exact_extract(Evergr_ave.ras,validgeometry,'sum')
  validgeometry$Evergr_ave<-(validgeometry$Evergr_ave_01*0.09/validgeometry$AREAHA)
  media<-validgeometry$Evergr_ave
  return(media)
}

EVI_AveAve<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(EVI_AveAve.ras,validgeometry,'mean')
  return(media)
}

EVI_MAX_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(EVI_MAX_AVE.ras,validgeometry,'mean')
  return(media)
}

EVI_MaxAve<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(EVI_MaxAve.ras,validgeometry,'mean')
  return(media)
}

FST32F_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(FST32F_AVE.ras,validgeometry,'mean')
  return(media)
}

GW_P_Sp_Mx<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(GW_P_Sp_Mx.ras,validgeometry,'max')
  return(media)
}

HYDR_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(HYDR_WS.ras,validgeometry,'mean')
  return(media)
}

KFACT<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(Kfact.ras,validgeometry,'mean')
  return(media)
}

KFCT_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(KFCT_AVE.ras,validgeometry,'mean')
  return(media)
}

KFCT_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(KFCT_AVE.ras,validgeometry,'mean')
  return(media)
}

Lat_Dec<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-st_coordinates(validgeometry)[,2]
  return(media)
}

LOG_KM2<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-log10(drop_units(st_area(validgeometry)/1000000))
  return(media)
}

LOG_LT_PPT_PT<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-log10(raster::extract(LOG_LT_PPT_PT.ras,validgeometry))
  return(media)
}

LOG_XP_PT<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-log10(raster::extract(LOG_XP_PT.ras,validgeometry))
  return(media)
}

long<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-st_coordinates(validgeometry)[,1]
  return(media)
}

Lon_Dec<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-st_coordinates(validgeometry)[,1]
  return(media)
}

LPREM_mean<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(LPREM_mean.ras,validgeometry,'mean')
  return(media)
}

LST32AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(LST32AVE.ras,validgeometry,'mean')
  return(media)
}

MAXP_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(MAXP_AVE.ras,validgeometry,'mean')
  return(media)
}

MAXWD_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(MAXWD_AVE.ras,validgeometry,'mean')
  return(media)
}

MAXWD_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(MAXWD_WS.ras,validgeometry,'mean')
  return(media)
}

MEANP_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(MEANP_AVE.ras,validgeometry,'mean')
  return(media)
}

MEANP_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(MEANP_WS.ras,validgeometry,'mean')
  return(media)
}

MgO_Mean<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(MgO_Mean.ras,validgeometry,'mean')
  return(media)
}

MINP_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(MINP_WS.ras,validgeometry,'mean')
  return(media)
}

MINWD_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(MINWD_WS.ras,validgeometry,'mean')
  return(media)
}

N_MEAN<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(N_MEAN.ras,validgeometry,'mean')
  return(media)
}

New_Lat<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-st_coordinates(validgeometry)[,2]
  return(media)
}

New_Long<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-st_coordinates(validgeometry)[,1]
  return(media)
}

NHDSLOPE<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  AOItrans<-st_transform(validgeometry, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>% 
    st_geometry() %>% # convert to sfc
    st_buffer(200) %>% # buffer 200 meters
    st_as_text() # convert to well known text
  NHDSLOPE.vec<-st_read(here("GIS_Stats01/Metrics/Colorado/Data","NHD_West_str_ord.shp"), wkt_filter = AOItrans_wkt)
  AOI_Buffer<-st_join(AOItrans, NHDSLOPE.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
  media<-AOI_Buffer$SLOPE
  return(media)
}

P_MEAN<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(P_MEAN.ras,validgeometry,'mean')
  return(media)
}

Pct_Alfi<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  validgeometry$AREAHA<-drop_units(st_area(validgeometry)/10000)
  validgeometry$Pct_Alfi_01<-exact_extract(Pct_Alfi.ras,validgeometry,'sum')
  validgeometry$Pct_Alfi<-(validgeometry$Pct_Alfi_01*25/validgeometry$AREAHA)*100
  media<-validgeometry$Pct_Alfi
  return(media)
}

PCT_SEDIM<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(validgeometry)
  crs2use<-crs(PCT_SEDIM.vec)
  validgeometry<-st_transform(validgeometry,crs=crs2use)
  PCT_SEDIM.vec<-st_make_valid(PCT_SEDIM.vec)
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

Pmax_PT<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(Pmax_WS.ras,validgeometry)
  return(media)
}

Pmax_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(Pmax_WS.ras,validgeometry,'mean')
  return(media)
}

Pmin_PT<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(PMIN_WS.ras,validgeometry)
  return(media)
}

Pmin_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(PMIN_WS.ras,validgeometry,'mean')
  return(media)
}

PPT_00_09<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(PPT_00_09.ras,validgeometry)
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

PPT_ACCUM<-function(points2process, CalendarYear){
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

PRCPSHORTW<-function(points2process, CalendarYear){
  validgeometry<-geojson_sf(points2process)
  WaterYearStart<-paste0(CalendarYear,"-01-01")
  WaterYearEnd<-paste0(CalendarYear,"-12-31")
  prism.accum0<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(WaterYearStart, WaterYearEnd))$select('ppt')
  prism.accum.precip<-prism.accum0$sum()
  media<-ee_extract(prism.accum.precip, validgeometry, fun = ee$Reducer$mean(), scale=4000)
  return(media)
}
  
precip<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  myvars <- "precip_mm"
  Pred_Input_All_USGS.vec <- Pred_Input_All_USGS.vec[myvars]
  Pred_Input_All_USGS.vec.WGS<-st_transform(Pred_Input_All_USGS.vec, crs = 4326)
  media<-st_intersection(validgeometry, Pred_Input_All_USGS.vec.WGS)%>%pull(precip_mm)
  return(media)
}

PRMH_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(PRMH_AVE.ras,validgeometry,'mean')
  return(media)
}

PT_Tmin<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(PT_Tmin.ras,validgeometry)
  return(media)
}

RH_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(RH_AVE.ras,validgeometry,'mean')
  return(media)
}

RH_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(RH_AVE.ras,validgeometry,'mean')
  return(media)
}

S_Mean<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(S_Mean.ras,validgeometry,'mean')
  return(media)
}

SITE_ELEV<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-ee_extract(USGS_NED, validgeometry, scale=90)/10 
  return(media)
}

SOC<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(SOC.ras,validgeometry,'mean')
  return(media)
}

SQ_KM<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-drop_units(st_area(validgeometry)/1000000)
  return(media)
}
#################################################
### These are VERY LARGE vector datasets --- I am inclined to query what is needed directly from disk 
### as opposed to load the entire vectors in memory
### Regular version of a function that works with an object loaded to memory
SQRT_TOPO<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  crs2use<-crs(SQRT_TOPO.vec)
  AOItrans<-st_transform(validgeometry, crs2use)
  AOI_Buffer<-st_join(AOItrans, SQRT_TOPO.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
  media<-AOI_Buffer$TOPOCV
  return(media)
}

#### Super fast version - loads into memory ONLY WHAT is strictly necessary
SQRT_TOPO<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  AOItrans<-st_transform(validgeometry, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>% 
    st_geometry() %>% # convert to sfc
    st_buffer(150) %>% # buffer 150 meters
    st_as_text() # convert to well known text
  SQRT_TOPO.vec<-st_read(here("GIS_Stats01/Metrics/Colorado/Data","SQRT_TOPO6703.shp"), wkt_filter = AOItrans_wkt)
  AOI_Buffer<-st_join(AOItrans, SQRT_TOPO.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
  media<-AOI_Buffer$TOPOCV
  return(media)
}
#################################################
SumAve_P<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(SumAve_P.ras,validgeometry,'mean')
  return(media)
}
#################################################
### These are VERY LARGE vector datasets --- I am inclined to query what is needed directly from disk 
### as opposed to load the entire vectors in memory
### Regular version of a function that works with an object loaded to memory
SUMMER<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  crs2use<-crs(SQRT_TOPO.vec)
  AOItrans<-st_transform(validgeometry, crs2use)
  AOI_Buffer<-st_join(AOItrans, SUMMER.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
  media<-AOI_Buffer$summer
  return(media)
}

#### Super fast version - loads into memory ONLY WHAT is strictly necessary
SUMMER<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  AOItrans<-st_transform(validgeometry, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>% 
    st_geometry() %>% # convert to sfc
    st_buffer(150) %>% # buffer 150 meters
    st_as_text() # convert to well known text
  SUMMER.vec<-st_read(here("GIS_Stats01/Metrics/Colorado/Data","summer_6703.shp"), wkt_filter = AOItrans_wkt)
  AOI_Buffer<-st_join(AOItrans, SUMMER.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
  media<-AOI_Buffer$summer
  return(media)
}
#################################################
temp<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  myvars <- "temp_Cx10"
  Pred_Input_All_USGS.vec <- Pred_Input_All_USGS.vec[myvars]
  crs2use<-crs(Pred_Input_All_USGS.vec)
  validgeometry<-st_transform(validgeometry, crs = crs2use)
  #Pred_Input_All_USGS.vec.WGS<-st_transform(Pred_Input_All_USGS.vec, crs = 4326)
  media<-st_intersection(validgeometry, Pred_Input_All_USGS.vec)%>%pull(temp_Cx10)
  return(media)
}

TEMP_00_09<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(TEMP_00_09.ras,validgeometry)
  return(media)
}

Tmax_PT<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(Tmax_WS.ras,validgeometry)/10
  return(media)
}

TMAX_PT<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(Tmax_WS.ras,validgeometry)/10
  return(media)
}


TMAX_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(Tmax_WS.ras,validgeometry,'mean')
  return(media)
}

TMEAN_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(TMEAN_AVE.ras,validgeometry,'mean')
  return(media)
}

TMEAN_PT<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(TMEAN_WS.ras,validgeometry)
  return(media)
}

TMEAN_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(TMEAN_WS.ras,validgeometry,'mean')
  return(media)
}

TMEANPT<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(TMEAN_AVE.ras,validgeometry)
  return(media)
}

TMIN_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(TMIN_AVE.ras,validgeometry,'mean')
  return(media)
}

TMIN_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(TMIN_WS.ras,validgeometry,'mean')
  return(media)
}

TP_Mean<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(TP_Mean.ras,validgeometry,'mean')
  return(media)
}

UCS_Mean<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(UCS_Mean.ras,validgeometry,'mean')
  return(media)
}

Vol_ave<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(Vol_ave.ras,validgeometry,'mean')
  return(media)
}

Wb_mx_area<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  validgeometry<-st_transform(validgeometry, 5070)
  validgeometry2<-st_make_valid(validgeometry)
  bodies<-st_intersection(Wb_mx_area.vec, validgeometry2)
  bodies$AreaSqKm<-drop_units(st_area(bodies)/1000000)
  media<-ifelse(is.infinite(max(bodies$AreaSqKm)),0,max(bodies$AreaSqKm)) 
  return(media)
}

WDmax_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(WDmax_WS.ras,validgeometry,'mean')
  return(media)
}

#################################################
### These are VERY LARGE vector datasets --- I am inclined to query what is needed directly from disk 
### as opposed to load the entire vectors in memory
### Regular version of a function that works with an object loaded to memory
WINTER<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  crs2use<-crs(WINTER.vec)
  AOItrans<-st_transform(validgeometry, crs2use)
  AOI_Buffer<-st_join(AOItrans, WINTER.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
  media<-AOI_Buffer$summer
  return(media)
}

#### Super fast version - loads into memory ONLY WHAT is strictly necessary
WINTER<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  AOItrans<-st_transform(validgeometry, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>% 
    st_geometry() %>% # convert to sfc
    st_buffer(150) %>% # buffer 150 meters
    st_as_text() # convert to well known text
  WINTER.vec<-st_read(here("GIS_Stats01/Metrics/Colorado/Data","winter_6703.shp"), wkt_filter = AOItrans_wkt)
  AOI_Buffer<-st_join(AOItrans, WINTER.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
  media<-AOI_Buffer$winter
  return(media)
}
#################################################

WSA_SQKM<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-drop_units(st_area(validgeometry)/1000000)
  return(media)
}

XWD_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(XWD_WS.ras,validgeometry,'mean')
  return(media)
}
####################################################################################################
####################################################################################################
####################################################################################################





