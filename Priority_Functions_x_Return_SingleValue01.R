
### For all functions use the package::function sintaxis


#### list of functions for each predictor #######################
####################################################################################################
####################################################################################################
####################################################################################################
AREA_SQKM<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-units::drop_units(sf::st_area(validgeometry)/1000000)
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

EVI_MAX_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(EVI_MAX_AVE.ras,validgeometry,'mean')
  return(media)
}

FST32F_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(FST32F_AVE.ras,validgeometry,'mean')
  return(media)
}

HYDR_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(HYDR_WS.ras,validgeometry,'mean')
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

MEANP_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(MEANP_AVE.ras,validgeometry,'mean')
  return(media)
}

MgO_Mean<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(MgO_Mean.ras,validgeometry,'mean')
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

SQ_KM<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-drop_units(st_area(validgeometry)/1000000)
  return(media)
}

SQRT_TOPO<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  crs2use<-crs(SQRT_TOPO.vec)
  AOItrans<-st_transform(validgeometry, crs2use)
  AOI_Buffer<-st_join(AOItrans, SQRT_TOPO.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
  media<-AOI_Buffer$TOPOCV
  return(media)
}

SumAve_P<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(SumAve_P.ras,validgeometry,'mean')
  return(media)
}

SUMMER<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  crs2use<-crs(SQRT_TOPO.vec)
  AOItrans<-st_transform(validgeometry, crs2use)
  AOI_Buffer<-st_join(AOItrans, SUMMER.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
  media<-AOI_Buffer$summer
  return(media)
}

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


####################################################################################################
####################################################################################################
####################################################################################################

WSA_SQKM<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$WSA_SQKM<-drop_units(st_area(validgeometry)/1000000)
  media<-as.data.frame(validgeometry$WSA_SQKM)
  colnames(media)<-"WSA_SQKM"
  return(media)
}

Wb_mx_area<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$Wb_mx_area<-NA
  for (i in 1:nrow(validgeometry)){
    tryCatch({ #if an error is found then it is printed, but the loop does not break and continues with the next iteration
      objecto<-validgeometry[i,] # Take the first feature 
      bodies<-st_intersection(Wb_mx_area.vec, objecto)
      bodies$AreaSqKm<-drop_units(st_area(bodies)/1000000)
      maxarea<-max(bodies$AreaSqKm)
      validgeometry$Wb_mx_area[i]<-maxarea 
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  media<-as.data.frame(validgeometry$Wb_mx_area)
  colnames(media)<-"Wb_mx_area"
  return(media)
}

Eco3_PT<-function(points2process){
  sfobject<-geojson_sf(points2process)
  validgeometry<-st_make_valid(sfobject)
  myvars <- "US_L3CODE3"
  Eco3_PT.vec <- Eco3_PT.vec[myvars]
  validgeometry$Eco3_PT01<-st_intersection(validgeometry, Eco3_PT.vec)%>%pull(US_L3CODE)
  validgeometry$Eco3_PT<- validgeometry %>%
    mutate(Eco3_PT = case_when(
      Eco3_PT01 == 23 ~ "Y",
      Eco3_PT01 != 23 ~ "N"))%>%pull(Eco3_PT)
  media<-as.data.frame(validgeometry$Eco3_PT)
  colnames(media)<-"Eco3_PT"
  return(media)
}

########## Pure GEE ###########################


ELVmean_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  #validgeometry<-sfobject
  #validgeometry<-st_make_valid(sfobject)
  validgeometry$ELVmean_WS<-NA
  ncolumn<-as.numeric(ncol(validgeometry))
  ptm <- proc.time()
  for (i in 1:nrow(validgeometry)){
    tryCatch({ #if an error is found then it is printed, but the loop does not break and continues with the next iteration
      objecto<-validgeometry[i,] # Take the first feature
      elmean<-ee_extract(USGS_NED, objecto, fun = ee$Reducer$mean(), scale=90)%>% as_tibble()
      elmean<-elmean[,ncolumn]
      validgeometry[[ncolumn]][i]<-elmean
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
  }
  proc.time() - ptm
  validgeometry$ELVmean_WS<-unlist(validgeometry$ELVmean_WS)
  media<-as.data.frame(validgeometry$ELVmean_WS)
  colnames(media)<-"ELVmean_WS"
  return(media)
}



ELVmin_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  #validgeometry<-st_make_valid(sfobject)
  validgeometry$ELVmin_WS<-NA
  ncolumn<-as.numeric(ncol(validgeometry))
  ptm <- proc.time()
  for (i in 1:nrow(validgeometry)){
    tryCatch({ #if an error is found then it is printed, but the loop does not break and continues with the next iteration
      objecto<-validgeometry[i,] # Take the first feature
      elmean<-ee_extract(USGS_NED, objecto, fun = ee$Reducer$min(), scale=90)%>% as_tibble()
      elmean<-elmean[,ncolumn]
      validgeometry[[ncolumn]][i]<-elmean
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
  }
  proc.time() - ptm
  validgeometry$ELVmin_WS<-unlist(validgeometry$ELVmin_WS)
  media<-as.data.frame(validgeometry$ELVmin_WS)
  colnames(media)<-"ELVmin_WS"
  return(media)
}




ELVmax_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$ELVmax_WS<-NA
  ncolumn<-as.numeric(ncol(validgeometry))
  ptm <- proc.time()
  for (i in 1:nrow(validgeometry)){
    tryCatch({ #if an error is found then it is printed, but the loop does not break and continues with the next iteration
      objecto<-validgeometry[i,] # Take the first feature
      elmean<-ee_extract(USGS_NED, objecto, fun = ee$Reducer$max(), scale=30)%>% as_tibble()
      elmean<-elmean[,ncolumn]
      validgeometry[[ncolumn]][i]<-elmean
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
  }
  proc.time() - ptm
  validgeometry$ELVmax_WS<-unlist(validgeometry$ELVmax_WS)
  media<-as.data.frame(validgeometry$ELVmax_WS)
  colnames(media)<-"ELVmax_WS"
  return(media)
}


PPT_ACCUM_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$PPT_ACCUM<-NA
  ncolumn<-as.numeric(ncol(validgeometry))
  ptm <- proc.time()
  for (i in 1:nrow(validgeometry)){
    tryCatch({ #if an error is found then it is printed, but the loop does not break and continues with the next iteration
      objecto<-validgeometry[i,] # Take the first feature
      pcpacum.extraction<-ee_extract(prism.accum.precip, objecto, fun = ee$Reducer$mean(), scale=50)%>% as_tibble()
      pcpacum.extraction<-pcpacum.extraction[,ncolumn]# because the tibble is a long list of attributes 
      validgeometry[[ncolumn]][i]<-pcpacum.extraction
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
  }
  proc.time() - ptm
  validgeometry$PPT_ACCUM<-unlist(validgeometry$PPT_ACCUM)
  media<-as.data.frame(validgeometry$PPT_ACCUM)
  colnames(media)<-"PPT_ACCUM"
  return(media)
}

PPT_2MoAvg_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$dates<-as.Date(validgeometry$SampleDate, "%m/%d/%y")
  validgeometry$julian <- yday(validgeometry$dates)
  validgeometry$PPT_2MoAvg<-NA
  ncolumn<-as.numeric(ncol(validgeometry))
  ncol.julian<-as.numeric(which(colnames(validgeometry) == "julian"))
  ptm <- proc.time()
  for (i in 1:nrow(validgeometry)){
    tryCatch({ #if an error is found then it is printed, but the loop does not break and continues with the next iteration
      juliandate<-validgeometry[[ncol.julian]][i] # Grab the Julian Date
      month.cur<-as.Date(juliandate-1, origin = paste0(curYear.2month,"-01-01")) # Transform to a YYYY-MM-DD format
      monthy.cur<-as.numeric(substr(month.cur, 6, 7)) # Estimate the CURRENT month number based on the YYYY-MM-DD format
      monthy.pre<-monthy.cur-1 # Estimate the PREVIOUS month number based on the YYYY-MM-DD format
      xx<-eval(parse(text = paste0("prism.",monthy.cur))) # Evaluations that are required so that a variable is recognized as such
      xxx<-eval(parse(text = paste0("prism.",monthy.pre)))# Evaluations that are required so that a variable is recognized as such
      objecto<-validgeometry[i,] # Take the first feature
      pcp.extraction.cur<-ee_extract(xx, objecto, fun = ee$Reducer$mean(), scale=50)%>% as_tibble() # Compute pcp for CURRENT month
      pcp.extraction.cur<-pcp.extraction.cur[,ncolumn]
      pcp.extraction.pre<-ee_extract(xxx, objecto, fun = ee$Reducer$mean(), scale=50)%>% as_tibble()# Compute pcp for PREVIOUS month
      pcp.extraction.pre<-pcp.extraction.pre[,ncolumn]
      pcp.extraction<-((pcp.extraction.pre+pcp.extraction.cur)/2)*100 # Obtain average and multiply by 100 so it is similar to Olson
      pcp.extraction<-pcp.extraction%>% as_tibble()
      #print(site)
      pcp.extraction<-pull(pcp.extraction)
      print(pcp.extraction)
      validgeometry[[ncolumn]][i]<-pcp.extraction
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
  }
  proc.time() - ptm
  validgeometry$PPT_2MoAvg<-unlist(validgeometry$PPT_2MoAvg)
  media<-as.data.frame(validgeometry$PPT_2MoAvg)
  colnames(media)<-"PPT_2MoAvg"
  return(media)
}

################## Areal Extractions ####################
#########################################################
KFACT<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$KFACT<-exact_extract(KFACT.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$KFACT)
  colnames(media)<-"KFACT"
  return(media)
}

PMIN_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$PMIN_WS<-exact_extract(PMIN_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$PMIN_WS)
  colnames(media)<-"PMIN_WS"
  return(media)
}


RH_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  #validgeometry<-st_make_valid(sfobject)
  validgeometry$RH_WS<-exact_extract(RH_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$RH_WS)
  colnames(media)<-"RH_WS"
  return(media)
}

TMAX_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  #validgeometry<-st_make_valid(sfobject)
  validgeometry$TMAX_WS<-exact_extract(TMAX_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$TMAX_WS)
  colnames(media)<-"TMAX_WS"
  return(media)
}

TMEAN_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$TMEAN_WS<-exact_extract(TMEAN_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$TMEAN_WS)
  colnames(media)<-"TMEAN_WS"
  return(media)
}

TMEAN_UT_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  #validgeometry<-st_make_valid(sfobject)
  validgeometry$TMEAN_UT_WS<-exact_extract(TMEAN_UT_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$TMEAN_UT_WS)
  colnames(media)<-"TMEAN_UT_WS"
  return(media)
}

TMIN_UT_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  #validgeometry<-st_make_valid(sfobject)
  validgeometry$TMIN_UT_WS<-exact_extract(TMIN_UT_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$TMIN_UT_WS)
  colnames(media)<-"TMIN_UT_WS"
  return(media)
}

MEANP_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  #validgeometry<-st_make_valid(sfobject)
  validgeometry$MEANP_WS<-exact_extract(MEANP_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$MEANP_WS)
  colnames(media)<-"MEANP_WS"
  return(media)
}

MAXP_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  #validgeometry<-st_make_valid(sfobject)
  validgeometry$MAXP_WS<-exact_extract(MAXP_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$MAXP_WS)
  colnames(media)<-"MAXP_WS"
  return(media)
}

MAXWD_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  #validgeometry<-st_make_valid(sfobject)
  validgeometry$MAXWD_WS<-exact_extract(MAXWD_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$MAXWD_WS)
  colnames(media)<-"MAXWD_WS"
  return(media)
}

FST32F_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  #validgeometry<-st_make_valid(sfobject)
  validgeometry$FST32F_WS<-exact_extract(FST32F_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$FST32F_WS)
  colnames(media)<-"FST32F_WS"
  return(media)
}

EVI_MAX_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  #validgeometry<-st_make_valid(sfobject)
  validgeometry$EVI_MAX_WS<-exact_extract(EVI_MAX_AVE.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$EVI_MAX_WS)
  colnames(media)<-"EVI_MAX_WS"
  return(media)
}


Vol_ave_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$Vol_ave_WS<-exact_extract(Vol_ave.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$Vol_ave_WS)
  colnames(media)<-"Vol_ave"
  return(media)
}

TMIN_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$TMIN_WS<-exact_extract(TMIN_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$TMIN_WS)
  colnames(media)<-"TMIN_WS"
  return(media)
}


XWD_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$XWD_WS<-exact_extract(XWD_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$XWD_WS)
  colnames(media)<-"XWD_WS"
  return(media)
}

EVI_AveAve_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$EVI_AveAve<-exact_extract(EVI_AveAve.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$EVI_AveAve)
  colnames(media)<-"EVI_AveAve"
  return(media)
}

CaO_Mean_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$CaO_Mean<-exact_extract(CaO_Mean.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$CaO_Mean)
  colnames(media)<-"CaO_Mean"
  return(media)
}

TP_Mean_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$TP_Mean<-exact_extract(TP_Mean.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$TP_Mean)
  colnames(media)<-"TP_Mean"
  return(media)
}

AWC_soil_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$AWC_soil<-exact_extract(AWC_soil.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$AWC_soil)
  colnames(media)<-"AWC_soil"
  return(media)
}

Db3rdbar_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$Db3rdbar<-exact_extract(Db3rdbar.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$Db3rdbar)
  colnames(media)<-"Db3rdbar"
  return(media)
}


GW_P_Sp_Mx_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$GW_P_Sp_Mx<-exact_extract(GW_P_Sp_Mx.ras,validgeometry,'max')
  media<-as.data.frame(validgeometry$GW_P_Sp_Mx)
  colnames(media)<-"GW_P_Sp_Mx"
  return(media)
}

SOC_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$SOC<-exact_extract(SOC.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$SOC)
  colnames(media)<-"SOC"
  return(media)
}


alru_dom_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$AREAHA<-drop_units(st_area(validgeometry)/10000)
  validgeometry$alru_dom_01<-exact_extract(alru_dom.ras,validgeometry,'count')
  validgeometry$alru_dom<-(validgeometry$alru_dom_01*0.09/validgeometry$AREAHA)*100
  media<-as.data.frame(validgeometry$alru_dom)
  colnames(media)<-"alru_dom"
  return(media)
}

Evergr_ave_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$AREAHA<-drop_units(st_area(validgeometry)/10000)
  validgeometry$Evergr_ave_01<-exact_extract(Evergr_ave.ras,validgeometry,'sum')
  validgeometry$Evergr_ave<-(validgeometry$Evergr_ave_01*0.09/validgeometry$AREAHA)
  media<-as.data.frame(validgeometry$Evergr_ave)
  colnames(media)<-"Evergr_ave"
  return(media)
}

Pct_Alfi_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$AREAHA<-drop_units(st_area(validgeometry)/10000)
  validgeometry$Pct_Alfi_01<-exact_extract(Pct_Alfi.ras,validgeometry,'sum')
  validgeometry$Pct_Alfi<-(validgeometry$Pct_Alfi_01*25/validgeometry$AREAHA)*100
  media<-as.data.frame(validgeometry$Pct_Alfi)
  colnames(media)<-"Pct_Alfi"
  return(media)
}








################## Point Extractions ####################
#########################################################
TMEAN_PT<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  #validgeometry<-st_make_valid(sfobject)
  validgeometry$TMEAN_PT<-raster::extract(TMEAN_WS.ras,validgeometry)
  media<-as.data.frame(validgeometry$TMEAN_PT)
  colnames(media)<-"TMEAN_PT"
  return(media)
}

TMEAN_UT_PT<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  #validgeometry<-st_make_valid(sfobject)
  validgeometry$TMEAN_PT<-raster::extract(TMEAN_UT_WS.ras,validgeometry)
  media<-as.data.frame(validgeometry$TMEAN_PT)
  colnames(media)<-"TMEAN_PT"
  return(media)
}

# For UTDEQ this variable is called TMAX_AVE
TMAX_PT<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  #validgeometry<-st_make_valid(sfobject)
  validgeometry$TMAX_PT<-raster::extract(TMAX_WS.ras,validgeometry)
  media<-as.data.frame(validgeometry$TMAX_PT)
  colnames(media)<-"TMAX_PT"
  return(media)
}

LOG_LT_PPT_PT<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  #validgeometry<-st_make_valid(sfobject)
  validgeometry$LOG_LT_PPT_PT<-raster::extract(MEANP_PIBO_WS.ras,validgeometry)
  media<-as.data.frame(log10(validgeometry$LOG_LT_PPT_PT))
  colnames(media)<-"LOG_LT_PPT_PT"
  return(media)
}
  
  
  
  

# Extract the Lat / Long
DD_LAT_PT<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  #validgeometry<-st_make_valid(sfobject)
  validgeometry$DD_LAT_PT<-as.data.frame(st_coordinates(validgeometry))[,2]
  media<-as.data.frame(validgeometry$DD_LAT_PT)
  colnames(media)<-"DD_LAT_PT"
  return(media)
}

# Extract the Lat / Long
DD_LON_PT<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  #validgeometry<-st_make_valid(sfobject)
  validgeometry$DD_LON_PT<-as.data.frame(st_coordinates(validgeometry))[,1]
  media<-as.data.frame(validgeometry$DD_LON_PT)
  colnames(media)<-"DD_LON_PT"
  return(media)
}



PMIN_PT<-function(points2process){
  sfobject<-geojson_sf(points2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$PMIN_PT<-raster::extract(PMIN_WS.ras,validgeometry)
  media<-as.data.frame(validgeometry$PMIN_PT)
  colnames(media)<-"PMIN_PT"
  return(media)
}

AtmCa_PT<-function(points2process){
  sfobject<-geojson_sf(points2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$AtmCa<-terra::extract(AtmCa.ras,validgeometry)
  media<-as.data.frame(validgeometry$AtmCa)
  colnames(media)<-"AtmCa"
  return(media)
}

AtmSO4_PT<-function(points2process){
  sfobject<-geojson_sf(points2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$AtmSO4<-terra::extract(AtmSO4.ras,validgeometry)
  media<-as.data.frame(validgeometry$AtmSO4)
  colnames(media)<-"AtmSO4"
  return(media)
}

AtmNa_PT<-function(points2process){
  sfobject<-geojson_sf(points2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$AtmNa<-terra::extract(AtmNa.ras,validgeometry)
  media<-as.data.frame(validgeometry$AtmNa)
  colnames(media)<-"AtmNa"
  return(media)
}

AtmNO3_PT<-function(points2process){
  sfobject<-geojson_sf(points2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$AtmNO3<-terra::extract(AtmNO3.ras,validgeometry)
  media<-as.data.frame(validgeometry$AtmNO3)
  colnames(media)<-"AtmNO3"
  return(media)
}

PT_Tmin_PT<-function(points2process){
  sfobject<-geojson_sf(points2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$PT_Tmin<-terra::extract(PT_Tmin.ras,validgeometry)
  media<-as.data.frame(validgeometry$PT_Tmin)
  colnames(media)<-"PT_Tmin"
  return(media)
}




