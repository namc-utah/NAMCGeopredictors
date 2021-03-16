


#### list of functions for each predictor #######################
########## Pure Vectors ###########################

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
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$ELVmean_WS<-NA
  ncolumn<-as.numeric(ncol(validgeometry))
  ptm <- proc.time()
  for (i in 1:nrow(validgeometry)){
    tryCatch({ #if an error is found then it is printed, but the loop does not break and continues with the next iteration
      objecto<-validgeometry[i,] # Take the first feature
      elmean<-ee_extract(USGS_NED, objecto, fun = ee$Reducer$mean(), scale=30)%>% as_tibble()
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
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$ELVmin_WS<-NA
  ncolumn<-as.numeric(ncol(validgeometry))
  ptm <- proc.time()
  for (i in 1:nrow(validgeometry)){
    tryCatch({ #if an error is found then it is printed, but the loop does not break and continues with the next iteration
      objecto<-validgeometry[i,] # Take the first feature
      elmean<-ee_extract(USGS_NED, objecto, fun = ee$Reducer$min(), scale=30)%>% as_tibble()
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
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$RH_WS<-exact_extract(RH_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$RH_WS)
  colnames(media)<-"RH_WS"
  return(media)
}

TMAX_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
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
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$TMEAN_UT_WS<-exact_extract(TMEAN_UT_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$TMEAN_UT_WS)
  colnames(media)<-"TMEAN_UT_WS"
  return(media)
}

TMIN_UT_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$TMIN_UT_WS<-exact_extract(TMIN_UT_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$TMIN_UT_WS)
  colnames(media)<-"TMIN_UT_WS"
  return(media)
}

MEANP_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$MEANP_WS<-exact_extract(MEANP_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$MEANP_WS)
  colnames(media)<-"MEANP_WS"
  return(media)
}

MAXP_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$MAXP_WS<-exact_extract(MAXP_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$MAXP_WS)
  colnames(media)<-"MAXP_WS"
  return(media)
}

MAXWD_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$MAXWD_WS<-exact_extract(MAXWD_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$MAXWD_WS)
  colnames(media)<-"MAXWD_WS"
  return(media)
}

FST32F_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$FST32F_WS<-exact_extract(FST32F_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$FST32F_WS)
  colnames(media)<-"FST32F_WS"
  return(media)
}

EVI_MAX_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
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
  sfobject<-geojson_sf(points2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$TMEAN_PT<-raster::extract(TMEAN_WS.ras,validgeometry)
  media<-as.data.frame(validgeometry$TMEAN_PT)
  colnames(media)<-"TMEAN_PT"
  return(media)
}

TMEAN_UT_PT<-function(points2process){
  sfobject<-geojson_sf(points2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$TMEAN_PT<-raster::extract(TMEAN_UT_WS.ras,validgeometry)
  media<-as.data.frame(validgeometry$TMEAN_PT)
  colnames(media)<-"TMEAN_PT"
  return(media)
}

# For UTDEQ this variable is called TMAX_AVE
TMAX_PT<-function(points2process){
  sfobject<-geojson_sf(points2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$TMAX_PT<-raster::extract(TMAX_WS.ras,validgeometry)
  media<-as.data.frame(validgeometry$TMAX_PT)
  colnames(media)<-"TMAX_PT"
  return(media)
}

# Extract the Lat / Long
DD_LAT_PT<-function(points2process){
  sfobject<-geojson_sf(points2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$DD_LAT_PT<-as.data.frame(st_coordinates(validgeometry))[,2]
  media<-as.data.frame(validgeometry$DD_LAT_PT)
  colnames(media)<-"DD_LAT_PT"
  return(media)
}

# Extract the Lat / Long
DD_LON_PT<-function(points2process){
  sfobject<-geojson_sf(points2process)
  validgeometry<-st_make_valid(sfobject)
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




