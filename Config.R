config <- list(
  raster_base_path = "/Volumes/GIS/GIS_Stats/",
  ## Atmosphere !!!
  AtmCa  = "Atmos/Data/atm_ca/w001001.adf",
  AtmSO4  = "Atmos/Data/atm_so4/w001001.adf",
  AtmMg  = "Atmos/Data/atm_mg2.tif",
  
  ## Climate !!!
  LST32AVE  = "Climate/Data/lstfrz_usgs/w001001.adf",
  MINWD_WS  = "Climate/Data/Wdmin_usgs/w001001.adf",
  MEANP_WS  = "Climate/Data/meanp_usgs/w001001.adf",
  XWD_WS  = "Climate/Data/xwd_usgs/w001001.adf",
  SumAve_P  = "Climate/Data/sumave_p2.tif",
  MAXWD_WS  = "Climate/Data/Wdmax_usgs/w001001.adf",
  MINP_WS  = "Climate/Data/pmin_usgs/w001001.adf",
  TMAX_WS  = "Climate/Data/tmax_usgs/w001001.adf",
  
  ## Geology
  LPREM_mean  = "Geology/Data/lperm_2feb10/w001001.adf",
  
  ## Landcover
  EVI_MaxAve  = "Vegetation/Data/evi_max_10B.tif",
  
  ## Metrics
  PRMH_AVE  = "Soils/Data/permh_usgs/w001001.adf",
  CaO_Mean  = "Geology/Data/cao_19jan10/w001001.adf",
  MgO_Mean  = "Geology/Data/Mgo_19jan10/w001001.adf",
  S_Mean  = "Geology/Data/s_23aug10/w001001.adf",
  UCS_Mean  = "Geology/Data/ucs_19jan10/w001001.adf",
  BDH_AVE  = "Soils/Data/bdh_usgs/w001001.adf",
  KFCT_AVE  = "Soils/Data/kfact_usgs/w001001.adf",

  formula_type = list(
    AtmCa   = "mean",
    AtmSO4  = "mean",
    AtmMg   = "mean",
    
    ## Climate !!!
    LST32AVE  = "mean",
    MINWD_WS  = "mean",
    MEANP_WS  = "mean",
    XWD_WS    = "mean",
    SumAve_P  = "mean",
    MAXWD_WS  = "mean",
    MINP_WS   = "mean",
    TMAX_WS   = "mean",
    
    ## Geology
    LPREM_mean  = "mean",
    
    ## Landcover
    EVI_MaxAve  = "mean",
    
    ## Metrics
    PRMH_AVE  = "mean",
    CaO_Mean  = "mean",
    MgO_Mean  = "mean",
    S_Mean    = "mean",
    UCS_Mean  = "mean",
    BDH_AVE   = "mean",
    KFCT_AVE  = "mean"
  )  
)

