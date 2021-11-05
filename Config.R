
## Load required packages
## Load useful packages
library(sf)
library(raster)
library(data.table)
wd="Z://GIS/GIS_Stats"
library(ggpubr)
library(mapview)
library(prism)
library(exactextractr)
library(mapedit)
library(reticulate)
library(rgee)
library(tidyverse)
library(survival)
library(dplyr)
library(nhdplusTools)
library(lubridate)
library(units)
library(geojsonio)
library(rmapshaper)
library(whitebox)
ee_install_upgrade()
ee_Initialize()
# if error run
#ee_install()
ee_install_set_pyenv(py_path ="C:/Users/jenni/AppData/Local/r-miniconda/envs/r-reticulatepython.exe", py_env="rgee")
# restart R
#ee_install()
ee_check()
use_python("C:/Users/jenni/AppData/Local/r-miniconda/envs/r-reticulate/python.exe")
> py_config()
python:         C:/Users/jenni/AppData/Local/r-miniconda/envs/rgee/python.exe
libpython:      C:/Users/jenni/AppData/Local/r-miniconda/envs/rgee/python39.dll
pythonhome:     C:/Users/jenni/AppData/Local/r-miniconda/envs/rgee
version:        3.9.4 | packaged by conda-forge | (default, May 10 2021, 22:10:34) [MSC v.1916 64 bit (AMD64)]
Architecture:   64bit
numpy:          C:/Users/jenni/AppData/Local/r-miniconda/envs/rgee/Lib/site-packages/numpy
numpy_version:  1.20.3
ee:             C:\Users\jenni\AppData\Local\R-MINI~1\envs\rgee\lib\site-packages\ee\__init__.p

NOTE: Python version was forced by RETICULATE_PYTHON


###### Define predictors GEE
USGS_NED<-ee$Image("USGS/NED")$select("elevation") # elevation
slopegee<-ee$Terrain$slope(USGS_NED) # slope
slopegee.perc<- slopegee$divide(180)$multiply(3.14159)$tan()$multiply(1)$rename("percent")#Slope percent


###### Define predictors stored in-house
## Climate !!!
TMIN_WS.ras<-raster(paste0(wd,"/Climate/Data/tmin_oldtntp/w001001.adf")) 
TMIN_UT_WS.ras<-raster(paste0(wd,"/Climate/Data/tmin_usgs/w001001.adf"))# UTDEQ version
PT_Tmin.ras<-raster(paste0(wd,"/Climate/Data/tmin_usgs/w001001.adf"))
TMIN_AVE.ras<-raster(paste0(wd,"/Climate/Data/tmin_usgs/w001001.adf"))# UTDEQ version
KFACT.ras<-raster(paste0(wd,"/Soils/Data/kfact_usgs/w001001.adf"))
PMIN_WS.ras<-raster(paste0(wd,"/Climate/Data/pmin_usgs/w001001.adf"))
RH_WS.ras<-raster(paste0(wd,"/Climate/Data/rhmean_usgs/w001001.adf"))# UTDEQ version is RH_AVE
TMAX_WS.ras<-raster(paste0(wd,"/Climate/Data/tmax_usgs/w001001.adf")) # UTDEQ version is TMAX_AVE
TMEAN_WS.ras<-raster(paste0(wd,"/Climate/Data/tmean_usgs/w001001.adf"))
TMEAN_UT_WS.ras<-raster(paste0(wd,"/Climate/Data/tmean_usgsut/w001001.adf"))# UTDEQ version
XWD_WS.ras<-raster(paste0(wd,"/Climate/Data/xwd_usgs/w001001.adf"))
MEANP_WS.ras<-raster(paste0(wd,"/Climate/Data/meanp_usgs/w001001.adf"))# UTDEQ version
MAXP_WS.ras<-raster(paste0(wd,"/Climate/Data/pmax_usgs/w001001.adf"))# UTDEQ version
MAXWD_WS.ras<-raster(paste0(wd,"/Climate/Data/Wdmax_usgs/w001001.adf"))# UTDEQ version
FST32F_WS.ras<-raster(paste0(wd,"/Climate/Data/fstfrz_usgs/w001001.adf"))# UTDEQ version
MEANP_PIBO_WS.ras<-raster(paste0(wd,"/Climate/Data/meanppt_pibo.tif"))# UTDEQ version)


## Atmosphere !!!
AtmCa.ras<-raster(paste0(wd,"/Atmos/Data/atm_ca/w001001.adf"))
AtmSO4.ras<-raster(paste0(wd,"/Atmos/Data/atm_so4/w001001.adf"))
AtmNa.ras<-raster(paste0(wd,"/Atmos/Data/atm_na/w001001.adf"))
AtmNO3.ras<-raster(paste0(wd,"/Atmos/Data/atm_no3/w001001.adf"))


Eco3_PT.vec<-st_read(paste0(wd,"/Ecoregions/Data/Eco_Level_III_US.shp"))
Eco4_PT.vec<-st_read(paste0(wd,"/Ecoregions/Data/us_eco_l4_no_st.shp"))
Vol_ave.ras<-raster(paste0(wd,"/Geology/Data/vol/w001001.adf"))
alru_dom.ras<-raster(paste0(wd,"/Vegetation/Data/alru_domrec/w001001.adf"))
Evergr_ave.ras<-raster(paste0(wd,"/Vegetation/Data/evergr/w001001.adf"))
EVI_AveAve.ras<-raster(paste0(wd,"/Vegetation/Data/evi_ave/w001001.adf"))
EVI_MAX_AVE.ras<-raster(paste0(wd,"/Vegetation/Data/evi_max_10b.tif"))# UTDEQ version
CaO_Mean.ras<-raster(paste0(wd,"/Geology/Data/cao_19jan10/w001001.adf"))
TP_Mean.ras<-raster(paste0(wd,"/Geology/Data/p_19jan10/w001001.adf"))
AWC_soil.ras<-raster(paste0(wd,"/Soils/Data/awc/w001001.adf"))
GW_P_Sp_Mx.ras<-raster(paste0(wd,"/Hydro/Data/gw_p_sp/w001001.adf"))
SOC.ras<-raster(paste0(wd,"/Soils/Data/soc/w001001.adf"))
Pct_Alfi.ras<-raster(paste0(wd,"/Soils/Data/alfi_nonulls/w001001.adf"))
Wb_mx_area.vec<-st_read(paste0(wd,"/Metrics/Data/Wb.shp"))
Kfact.ras<-raster(paste0(wd,"/Soils/Data/kfact_usgs/w001001.adf"))
Db3rdbar.ras<-raster(paste0(wd,"/Soils/Data/db3rdbar/w001001.adf"))

## Additional CO model
LOG_XP_PT.ras<-raster(paste0(wd,"/Metrics/Colorado/data/meanppt/w001001.adf"))





config <- list(
  pred_geometry_base_path = "Z:/GIS/GIS_Stats/",
 
   
  
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
  
  ## Ecoregion
  insert predictor name here = "WY_Model/BIOREGIONS_2011_modifiedCP.shp",
  
  ## Metrics
  PRMH_AVE  = "Soils/Data/permh_usgs/w001001.adf",
  CaO_Mean  = "Geology/Data/cao_19jan10/w001001.adf",
  MgO_Mean  = "Geology/Data/Mgo_19jan10/w001001.adf",
  S_Mean  = "Geology/Data/s_23aug10/w001001.adf",
  UCS_Mean  = "Geology/Data/ucs_19jan10/w001001.adf",
  BDH_AVE  = "Soils/Data/bdh_usgs/w001001.adf",
  KFCT_AVE  = "Soils/Data/kfact_usgs/w001001.adf",
)

