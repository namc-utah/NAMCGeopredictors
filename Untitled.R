## EcoRegion !!!
Eco4_PT.vec<-st_read(here("GIS_Stats01/Ecoregion/Data","us_eco_l4_no_st.shp"))



LOG_XP_PT.ras<-raster(here("GIS_Stats01/Metrics/Colorado/Data/meanppt","w001001.adf"))


Pred_Input_All_USGS.vec<-st_read(here("GIS_Stats01/Metrics/Oregon/Data","Pred_Input_All_USGS.shp"))
SQRT_TOPO.vec<-st_read(here("GIS_Stats01/Metrics/Colorado/Data","SQRT_TOPO6703.shp"))

#SUMMER.vec<-st_read(here("GIS_Stats01/Metrics/Colorado/Data","summer.shp"))
SUMMER.vec<-st_read(here("GIS_Stats01/Metrics/Colorado/Data","summer_6703.shp"))
