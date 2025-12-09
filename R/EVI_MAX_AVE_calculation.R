#EVI

EVI_rast<-terra::rast('C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC WATS Department Files//GIS/GIS_Stats/CONUS/vegetation/evi_max_10B.tif')
sheds<-terra::vect(def_watersheds)

EVI_crs<-crs(EVI_rast)

sheds_trans<-terra::project(sheds,EVI_crs)
EVI_zonal<-terra::zonal(EVI_rast, sheds_trans,fun='mean',na.rm=T)
EVI_zonal$siteId=sheds$siteId
EVI_zonal

for (i in 1:nrow(EVI_zonal)){
      NAMCr::save(
        api_endpoint = "setSitePredictorValue",
        siteId = EVI_zonal$siteId[i],
        predictorId = 58,
        value = EVI_zonal$evi_max_10B[i]
      )
      message ('saved predictor!')
    }
