MS<-st_read(watershed_file_path)
MS<-MS[MS$siteId %in% atm$siteId,] #change this to be the appropriate list of sites!

SumAveP<-terra::rast("C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC WATS Department Files//GIS//GIS_Stats//CONUS//precipitation//sumave_p//sumave_p2.tif")
SumAveP<-terra::project(SumAveP,'EPSG:5070')

df<-data.frame(siteId=atm$siteId,
               predictorId=53,
               value=NA)
for(i in 1:nrow(MS)){
  oop<-terra::vect(MS[i,])
  R<-terra::crop(SumAveP,oop)
  mean(R)
  sumavep<-terra::global(R,fun='mean')$mean
  df$value[i]<-sumavep
  message(sumavep)
}



for(i in 1:nrow(df)){
NAMCr::save(
  api_endpoint = "setSitePredictorValue",
  siteId = df$siteId[i],
  predictorId = df$predictorId[i],
  value = df$value[i]
)
}
