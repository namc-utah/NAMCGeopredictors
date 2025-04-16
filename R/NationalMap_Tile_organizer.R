tiles<-read.csv('C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC WATS Department Files//GIS//DEMs//NationaMap//AZ//AZ_tiles.csv')

terra_tiles<-list()
for(i in 1:nrow(tiles)){
  message(paste(c('starting iteration ',i)))
  terra_tiles[[i]]<-terra::rast(tiles$URL[i])
}
s<-terra::sprc(terra_tiles)
m<-terra::merge(s)

terra::writeRaster(m,'C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC WATS Department Files//GIS//DEMs//NationaMap//AZ//AZ_Statewide_DEM.tif')
