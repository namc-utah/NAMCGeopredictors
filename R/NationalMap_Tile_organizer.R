tiles<-read.csv('C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC WATS Department Files//GIS//DEMs//NationaMap//AZ//AZ_tiles.csv')

terra_tiles<-list()
for(i in 1:nrow(tiles)){
  message(paste(c('starting iteration ',i)))
  terra_tiles[[i]]<-terra::rast(tiles$URL[i])
}
s<-terra::sprc(terra_tiles)
m<-terra::merge(s)

terra::writeRaster(m,'C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC WATS Department Files//GIS//DEMs//NationaMap//AZ//AZ_Statewide_DEM.tif')

library(terra)
AZs<-terra::rast('C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC WATS Department Files//GIS//DEMs//NationaMap//AZ//AZ_Statewide_DEM.tif')



terra::plot(Azs)
#essentially a conditionally formatted raster
#from the original. (this example is cold vs warm in AZ)
d<-AZs > 1524
terra::plot(d,legend=F,col=c('firebrick3','dodgerblue4'))
add_legend('bottomleft',
           leg=c('Warm','Cold'),
           pch=22,
           pt.bg=c('firebrick3','dodgerblue4'),
           bty='n')
terra::writeRaster(d,'C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC//OEModeling//NAMC_Supported_OEmodels//Arizona//AZ_ColdWarm_DEM.tif')

IDW_AZ<-terra::rast("C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC WATS Department Files//GIS//GIS_Stats//AZ_IDW_elevcat.tif")

d_idw<-IDW_AZ>1524
terra::writeRaster(d_idw,'C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC//OEModeling//NAMC_Supported_OEmodels//Arizona//AZ_IDW_Coldwarm_DEM.tif')
par(mfrow=c(2,1))
cropped_d<-terra::crop(d,d_idw,mask=T)
terra::plot(cropped_d,col=c('firebrick3','dodgerblue4'))
terra::plot(d_idw,col=c('firebrick3','dodgerblue4'))

ext(cropped_d)<-terra::align(ext(d_idw),cropped_d)
mask(cropped_d,d_idw)
ext(cropped_d)
ext(d_idw)
