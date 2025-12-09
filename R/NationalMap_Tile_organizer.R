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
mappy<-terra::plot(d,legend=F,col=c('firebrick3','dodgerblue4'))
add_legend('bottomleft',
           leg=c('Warm','Cold'),
           pch=22,
           pt.bg=c('firebrick3','dodgerblue4'),
           bty='n')
terra::writeRaster(d,'C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC//OEModeling//NAMC_Supported_OEmodels//Arizona//AZ_ColdWarm_DEM.tif')
