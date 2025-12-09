MS<-sf::st_read(paste0(watershed_file_path))

MS<-MS[,c(1,3)]
lilMS<-MS[MS$siteId %in% siteIds,]

#test<-terra::rast(elevatr::get_elev_raster(lilMS,z=13))

#ok<-terra::crop(test,terra::vect(lilMS),mask=T)
#terra::plot(ok)

#range(ok)
#mapview::mapview(ok)


flowdir<- 'C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC WATS Department Files//GIS//GIS_Stats//Nevada//elevation//NVFLD8.tif'
FL<-terra::rast(flowdir)
sheddy_list<-list()
for(i in 1:nrow(lilMS)){
  xx<-lilMS[i,]
  C<-terra::crop(FL,terra::vect(xx),mask=T)
  #C$siteId<-lilMS$siteId[i]
  sheddy_list[[i]]<-C
}
do.call(rbind,sheddy_list)
terra::plot(sheddy_list[[31]])
lilMS$siteId[c(11,14,21,23,26,27,28,31)]
mapview::mapview(C,map.type)
