MS<-sf::st_read(paste0(watershed_file_path))


siteIds=c(23180,
          23189,
          23175,
          23187,
          46578,
          46581,
          46583,
          23185)
ok<-NAMCr::query('sites',siteIds=siteIds)

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


#plot each watershed iteratively to inspect how it looks
#if the raster is largely one color, or contains a lot of
#conflicting directions, the flow direction will be
#essentially 0, so we will set those aside and assign them to be 0
#for the predictor Slope_WS
graphics.off()
s<-8
terra::plot(sheddy_list[[s]],
            main=as.character(lilMS$siteId[s]))

bad_sites<-c(43242,43261,43260,43262)
#subset only the sites that have good flow directions and run those
#sites in the Slope_Ws script.
#Once the Slope_WS script calculates the slope for the good sheds,
#append the rest of the failed sites and assign 0 to those manually
#then, save the results for all sites to the database.

good_sites<-siteIds[siteIds %in% bad_sites==F]

do.call(rbind,sheddy_list)
terra::plot(sheddy_list[[31]])
lilMS$siteId[c(11,14,21,23,26,27,28,31)]
mapview::mapview(C,map.type)

