#read in the mastersheds file
MS<-sf::st_read(paste0(watershed_file_path))

ok<-NAMCr::query('sites',siteIds=siteIds)


#subset the MS file to just the siteIds in question (box or project)
lilMS<-MS[MS$siteId %in% ok$siteId,]
#read in the flow direction raster for Nevada
flowdir<- 'C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC WATS Department Files//GIS//GIS_Stats//Nevada//elevation//NVFLD8.tif'
FL<-terra::rast(flowdir)
#create an empty list
sheddy_list<-list()
lilMS<-sf::st_transform(lilMS,terra::crs(FL))
#fill that list with the clipped rasters.
for(i in 1:nrow(lilMS)){
  xx<-lilMS[i,]
  C<-terra::crop(FL,xx,mask=T)
  #C$siteId<-lilMS$siteId[i]
  sheddy_list[[i]]<-C
}

#plot each watershed iteratively to inspect how it looks
#if the raster is largely one color, or contains a lot of
#conflicting directions, the flow direction will be
#essentially 0, so we will set those aside and assign them to be 0
#for the predictor Slope_WS
graphics.off()
s<-70
terra::plot(sheddy_list[[s]],
            main=as.character(lilMS$siteId[s]))


#subset only the sites that have good flow directions and run those
#sites in the Slope_Ws script.
#Once the Slope_WS script calculates the slope for the good sheds,
#append the rest of the failed sites and assign 0 to those manually
#then, save the results for all sites to the database.

lilMS$siteId[c(118,115,113,111,101,72,71,70,69,68,67,66,58,56,40,9,21)]
lilMS[lilMS$siteId==30661,]
