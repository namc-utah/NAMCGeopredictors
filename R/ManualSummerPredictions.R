SUpath<-"C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC WATS Department Files//GIS//GIS_Stats//Colorado//temperature//summer_temp_COMID_intersect.shp"
SUSU<-function(point2process,predictor_geometry,geometry_input_path, ...){
  AOItrans<- sf::st_transform(point2process, 5070) # must use the same EPSG as in the shapefile
  AOItrans_wkt <- AOItrans %>%
    sf::st_geometry() %>% # convert to sfc
    sf::st_buffer(150) %>% # buffer 150 meters
    sf::st_as_text() # convert to well known text
  SUMMER.vec<-sf::st_read(geometry_input_path, wkt_filter = AOItrans_wkt)
  AOI_Buffer<-sf::st_join(AOItrans, SUMMER.vec, join = nngeo::st_nn, maxdist = 500, k = 1, progress = FALSE)
  media<-AOI_Buffer$summer
  return(media)
}
summer<-sf::st_read("C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC WATS Department Files//GIS//GIS_Stats//Colorado//temperature//summer_temp_COMID_intersect.shp")
samps<-NAMCr::query('samples',boxId=10030)
sites<-NAMCr::query('sites',boxIds=10030)

all_dat<-plyr::join(samps,sites[,c('siteId','waterbodyCode')])

lildf<-data.frame(sampleId=all_dat$sampleId,
                  siteId=all_dat$siteId,
                  summer=NA,
                  predictorId=112)

for(i in 1:nrow(all_dat)){
  tryCatch({
X<-all_dat[i,]
point2process=sf::st_as_sf(X,coords=c('siteLongitude','siteLatitude'),crs=4269)


lildf$summer[i]<-SUSU(point2process = point2process,predictor_geometry = summer,geometry_input_path = SUpath)
message(paste(c(i,' of ',nrow(all_dat))))
},error=function(cond){
  message('Error! Geometry with WKT was empty!')
  message('Points with a summer value of NA should be inspected!')
})
}

lildf

plot(summer)
