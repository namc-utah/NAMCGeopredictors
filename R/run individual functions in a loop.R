def_samples = NAMCr::query(
  api_endpoint = "samples",
  include = c("sampleId", "siteId", "sampleDate"),
  projectId=49

)

siteIds=unlist(unique(def_samples$siteId))
def_sites=list()
# for each site in def_predictors get site coordinates and comid from database
# store as a list of lists referenced by "x" plus the siteId
for (t in 1:length(siteIds)){
  if(t==1){
    def_sites= unlist(NAMCr::query(
      api_endpoint = "siteInfo",
      include = c("siteId", "siteName", "usState", "location","waterbodyCode"),
      siteId = siteIds[t]
    ))
  } else {def_sites1= unlist(NAMCr::query(
    api_endpoint = "siteInfo",
    include = c("siteId", "siteName", "usState", "location","waterbodyCode"),
    siteId = siteIds[t]
  ))
  def_sites=as.data.frame(rbind(def_sites,def_sites1))
  }

}
point2process=sf::st_read("C:/Users/jenni/Box/NAMC (Trip Armstrong)/Research Projects/AIM/P_Hab Modeling/Final analyses/modeling/allsites.shp")
point2process=sf::st_transform(point2process,crs=5070)
polygon2process=sf::st_buffer(point2process,1250)
polygon2process=subset(polygon2process,fines_scre!='R')
range=list()



for (s in 1:nrow(polygon2process)){
range[[s]]=VALLEY_ELEV_RANGE(polygon2process[s,])
print(s)
}

bslope=list()
for (s in 6221:nrow(polygon2process)){
  bslope[[s]]=slpavg(polygon2process[s,])
  print(s)
}


def_sites=read.csv("C:/Users/jenni/Box/NAMC (Trip Armstrong)/Research Projects/AIM/P_Hab Modeling/Final analyses/modeling/Final AIM results for NOC/AIM_2022_2023_PHAB_pred.csv")
def_sites=sf::st_as_sf(def_sites,coords=c('long','lat'),crs=4269)
lengths3=list()
for (s in 1:nrow(def_sites)){

  lengths3[[s]]=sum_lengthKM(point2process=def_sites[s,],geometry_input_path="C:/Users/jenni/Box/NAMC (Trip Armstrong)/GIS/GIS_Stats/CONUS/streams/NHD_West_str_ord.shp")
}

lengths=list()
for (s in 1:nrow(def_sites)){

  lengths[[s]]=pct_46003(point2process=geojsonsf::geojson_sf(def_sites$location[s]),geometry_input_path="C:/Users/jenni/Box/NAMC (Trip Armstrong)/GIS/GIS_Stats/CONUS/streams/NHD_West_str_ord.shp")
}

lengthsdf=unlist(as.data.frame(cbind(lengths)))


lengths2=list()
for (s in 1:nrow(def_sites)){

  lengths2[[s]]=length_46006(point2process=geojsonsf::geojson_sf(def_sites$location[s]),geometry_input_path="C:/Users/jenni/Box/NAMC (Trip Armstrong)/GIS/GIS_Stats/CONUS/streams/NHD_West_str_ord.shp")
}

lengths2df=as.data.frame(cbind(lengths2))

lengths3=list()
for (s in 1:nrow(def_sites)){

  lengths3[[s]]=sum_lengthKM(point2process=geojsonsf::geojson_sf(def_sites$location[s]),geometry_input_path="C:/Users/jenni/Box/NAMC (Trip Armstrong)/GIS/GIS_Stats/CONUS/streams/NHD_West_str_ord.shp")
}

lengths3df=as.data.frame(cbind(lengths3))

dd2=cbind(lengthsdf,lengths2df,lengths3df)


dd2=cbind(lengthsdf,lengths2df,lengths3df)
def_sites=cbind(def_sites,lengthsdf)
def_sites$siteId=as.numeric(def_sites$siteId)
join=left_join(def_samples,def_sites,by='siteId')
join$length_46003=unlist(join$lengths)
join$length_46006=unlist(join$lengths2)
join$SLOPE=unlist(join$lengths3)

join=join[,c("sampleId","length_46003","length_46006","sum_lengthKM")]
write.csv(join,'density_A1.csv')

lengths4=list()
for (s in 1:nrow(def_sites)){

  lengths4[[s]]=length_A1_3(point2process=geojsonsf::geojson_sf(def_sites$location[s]),geometry_input_path="C:/Users/jenni/Box/NAMC (Trip Armstrong)/GIS/GIS_Stats/CONUS/streams/NHD_West_str_ord_meritt.shp")
}

lengths3df=as.data.frame(cbind(lengths3))

lengths3=list()
for (s in 1:nrow(def_sites)){
tryCatch({lengths3[[s]]=NHDSLOPE(point2process=geojsonsf::geojson_sf(def_sites$location[s]),geometry_input_path="C:/Users/jenni/Box/NAMC (Trip Armstrong)/GIS/GIS_Stats/CONUS/streams/NHD_West_str_ord.shp")
}, error = function(e) {
  cat(paste0("\n\tERROR calculating: ",def_sites$siteId[s],"\n"))
  str(e,indent.str = "   "); cat("\n")
} )
}
DEM_trashbin=tempdir()

polygon2process=sf::st_make_valid(sf::st_read("C:/Users/jenni/Box/NAMC (Trip Armstrong)/Research Projects/AIM/P_Hab Modeling/Final analyses/modeling/watershedsforslope.shp"))
slopel=list()
for (s in 1:nrows(polygon2process)){

  tryCatch({slopel[[s]]=slpavg(polygon2process[s,])
  #unlink(paste0(DEM_trashbin,'/*'))

  }, error = function(e) {
    cat(paste0("\n\tERROR calculating: ",polygon2process[s,],"\n"))
    str(e,indent.str = "   "); cat("\n")
  } )

}
lengths3df=as.data.frame(unlist(as.data.frame(cbind(slopel))))

slopefinal=as.data.frame(cbind(lengths3df,polygon2process$siteId[c(1331:1333,1656,1723:1724,1777:1792)]))
write.csv(slopefinal,"slopepart9.csv")


polygon2process=def_watersheds
wsarea=list()
for (s in 1:nrow(polygon2process)){

  tryCatch({wsarea[[s]]=WSA_SQKM(polygon2process[s,])
  #unlink(paste0(DEM_trashbin,'/*'))

  }, error = function(e) {
    cat(paste0("\n\tERROR calculating: ",polygon2process[s,],"\n"))
    str(e,indent.str = "   "); cat("\n")
  } )

}
lengths3df=as.data.frame(unlist(as.data.frame(cbind(wsarea))))
wsareafinal=as.data.frame(cbind(polygon2process$siteId,wsarea))
