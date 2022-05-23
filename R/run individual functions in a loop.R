def_samples = NAMCr::query(
  api_endpoint = "samples",
  include = c("sampleId", "siteId", "sampleDate"),
  projectId=387

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


lengths=list()
for (s in 1:nrow(def_sites)){
