sites=read.csv("C:/Users/jenni/Box/NAMC (Trip Armstrong)/Research Projects/AIM/P_Hab Modeling/Final analyses/modeling/sitesfordrainageden.csv")
sites=read.csv("C:/Users/jenni/Box/NAMC (Trip Armstrong)/Research Projects/AIM/P_Hab Modeling/Final analyses/modeling/AIM_2022_2023sites_to_add_to_main_dataset.csv")

def_sites=sites=sf::st_as_sf(sites,coords=c('longitude','latitude'),crs=4269, agr="constant")

lengths=list()
for (s in 1:nrow(def_sites)){
  tryCatch({lengths[[s]]=drainage_density(SQLite_file_path,COMIDs=def_sites$COMID[s])
}, error = function(e) {
  cat(paste0("\n\tERROR calculating: ",def_sites$siteId[s],"\n"))
  str(e,indent.str = "   "); cat("\n")
})
}
  lengthsdf=unlist(as.data.frame(cbind(lengths)))






lengths=list()
for (s in 1:nrow(def_sites)){

  lengths[[s]]=length_46003(point2process=def_sites[s,'geometry'],geometry_input_path="C:/Users/jenni/Box/NAMC (Trip Armstrong)/GIS/GIS_Stats/CONUS/streams/NHD_West_str_ord.shp")
}

lengthsdf=unlist(as.data.frame(cbind(lengths)))


lengths2=list()
for (s in 1:nrow(def_sites)){

  lengths2[[s]]=length_46006(point2process=def_sites[s,'geometry'],geometry_input_path="C:/Users/jenni/Box/NAMC (Trip Armstrong)/GIS/GIS_Stats/CONUS/streams/NHD_West_str_ord.shp")
}

lengths2df=as.data.frame(cbind(lengths2))

lengths3=list()
for (s in 1:nrow(def_sites)){

  lengths3[[s]]=sum_lengthKM(point2process=def_sites[s,'geometry'],geometry_input_path="C:/Users/jenni/Box/NAMC (Trip Armstrong)/GIS/GIS_Stats/CONUS/streams/NHD_West_str_ord.shp")
}

lengths3df=as.data.frame(cbind(lengths3))

dd2=cbind(lengthsdf,lengths2df,lengths3df)
dd3=cbind(lengthsdf,unlist(lengths2df),unlist(lengths3df))

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

  lengths4[[s]]=length_A1_3(point2process=def_sites[s,'geometry'],geometry_input_path="C:/Users/jenni/Box/NAMC (Trip Armstrong)/GIS/GIS_Stats/CONUS/streams/NHD_West_str_ord_meritt.shp")
}

lengths3df=as.data.frame(cbind(lengths3))

lengths3=list()
for (s in 1:nrow(def_sites)){
  tryCatch({lengths3[[s]]=NHDSLOPE(point2process=def_sites[s,'geometry'],geometry_input_path="C:/Users/jenni/Box/NAMC (Trip Armstrong)/GIS/GIS_Stats/CONUS/streams/NHD_West_str_ord.shp")
  }, error = function(e) {
    cat(paste0("\n\tERROR calculating: ",def_sites$siteId[s],"\n"))
    str(e,indent.str = "   "); cat("\n")
  } )
}


lengths=list()
for (s in 1:nrow(def_sites)){
