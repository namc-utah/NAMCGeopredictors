library(sf)
library(dplyr)
#setwd("/Users/alexhernandez/Desktop/BUG_BLM/Temps/Alrus")
library(here)
setwd("/Users/alexhernandez/Desktop/BUG_BLM/ZonalTest")
set_here("/Users/alexhernandez/Desktop/BUG_BLM/ZonalTest/GIS_Stats01")
library(mapview)
library(data.table)
library()

### Read in CO features and just keeping identifiers and sample date
CO.Wats<-st_read(here("CO/COEDAS/MineralCrSheds.shp"))
CO.Points<-st_read(here("CO/COEDAS/MineralCrPts.shp"))
myvars<-c("sampleid","sampdate")
CO.Wats<-CO.Wats[myvars]
CO.Points<-CO.Points[myvars]
####### To GeoJson
CO.Wats.WGS<-st_transform(CO.Wats, crs = 4326)
CO.Wats.WGS.json<-geojson_json(CO.Wats.WGS)
#AIM2020.WGS.json.simp<-ms_simplify(AIM2020.WGS.json)
CO.Wats.WGS.json.simpkeep<-ms_simplify(CO.Wats.WGS.json,keep_shapes=TRUE) # this makes sure to keep all 



#### variables CO model #######
CO.model<-c("ECO3","ECO4","ELEV_SITE")

Nested.List<-list()
# Start the clock!
#ptm <- proc.time()
for (m in CO.model){
  vari<-paste('item:',m,sep='')
  print(paste0("Now processing variable:...",vari))
  tmp<-list()
  for (i in 1:5){
    geobjecto<-CO.Points[i,]
    geobjecto.WGS<-st_transform(geobjecto, crs = 4326)
    geobjecto.WGS.json<-geojson_json(geobjecto.WGS)
    #touse<-ms_simplify(geobjecto.WGS.json,keep_shapes=TRUE)
    touse<-geobjecto.WGS.json # for points
    function2use<-eval(parse(text = m))
    #print(str(function2use))
    a<-function2use(touse)
    #a<-pull(a) # Must pull when working with GEE assets
    #print(a)
    tmp[[i]]<-a
  }
  Nested.List[[m]]<-unlist(tmp)
  
}

as.data.frame(do.call(cbind,Nested.List)) ### This one!


do.call(rbind,unlist(Nested.List, recursive=FALSE)) ### This one!


unlist(Nested.List, recursive=TRUE,use.names = TRUE)
rbindlist(Nested.List)
ttc<-as.data.frame(do.call(cbind, Nested.List))
#ttc2<-do.call(rbind, lapply(Nested.List, data.frame))
ttc3<-as.data.frame(data.table::rbindlist(Nested.List, idcol = TRUE))


data.table::rbindlist(Nested.List, idcol = TRUE)

mybiglist <- list()
for(i in 1:5){
  a <- runif(10)
  b <- rnorm(16)
  c <- rbinom(8, 5, i/10)
  name <- paste('item:',i,sep='')
  tmp <- list(uniform=a, normal=b, binomial=c)
  mybiglist[[name]] <- tmp
}
ttc<-do.call(rbind, mybiglist)
ttc2<-unlist(mybiglist)

  
    tryCatch({
    ii<-AIMCatch[i,]
    print(ii)
    ttcobjecto<-NAMCr::query("siteInfo", siteId=ii)
    geobjecto<-ttcobjecto[["catchment"]] # Extract the geojson object from the list
    siteident<-ttcobjecto[["siteName"]] # Extract the site identifier
    sfttc<-geojson_sf(geobjecto) # convert the geojson to sf object
    sfttc$nombre<-NA 
    sfttc$nombre<-siteident # add the watershed identifier as an attribute to the sf object
    sfttc$siteID<-ii
    pirin[[i]]<-sfttc # fill the empty list
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
# Stop the clock
proc.time() - ptm

sftotal<-do.call(rbind, pirin) # get the list components and turn them into a full sf object