HUCs<- st_read("/Users/alexhernandez/Desktop/BUG_BLM/ZonalTest/Vectors/HUC4_All.shp")

## And now extract its crs
crs2use<-crs(HUCs)



arem.kmz<-st_read("/Users/alexhernandez/Desktop/BUG_BLM/ZonalTest/Vectors/CAL_ws_4June2014.kml")
arem.kmz<-st_transform(arem.kmz, crs2use)
arem.kmz<-arem.kmz[,1]
arem.kmz.valid<-st_make_valid(arem.kmz)

arem.kmz.valid2<-st_transform(arem.kmz.valid, crs = 4326)

arem.kmz.valid.json<-geojson_json(arem.kmz.valid2)
arem.kmz.valid.json.simp<-ms_simplify(arem.kmz.valid.json)
arem.kmz.valid3<- geojson_sf(arem.kmz.valid.json.simp)

geojson_write(arem.kmz.valid.json.simp, file = "/Users/alexhernandez/Desktop/NAMC_Borrar/aremtestsimplified.geojson")

putin<-st_read("/Users/alexhernandez/Desktop/NAMC_Borrar/aremtestsimplified.geojson")
#putin<-polygon2process
