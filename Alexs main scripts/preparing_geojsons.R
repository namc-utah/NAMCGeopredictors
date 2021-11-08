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

############# Simulating a geojson #########

AREMP2020<-st_read("C://Temp//AREMP//AREMPwatersheds.shp")
AREMP2020.WGS<-st_transform(AREMP2020, crs = 4326)
AREMP2020.WGS.json<-geojson_json(AREMP2020.WGS)
AREMP2020.WGS.json.simp<-ms_simplify(AREMP2020.WGS.json)
# sf object to use
samplearemp<-geojson_sf(AREMP2020.WGS.json.simp)


AREMP2020.points<-st_read("C://Temp//AREMP//AREMPpoints.shp")
AREMP2020.WGS.points<-st_transform(AREMP2020.points, crs = 4326)
AREMP2020.WGS.json.points<-geojson_json(AREMP2020.WGS.points)
AREMP2020.WGS.json.simp.points<-ms_simplify(AREMP2020.WGS.json.points)

############# Simulating a geojson AIM2020 #########

AIM2020<-st_read("C://Temp//AIM2020//All2020Sheds.shp")# line @ NAMC server
AIM2020<-st_read("/Users/alexhernandez/Desktop/BUG_BLM/Temps/AIM2020/All2020Sheds.shp")# line @ Alex
AIM2020<-AIM2020[,2]
AIM2020.WGS<-st_transform(AIM2020, crs = 4326)
AIM2020.WGS.json<-geojson_json(AIM2020.WGS)
AIM2020.WGS.json.simp<-ms_simplify(AIM2020.WGS.json)
AIM2020.WGS.json.simpkeep<-ms_simplify(AIM2020.WGS.json,keep_shapes=TRUE) # this makes sure to keep all 352
AIM2020.WGS.json025.simp<-ms_simplify(AIM2020.WGS.json, keep = 0.25)
AIM2020.WGS.json035.simp<-ms_simplify(AIM2020.WGS.json, keep = 0.35)
AIM2020.WGS.json050.simp<-ms_simplify(AIM2020.WGS.json, keep = 0.5, keep_shapes=TRUE)
AIM2020.WGS.json075.simp<-ms_simplify(AIM2020.WGS.json, keep = 0.75, keep_shapes=TRUE)
# sf object to use
sampleaim<-geojson_sf(AIM2020.WGS.json.simp)


AIMP2020.points<-st_read("C://Temp//AIM2020//All2020Points.shp")
AIM2020.WGS.points<-st_transform(AIMP2020.points, crs = 4326)
AIM2020.WGS.json.points<-geojson_json(AIM2020.WGS.points)
#AIM2020.WGS.json.simp.points<-ms_simplify(AIM2020.WGS.json.points)


############# Simulating a geojson PIBO2020 #########

PIBO2020<-st_read("C://Temp//PIBO//PIBOsheds.shp")
length(unique(PIBO2020$Sample_ID))
PIBO2020<-PIBO2020[,13]
PIBO2020.WGS<-st_transform(PIBO2020, crs = 4326)
PIBO2020.WGS.json<-geojson_json(PIBO2020.WGS)
#AIM2020.WGS.json.simp<-ms_simplify(AIM2020.WGS.json)
PIBO2020.WGS.json.simpkeep<-ms_simplify(PIBO2020.WGS.json,keep_shapes=TRUE) # this makes sure to keep all 
# AIM2020.WGS.json025.simp<-ms_simplify(AIM2020.WGS.json, keep = 0.25)
# AIM2020.WGS.json035.simp<-ms_simplify(AIM2020.WGS.json, keep = 0.35)
# AIM2020.WGS.json050.simp<-ms_simplify(AIM2020.WGS.json, keep = 0.5, keep_shapes=TRUE)
# AIM2020.WGS.json075.simp<-ms_simplify(AIM2020.WGS.json, keep = 0.75, keep_shapes=TRUE)
# sf object to use
sampleaim<-geojson_sf(AIM2020.WGS.json.simp)


PIBO2020.points<-st_read("C://Temp//PIBO//PIBOshedPts.shp")
PIBO2020.WGS.points<-st_transform(PIBO2020.points, crs = 4326)
PIBO2020.WGS.json.points<-geojson_json(PIBO2020.WGS.points)
#AIM2020.WGS.json.simp.points<-ms_simplify(AIM2020.WGS.json.points)
