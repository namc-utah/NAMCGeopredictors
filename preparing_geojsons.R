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

AIM2020<-st_read("C://Temp//AIM2020//All2020Sheds.shp")
AIM2020.WGS<-st_transform(AIMP2020, crs = 4326)
AIM2020.WGS.json<-geojson_json(AIMP2020.WGS)
AIM2020.WGS.json.simp<-ms_simplify(AIMP2020.WGS.json)
# sf object to use
sampleaim<-geojson_sf(AIM2020.WGS.json.simp)


AIMP2020.points<-st_read("C://Temp//AIM2020//All2020Points.shp")
AIM2020.WGS.points<-st_transform(AIM2020.points, crs = 4326)
AIM2020.WGS.json.points<-geojson_json(AIMP2020.WGS.points)
AIM2020.WGS.json.simp.points<-ms_simplify(AIM0.WGS.json.points)


