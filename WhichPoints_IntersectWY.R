library(GADMTools)

USA<-gadm_sp_loadCountries("USA", level = 2, basefile = "/Users/alexhernandez/Desktop/AME/GADM")
WY.State<-gadm_subset(USA, level = 1, regions = c("Wyoming"))
gadm_plot(WY.State)

WY.State.sf<-st_as_sf(WY.State$spdf)

# Which points intersect WY

points2use4WY<-st_intersection(sftotalpoint, WY.State.sf)
mapview(points2use4WY)+mapview(WY.State.sf)

siteIDWY<-points2use4WY$siteID
