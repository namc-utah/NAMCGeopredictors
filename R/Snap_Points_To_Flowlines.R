#Load required packages
library(NAMCr)#used to retrieve data from NAMC database
library(sf)
library(mapview)
library(nhdplusTools)
library(raster)
library(dplyr)

##Snap_Points_to_Lines_Tool
#This is the function that snaps the points to the flowlines
st_snap_points <- function(x, y, namevar, max_dist = 1000) {

  # this evaluates the length of the data
  if (inherits(x, "sf")) n = nrow(x)
  if (inherits(x, "sfc")) n = length(x)

  # this part:
  # 1. loops through every piece of data (every point)
  # 2. snaps a point to the nearest line geometries
  # 3. calculates the distance from point to line geometries
  # 4. retains only the shortest distances and generates a point at that intersection
  out = do.call(c,
                lapply(seq(n), function(i) {
                  nrst = st_nearest_points(st_geometry(x)[i], y)
                  nrst_len = st_length(nrst)
                  nrst_mn = which.min(nrst_len)
                  if (as.vector(nrst_len[nrst_mn]) > max_dist) return(st_geometry(x)[i])
                  return(st_cast(nrst[nrst_mn], "POINT")[2])
                })
  )
  # this part converts the data to a dataframe and adds a named column of your choice
  out_xy <- st_coordinates(out) %>% as.data.frame()
  out_xy <- out_xy %>%
    mutate({{namevar}} := x[[namevar]]) %>%
    st_as_sf(coords=c("X","Y"), crs=st_crs(x), remove=FALSE)

  return(out_xy)
}

#Retrieve data from NAMC database using the NAMCr package
data = query("samples", boxId = 4981)
View(data) #View the data
multipoint_matrix<-as.matrix(data[c("sampleLatitude","sampleLongitude")]) #make a matrix of the subset of lat/long
start_point <- sf::st_sfc(sf::st_multipoint(multipoint_matrix), crs = 4269)#transform matrix to geospatial object
#mapview(start_point) #map the points

#Load the nhd flowlines shapefile from NAMC Box folder
##Andrew work
nhd <- st_read("C://Users//andrew.caudillo//Box//NAMC//GIS//GIS_Stats//CONUS//streams//NHD_West_str_ord.shp")
#nhd <- st_read("~/Library/CloudStorage/Box-Box/NAMC/GIS/GIS_Stats/CONUS/streams/NHD_West_str_ord.shp")
#or_strgrid <- raster("~/Desktop/or_strgrid/str100/") #this loads the raster grids from streamstats...but I'm not sure how to snap the points to these grids yet

usa = getData(country = "USA", level = 1)#get geospatial data for the
OR <- subset(usa, NAME_1 == "Oregon")
st_crs(OR) #get crs for state layer
st_crs(nhd) #get crs for nhd layer
st_crs(OR)==st_crs(nhd)
nhd_proj <- st_transform(nhd, crs = crs(OR))#transform nhd to crs of the state layer so they are the same. original was "4326"
or = st_as_sf(OR)#tranmsforms state layer to sf object
rios = st_intersection(nhd_proj, or)#intersect state layer with nhd layer so that you are only dealing with nhd flowlines for that state

df <- st_as_sf(x = data, coords = c("sampleLongitude", "sampleLatitude"), crs = crs(OR)) #original was "4326"

points_snapped <- st_snap_points(df, rios, namevar = "siteId", max_dist = 500) #namevar should be siteId

setwd('C://Users//andrew.caudillo//Box//NAMC//Operations//Sample_submission_info//COMIDs_for_submission_append')
write.csv(points_snapped,'4981_snapped.csv')

#st_write(points_snapped, "OR_pts_snapped.shp") #create shapefile of snapped points

##This section is for after you receive output from streamstats
st_read("~/Desktop/or_pts_snapped133023796061665868/or_pts_snapped133023796061665868.gdb")#read in geodatabase
oregon_layers <- st_layers(dsn = "~/Desktop/or_pts_snapped133023796061665868/or_pts_snapped133023796061665868.gdb")#identify the different layers in the geodatabase
oregon_layers
flow <- st_read("~/Desktop/or_pts_snapped133023796061665868/or_pts_snapped133023796061665868.gdb", layer = "FLOWSTATS")
shed <- st_read("~/Desktop/or_pts_snapped133023796061665868/or_pts_snapped133023796061665868.gdb", layer = "GlobalWatershed")
point <- st_read("~/Desktop/or_pts_snapped133023796061665868/or_pts_snapped133023796061665868.gdb", layer = "GlobalWatershedPoint")

points <- st_as_sf(point$Shape)
points$siteId <- points_snapped$siteId

sheds <- st_as_sf(shed$Shape)
sheds$siteId = points_snapped$siteId

#st_write(points, "OR_Points.shp") #create shapefile of points from streamstats output

#st_write(sheds, "OR_Sheds.shp") #create shapefile of watersheds from streamstats output
