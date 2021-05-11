### Load Required packages

library(sf)
library(raster)
library(ggplot2)
library(data.table)
setwd("/Users/alexhernandez/Desktop/BUG_BLM/ZonalTest") # Working directory
library(here) # For this exercise use library here to tell R where to find files
library(ggpubr)
library(mapview)
library(dplyr)
library(rgee)
ee_Initialize()
### OR_WCCP model
### Description


### Load required inputs
### Required for OR_WCCP are:
# * Excel or csv files with coordinates + essential attributes of pour-points

Oregon.points<-read.csv(here("Vectors","AllSites2019.csv"))
head(Oregon.points)

## Working with sf package to deal with geospatial objects
## Now make a simple features object from the coordinates - lat long
Oregon_sf = st_as_sf(Oregon.points, coords = c("Longitude", "Latitude"), 
                 crs = 4326)

## Now re-project the object to the required coordinate system: Albers NAD 102039
## We can obtain the projection details (crs info) from the HUCs shapefile
## Let's first read the HUCs shapefile
HUCs<- st_read(here("Vectors","HUC4_All.shp"))
## And now extract its crs or projection info
crs2use<-crs(HUCs)
## Now let's reproject, it requires the input object and the crs to use
Oregon_sf_Albers<-st_transform(Oregon_sf,crs=crs2use)

## Due to the current file structure @ NAMC (Topo rasters are organized in folders which follow a per-Level-4-HUC nomenclature),
## Illustrate
mapview(list(HUCs["HUC4"],Oregon_sf_Albers["X"]),layer.name=c("HUCs Level 4", "Site"))
## we need to know how pourpoints spatially relate to each of those folders 
## Now let's intersect the points with the HUC polygons to obtain the HUC attribute into the points attributes 

mapview(HUCs["HUC4"])

Oregon.Att<-st_intersection(Oregon_sf_Albers, HUCs)
head(Oregon.Att)
# Obtain a summary of which HUCs are intersected, i.e. how many points are contained in each HUC
summary(as.factor(Oregon.Att$HUC4))
## Let's add a string field which is identical to folder containing HUC DEMs
Oregon.Att$HUC4str<-paste("r",Oregon.Att$HUC4,sep = "")
head(Oregon.Att)

## Let's add an empty field to store elevations that will be extracted from rasters
Oregon.Att$elevin<-NA
# 
## Create a raster list of the folders that contain DEMs that are intersected for a set of points
HUCfolders<-unique(as.character(paste("r",Oregon.Att$HUC4, sep = "")))

# Because this is a test, we will use only those DEMs that are in this computer
HUCfolders.thisMac<-HUCfolders[c(3:5)]

## Prepare a for loop to extract elevations at each one of the points


for (i in HUCfolders.thisMac){
  rastdemname<-paste(here(),"/DEM/rasters/",i,"/",i,"_dem/","w001001.adf",sep = "")# create temporary object for each DEM being read
  dem<-raster(rastdemname) # Object is converted to raster
  points2use<-Oregon.Att[Oregon.Att$HUC4str == i, ] # Work exclusively with the points that intersect the current DEM
  tetingo<-extract(dem, points2use) # Extract the elevations for the current subset of points
  Oregon.Att$elevin[Oregon.Att$HUC4str == i ]<- tetingo # Create database field to store elevations
  ## 
  print(paste("working with..",i))
  
}

# Convert to square root of elevation divided by 10
Oregon.Att$elevin<-sqrt(Oregon.Att$elevin/10)
head(Oregon.Att)


### Extract elevation using Earth Engine
USGS_NED<-ee$Image("USGS/NED")$select("elevation")

Oregon_sf_Alberss<-ee_extract(x = USGS_NED, y = Oregon_sf_Albers, scale=30, sf = TRUE)
## Convert meters to feet
Oregon_sf_Alberss$elev<-(sqrt((Oregon_sf_Alberss$elevation*3.28084)/10))



# Now let's bring the Precipitation and Temperature vectors that are utilized for the OR_WCCP model

Pred_input<-st_read(here("GIS_Stats01/Metrics/Oregon/Data","Pred_Input_All_USGS.shp"))
Pred_input<-Pred_input[,c(1,3)] # Keep only the Temperature and Precipitation columns
## Extract the value of precipitation and temperature at site locations
Oregon.Att.F<-st_intersection(Oregon_sf_Alberss ,Pred_input)

# Now let's bring the Ecoregions layer
Ecoregion<-st_read(here("GIS_Stats01/Metrics/Oregon/Data","OR_EastWest_Eco.shp"))
head(Ecoregion)
Ecoregion<-Ecoregion[,c(2,11:12)] # Keep only the EastWest column
Ecoregion$east<-0 # Add a dummy variable that depicts 1= East, 0= West
Ecoregion$east[Ecoregion$EastWest=="East"]<-1
head(Ecoregion)





# Now extract the values from the Ecoregions
Oregon.Att.FF<-st_intersection(Oregon.Att.F,Ecoregion)

## Final adjustments for WCCP model

## Reclassify into Trip's required
Oregon.Att.FF$zone<-NA
Oregon.Att.FF$zone[Oregon.Att.FF$US_L3NAME=="Coast Range"|Oregon.Att.FF$US_L3NAME=="Willamette Valley"]<-"MWCF"
Oregon.Att.FF$zone[Oregon.Att.FF$US_L3NAME=="Cascades"|Oregon.Att.FF$US_L3NAME=="Klamath Mountains"|
                     Oregon.Att.FF$US_L3NAME=="Eastern Cascades Slopes and Foothills"|
                     Oregon.Att.FF$US_L3NAME=="Blue Mountains"|
                     Oregon.Att.FF$US_L3NAME=="Columbia Plateau"]<-"WCCP"


# Now rename variables
Oregon.Att.FFF<-Oregon.Att.FF[,c(1:6,8:11,13,15)]
setnames(Oregon.Att.FFF, old=c("elev","temp_Cx10","precip_mm"), new =c("elev_sqrt","temp","precip"))

# Now export to a *csv
write.csv(Oregon.Att.FFF,here("Vectors","AllSites2019_Att.csv"))



ggscatter(Oregon.Att.FF, x = "elev_sqrt", y = "elevin", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Sqrt reference", ylab = "Sqrt Alex")

ggscatter(Oregon.Att.FF, x = "precip", y = "precip_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Precip reference", ylab = "Precip Alex")