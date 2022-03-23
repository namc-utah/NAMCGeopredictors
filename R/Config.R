pred_geometry_base_path="C:/Users/jenni/Box/NAMC (Trip Armstrong)/"
SQLite_file_path="C:/NAMC_S3/StreamCat/StreamCat2022.sqlite"
watershed_file_path="C:/Users/jenni/Box/NAMC (Trip Armstrong)/GIS/Watersheds/Mastersheds/mastersheds.shp" #siteID must be in file!!!!
watershed_layer_name="mastersheds" #siteID must be in file!!!!

# = 155612
boxId=2141
modelId=1
#projectId=

#read in csv with just sampleId and predictors, sampleId should be the first column
historic_pred_file_path="C:/Users/jenni/Box/NAMC (Trip Armstrong)/OE_Modeling/Geospatial predictors/UT_2021_pred.csv"


#def_samples=NAMCr::query("samples",sampleIds=c(155612,157568))
## Load required packages
library(NAMCr)
library(sf)
library(raster)
library(exactextractr)
library(rgee)
library(reticulate)
library(dplyr)
library(lubridate)
library(units)
library(geojsonsf)
library(tictoc)#what is this used for
library(R6)
library(DBI)
library(RSQLite)
library(nhdplusTools)

## Load useful packages
#library(rmapshaper)
#library(whitebox)

#jennifer needs to run this line but mac users likely dont need this. dont run unless ee_initallize() doesnt work
Sys.setenv(RETICULATE_PYTHON = "C:/Users/jenni/AppData/Local/ESRI/conda/envs/arcgispro-py3-clone-1/python.exe")

# Load all functions, comment out because the package should automatically do this
source("R/Ecoregion_functions.R")
source("R/Elevation_functions.R")
source("R/General_functions.R")
source("R/Geology_functions.R")
source("R/Geometry_functions.R")
source("R/Hydrology_functions.R")
source("R/Logger.R")
source("R/Precipitation_functions.R")
source("R/Slope_functions.R")
source("R/StreamCat_functions.R")
source("R/Temperature_functions.R")
source("R/Vegetation_functions.R")
