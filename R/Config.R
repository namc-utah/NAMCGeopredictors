pred_geometry_base_path="Z://GIS/GIS_Stats/"
SQLite_file_path="C:/NAMC_S3/StreamCat/StreamCat.sqlite"
temp_predictor_metadata="C:/Users/jenni/Box/NAMC (Trip Armstrong)/OE_Modeling/Geospatial predictors/predictor_table_for_database.csv"
sampleId = 155612

## Load required packages
library(sf)
library(raster)
library(exactextractr)
library(rgee)
library(reticulate)
library(dplyr)
library(lubridate)
library(units)
library(geojsonsf)
library(tictoc)
library(R6)
library(DBI)
library(RSQLite)
library(nhdplusTools)

## Load useful packages
#library(rmapshaper)
#library(whitebox)

#jennifer needs to run this line but mac users likely dont need this. dont run unless ee_initallize() doesnt work
#Sys.setenv(RETICULATE_PYTHON = "C:/Users/jenni/AppData/Local/ESRI/conda/envs/arcgispro-py3-clone-1/python.exe")

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
