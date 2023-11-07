#change file paths to match your computer

pred_geometry_base_path="C://Users//andrew.caudillo//Box//NAMC//"
SQLite_file_path="C://NAMC_S3//streamcat_dat//StreamCat2022.sqlite"
watershed_file_path=paste0(pred_geometry_base_path,"GIS//Watersheds//Mastersheds//mastersheds.shp") #siteId must be in file!!!!
watershed_layer_name="mastersheds" #siteId must be in file!!!!
#file path for .gdb that streamstats sends back

#streamStatsOutputFilePath=

# input boxId or projectId
boxId=7852
#projectId=684

# all predictors will always be computed but enter in Y or N here to control which predictors are saved in the database
#if overwrite='Y' all predictors will be saved if overwrite='N', only predictors that dont already exist in the database will be saved
overwrite='N' # or 'N'

models=NAMCr::query("models")
# input modelId to subset predictors calculated to only one model.note you cant input a list of models here.
modelId=10

#def_samples=NAMCr::query("samples",sampleIds=c(155612,157568))

#read in csv with just sampleId and predictors, sampleId should be the first column
historic_pred_file_path="C://Users//andrew.caudillo//Box//NAMC//OEModeling//Geospatial predictors//UT_2021_pred.csv"


## Load required packages
library(NAMCr)
library(sf)
library(raster)
library(exactextractr)
library(rgee)
library(reticulate)
library(rgee)
library(dplyr)
library(reshape2)
library(lubridate)
library(units)
library(geojsonsf)
library(R6)
library(DBI)
library(RSQLite)
library(nhdplusTools)
library(mapview)

## Load useful packages
library(rmapshaper)
library(whitebox)

#jennifer needs to run this line but mac users likely dont need this. dont run unless ee_initallize() doesnt work

#Sys.setenv(RETICULATE_PYTHON = "C:/Users/jenni/AppData/Local/ESRI/conda/envs/arcgispro-py3-clone-1/python.exe")

#And'rew's- don't touch
Sys.setenv(RETICULATE_PYTHON='C://Program Files//Conda//envs//rgee//python.exe')

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

