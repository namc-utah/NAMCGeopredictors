
## Load required packages
## Load useful packages
library(sf)
library(raster)
library(data.table)
wd="Z://GIS/GIS_Stats"
library(ggpubr)
library(mapview)
library(prism)
library(exactextractr)
library(mapedit)
library(reticulate)
library(rgee)
library(tidyverse)
library(survival)
library(dplyr)
library(nhdplusTools)
library(lubridate)
library(units)
library(geojsonsf)
library(rmapshaper)
library(whitebox)
# ee_install_upgrade()
# ee_Initialize()
# # if error run
# #ee_install()
# ee_install_set_pyenv(py_path ="C:/Users/jenni/AppData/Local/r-miniconda/envs/r-reticulatepython.exe", py_env="rgee")
# # restart R
# #ee_install()
# ee_check()
# use_python("C:/Users/jenni/AppData/Local/r-miniconda/envs/r-reticulate/python.exe")
# > py_config()
# python:         C:/Users/jenni/AppData/Local/r-miniconda/envs/rgee/python.exe
# libpython:      C:/Users/jenni/AppData/Local/r-miniconda/envs/rgee/python39.dll
# pythonhome:     C:/Users/jenni/AppData/Local/r-miniconda/envs/rgee
# version:        3.9.4 | packaged by conda-forge | (default, May 10 2021, 22:10:34) [MSC v.1916 64 bit (AMD64)]
# Architecture:   64bit
# numpy:          C:/Users/jenni/AppData/Local/r-miniconda/envs/rgee/Lib/site-packages/numpy
# numpy_version:  1.20.3
# ee:             C:\Users\jenni\AppData\Local\R-MINI~1\envs\rgee\lib\site-packages\ee\__init__.p
# 
# NOTE: Python version was forced by RETICULATE_PYTHON


# ###### Define predictors GEE
# USGS_NED<-ee$Image("USGS/NED")$select("elevation") # elevation
# slopegee<-ee$Terrain$slope(USGS_NED) # slope
# slopegee.perc<- slopegee$divide(180)$multiply(3.14159)$tan()$multiply(1)$rename("percent")#Slope percent

