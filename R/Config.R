
pred_geometry_base_path="Z://GIS/GIS_Stats"
## Load required packages
library(sf)
library(raster)
library(exactextractr)
library(reticulate)
library(rgee)
library(dplyr)
library(lubridate)
library(units)
library(geojsonsf)
library(tictoc)
library(R6)
## Load useful packages
#
#library(rmapshaper)
#library(whitebox)


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

