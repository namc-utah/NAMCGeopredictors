library(rgdal)
library(sf)
library(dplyr)
library(raster)

setwd('/Users/triparmstrong/Library/CloudStorage/Box-Box/NAMC/GIS/Watersheds')
sheds <- ('Old files to compile/OR_Sheds.shp')
master <- ('Mastersheds/mastersheds.shp')

data_new <- st_read(sheds)
data_all <- st_read(master)

data_all %>%
  filter(siteId %in% data_new$siteId)

plot(data_new, col='blue')

#both sf objects need to be in the same crs
st_crs(data_all) #this checks what the crs is of the sf object
st_crs(data_new)
crs2use <- st_crs(data_all)#this assigns a name to the crs of the mastersheds data
data_new_2 <- st_transform(data_new, crs = crs2use)#this transforms the crs of the new data to the crs of the mastersheds file
bind_rows(data_all, data_new_2)#this is a dplyr function that binds the 2 sf objects together. It should fill in NAs for the Evaluation
              
