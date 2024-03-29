% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Elevation_functions.R
\name{ELEV_SITE}
\alias{ELEV_SITE}
\title{Elevation of the point
The function requires that a Google Earth Engine GEE object be created
USGS_NED National Elevation Dataset. It uses rgee rgee::ee_extract function to conduct
zonal statistics. The resolution (pixel size to use) can be changed if desired by
modifying scale=. Now it is using a 90x90 m pixel size.
The rgee function rgee::ee_extract is used here without fun = ee$Reducer$min,mean,max()
argument since it only needs the information at the point}
\usage{
ELEV_SITE(points2process, USGS_NED, ...)
}
\arguments{
\item{...}{}
}
\value{
a single value which is the elevation at the point
}
\description{
Elevation of the point
The function requires that a Google Earth Engine GEE object be created
USGS_NED National Elevation Dataset. It uses rgee rgee::ee_extract function to conduct
zonal statistics. The resolution (pixel size to use) can be changed if desired by
modifying scale=. Now it is using a 90x90 m pixel size.
The rgee function rgee::ee_extract is used here without fun = ee$Reducer$min,mean,max()
argument since it only needs the information at the point
}
