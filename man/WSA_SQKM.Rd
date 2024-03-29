% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Geometry_functions.R
\name{WSA_SQKM}
\alias{WSA_SQKM}
\title{Area of the watershed in sq km
It obtains the area in square kilometers --> drop_units(st_area(validgeometry)/1000000) <-- for the watershed}
\usage{
WSA_SQKM(polygon2process, ...)
}
\arguments{
\item{polygon2process}{this is a geojson string for the watershed}

\item{...}{The geojson is converted to an object of type sf -->validgeometry<-geojson_sf(polygon2process)<--}
}
\value{
this functions returns one value which is the area of the watershed
}
\description{
Area of the watershed in sq km
It obtains the area in square kilometers --> drop_units(st_area(validgeometry)/1000000) <-- for the watershed
}
