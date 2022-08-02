####################

#     Geometry     #

####################


##### lat ####
#' Latitude of the point
#' This function returns the Y coordinate of the point in decimal degrees
#' The st_coordinates function returns the second column [,2] which is the latitude
#'
#' @param point2process
#' @param ...
#'
#' @return this functions returns one value which is the latitude of the point
#' @export
#'
#' @examples
lat<-function(point2process,...){
   media<-sf::st_coordinates(point2process)[,2]
  return(media)
}


#### long ####
#' Longitude of the point
#'
#' @param point2process
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
long<-function(point2process,...){
   media<-sf::st_coordinates(point2process)[,1]
  return(media)
}


#### watersheds ####

#' Area of the watershed in sq km
#' It obtains the area in square kilometers --> drop_units(st_area(validgeometry)/1000000) <-- for the watershed
#'
#' @param polygon2process this is a geojson string for the watershed
#' @param ...
#' The geojson is converted to an object of type sf -->validgeometry<-geojson_sf(polygon2process)<--
#' @return this functions returns one value which is the area of the watershed
#' @export
#'
#' @examples
WSA_SQKM<-function(polygon2process,...){
   media<-units::drop_units(sf::st_area(polygon2process)/1000000)
  return(media)
}

#' Log watershed area in sq km
#'
#' @param polygon2process
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
LOG_WSA_SQKM<-function(polygon2process,...){
  media<-log10(WSA_SQKM(polygon2process))
  return(media)
}



#' Snap points to a line
#'
#' @param points2process sf object of one or multiple points
#' @param line_geometry sf object in the same coordinate system as the points2process
#' @param namevar unique identifer in original dataset you want added back in
#' @param max_dist maximum distance you want points snapped
#' @param ...
#'
#' @return a single value 1 or 0 - 1 if the point is the eastern Oregon ecoregion, else 0

st_snap_points <- function(points2process, line_geometry, namevar, max_dist = 200,...) {

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
                  nrst = st_nearest_points(st_geometry(points2process)[i], line_geometry)
                  nrst_len = st_length(nrst)
                  nrst_mn = which.min(nrst_len)
                  if (as.vector(nrst_len[nrst_mn]) > max_dist) return(st_geometry(points2process)[i])
                  return(st_cast(nrst[nrst_mn], "POINT")[2])
                })
  )
  # this part converts the data to a dataframe and adds a named column of your choice
  out_xy <- st_coordinates(out) %>% as.data.frame()
  out_xy <- out_xy %>%
    mutate({{namevar}} := points2process[[namevar]]) %>%
    st_as_sf(coords=c("X","Y"), crs=st_crs(points2process), remove=FALSE)

  return(out_xy)
}

#https://rdrr.io/cran/spNetwork/man/snapPointsToLines2.html
