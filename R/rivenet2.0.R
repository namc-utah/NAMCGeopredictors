library(sf)
library(NAMCr)
library(sp)
library(rivnet)
library(tidyverse)
library(traudem)

library(terra)
temp_RASTER_path<-tempdir()
sus<-c(45138,45140,45151,45160,45161,45165,45168,45170,45176) #8282
x<-NAMCr::query(
  api_endpoint = "sites",
  args = list(boxIds=boxId))
download_dem_tiles <- function(outlet_coords, buffer_distance_m) {
  # Create bounding box around the outlet with a buffer
  outlet_point <- sf::st_point(c(outlet_coords$longitude, outlet_coords$latitude))
  outlet_point <- sf::st_sfc(outlet_point, crs = 4269)
  #now make a point with a buffer. This will help get a large raster tile.
  buff_pt <- sf::st_buffer(outlet_point, dist = buffer_distance_m)
  #convert the buffered point/polygon into a straight vector
  buffington <- unlist(buff_pt[[1]])
  #get some important information on how the vector is stored
  len <- length(buffington)
  len2 <- len / 2
  len3 <- len2 + 1
  #create a new df that is just the vector info (lon/lat)
  buff_df <- data.frame(lon = buffington[1:len2], lat = buffington[len3:len])
  #create the resulting polygon as an sf object that elevatr can use.
  buff_poly <- sf::st_as_sf(buff_df, coords = c('lon', 'lat'), crs = 4269)

  # Download DEM tiles covering the bounding box
  #z=13 is a common size resolution. Change as needed.
  dem_tiles <- elevatr::get_elev_raster(buff_poly, z = 13)

  return(dem_tiles)
}
#change this as needed. 12000-17000 is a good value from experience.
#if you are still getting z[[i]] out of bounds errors with these values
#use another method. It just means the watershed is large and the DEMs
#do not cover the necessary terrain
buffer_distance_m<-17000
shed_list<-sf::st_sfc(crs=4269)
for(i in 1:nrow(x)){
  #get the DEM that we need
  nastyrasty<-download_dem_tiles(outlet_coords=x[i,c('longitude','latitude')],buffer_distance_m)
  #unlink(paste0(DEM_trashbin,'/*'))
#remove an existing shed to avoid incorrect shed to siteId association

  if(exists("r")){
    rm(r)
  }
  print(i)


  old <- Sys.time() # get start time


  message('attempting shed extraction')
    #tryCatch will help us
      tryCatch(
        {

          outy<-sf::st_point(c(x$longitude[i],x$latitude[i]))
          outy <- sf::st_sfc(outy, crs = 4269)
        #extract the river/shed (you get both)
        r <- extract_river(outlet = c(outy[[1]][1],outy[[1]][2]),
                           DEM=nastyrasty,
                           n_processes = 8,z=13)
        #if r was successfully created, then let the user know
        if(exists("r")){
          message('extraction done!')
        }
        },
      #else, jump back up to the outer loop and try the next DEM size
      error=function(e){
        message('error - DEM is too small for the shed. Next iteration')



      }

        )
if(exists("r")){
  message('processing shed...')
}else{
  next
}


  #the river object stores a lot of neat stuff
  #some that we don't need for watershed delineation
  #what we do want is "catchment" or CM
  #but it is stored in a weird list format
  #so we will unlist it, then take the raw coords and make them
  #into an sf object

  o<-unlist(r$CM)
  #this is synonymous with the LIKE
  #operator in SQL. We want only values
  #whose columns contain X or Y countour
  xs<-o[ grepl( "XContour" , names( o ) ) ]
  ys<-o[ grepl( "YContour" , names( o ) ) ]
  #make a little df
  raw_shed_coords<-data.frame(xs,ys)
  #change the little df names
  names(raw_shed_coords)<-c('X','Y');row.names(raw_shed_coords)<-NULL
  #make the shed into an sf object
  sf_shed <- raw_shed_coords %>%
    st_as_sf(coords = c("X", "Y"), crs = 4269) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
  #fill the list
  sf_shed$siteId<-x$siteId[i]
  shed_list<-rbind(shed_list,sf_shed)
  message(paste('finished iteration',i,'of', nrow(x)))
  # print elapsed time
  new <- Sys.time() - old # calculate difference
  print(new)
  if(i >=1){
    unlink(paste0(temp_RASTER_path,'/*'))
  }
} #i
mapview::mapview(shed_list)

write_sf(shed_list,dsn='C://Users//andrew.caudillo//Box//NAMC//GIS//Watersheds//rivnet_sheds//7159_sub.shp')
x$siteId[x$siteId %in% shed_list$siteId==F]


#### big sheds?
temp_RASTER_path<-tempdir()
sus<-c(31032, 31035, 31267, 42857, 42859, 42860, 42863, 45097, 45177,45148, 45151, 45176,
         44856, 44857, 44858, 44862, 44863, 44864, 44865, 44866,44844,44860) #rest of AIM 23
x<-NAMCr::query(
  api_endpoint = "sites",
  args = list(siteIds=sus))
big_download_dem_tiles <- function(outlet_coords, buffer_distance_m) {
  # Create bounding box around the outlet with a buffer
  outlet_point <- sf::st_point(c(outlet_coords$longitude, outlet_coords$latitude))
  outlet_point <- sf::st_sfc(outlet_point, crs = 4269)
  #now make a point with a buffer. This will help get a large raster tile.
  buff_pt <- sf::st_buffer(outlet_point, dist = buffer_distance_m)
  #convert the buffered point/polygon into a straight vector
  buffington <- unlist(buff_pt[[1]])
  #get some important information on how the vector is stored
  len <- length(buffington)
  len2 <- len / 2
  len3 <- len2 + 1
  #create a new df that is just the vector info (lon/lat)
  buff_df <- data.frame(lon = buffington[1:len2], lat = buffington[len3:len])
  #create the resulting polygon as an sf object that elevatr can use.
  buff_poly <- sf::st_as_sf(buff_df, coords = c('lon', 'lat'), crs = 4269)

  # Download DEM tiles covering the bounding box
  #z=13 is a common size resolution. Change as needed.
  dem_tiles <- elevatr::get_elev_raster(buff_poly, z = 5)

  return(dem_tiles)
}
#change this as needed. 12000-17000 is a good value from experience.
#if you are still getting z[[i]] out of bounds errors with these values
#use another method. It just means the watershed is large and the DEMs
#do not cover the necessary terrain
buffer_distance_m<-70000
shed_list<-sf::st_sfc(crs=4269)
for(i in 9:9){
  #get the DEM that we need
  nastyrasty<-big_download_dem_tiles(outlet_coords=x[i,c('longitude','latitude')],buffer_distance_m)
  #unlink(paste0(DEM_trashbin,'/*'))
  #remove an existing shed to avoid incorrect shed to siteId association

  if(exists("r")){
    rm(r)
  }
  print(i)


  old <- Sys.time() # get start time


  message('attempting shed extraction')
  #tryCatch will help us
  tryCatch(
    {

      outy<-sf::st_point(c(x$longitude[i],x$latitude[i]))
      outy <- sf::st_sfc(outy, crs = 4269)
      #extract the river/shed (you get both)
      r <- extract_river(outlet = c(outy[[1]][1],outy[[1]][2]),
                         DEM=nastyrasty,
                         n_processes = 8)
      #if r was successfully created, then let the user know
      if(exists("r")){
        message('extraction done!')
      }
    },
    #else, jump back up to the outer loop and try the next DEM size
    error=function(e){
      message('error - DEM is too small for the shed. Next iteration')



    }

  )
  if(exists("r")){
    message('processing shed...')
  }else{
    next
  }


  #the river object stores a lot of neat stuff
  #some that we don't need for watershed delineation
  #what we do want is "catchment" or CM
  #but it is stored in a weird list format
  #so we will unlist it, then take the raw coords and make them
  #into an sf object

  o<-unlist(r$CM)
  #this is synonymous with the LIKE
  #operator in SQL. We want only values
  #whose columns contain X or Y countour
  xs<-o[ grepl( "XContour" , names( o ) ) ]
  ys<-o[ grepl( "YContour" , names( o ) ) ]
  #make a little df
  raw_shed_coords<-data.frame(xs,ys)
  #change the little df names
  names(raw_shed_coords)<-c('X','Y');row.names(raw_shed_coords)<-NULL
  #make the shed into an sf object
  sf_shed <- raw_shed_coords %>%
    st_as_sf(coords = c("X", "Y"), crs = 4269) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
  #fill the list
  sf_shed$siteId<-x$siteId[i]
  shed_list<-rbind(shed_list,sf_shed)
  message(paste('finished iteration',i,'of', nrow(x)))
  # print elapsed time
  new <- Sys.time() - old # calculate difference
  print(new)
  if(i >=1){
    unlink(paste0(temp_RASTER_path,'/*'))
  }
} #i

#jes<-list()
mapview::mapview(shed_list)




issues_pts<-c(1,2,4,5,6,7,8,9:22)

jes[[1]]<-shed_list
st_write(do.call(rbind,jes),'C://Users//andrew.caudillo//Box//NAMC//GIS//Watersheds//rivnet_sheds//AIM23_Misc1.shp')
