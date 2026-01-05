#load in necessary libraries
library(sf) # for spatial
library(NAMCr) #for site coords
library(sp) # for spatial
library(rivnet); library(traudem) #for watershed extraction
library(tidyverse) #for data manipulation
library(terra) #for rasterizing / converting

#create a temporary path to store the watershed and any other related info
#this will be wiped clean after every iteration of the for loop below
temp_RASTER_path<-tempdir()
#read in sites.
#alternatively, this could just be a csv of lon/lat
#in this case, x = all Alaska sites that NAMC has determined need watersheds
x<-NAMCr::query(
  api_endpoint = "sites",
  args = list(siteIds=33671))
#remove duplicates to save time and memory
x<-x[!duplicated(x$siteId),]
#define the function to donwload the DEM for each point
#Alaska is a huge region and a full DEM is not feasible (or available)
#so, stitching smaller DEMS together is the way to go.
#the function will download tiles from a bounding box (defined
#from buffer_distance_m)
#and then return the DEM.
#adjust the z (1-14) as needed, but know that the larger z is, the slower the process
#will take the slower the machine running this script will be.
big_download_dem_tiles <- function(outlet_coords, buffer_distance_m) {
  # Create bounding box around the outlet with a buffer
  outlet_point <- sf::st_point(c(outlet_coords$longitude, outlet_coords$latitude))
  outlet_point <- sf::st_sfc(outlet_point, crs = 5070)
  #now make a point with a buffer. This will help get a large raster tile.
  buff_pt <- sf::st_buffer(outlet_point, dist = buffer_distance_m)
  #convert the buffered point/polygon into a straight vector
  buff_unlisted <- unlist(buff_pt[[1]])
  #get some important information on how the vector is stored
  #then convert to lat/long chunks
  len <- length(buff_unlisted)
  len2 <- len / 2
  len3 <- len2 + 1
  #create a new df that is just the vector info (lon/lat)
  buff_df <- data.frame(lon = buff_unlisted[1:len2], lat = buff_unlisted[len3:len])
  #create the resulting polygon (bounding box) as an sf object that elevatr can use.
  buff_poly <- sf::st_as_sf(buff_df, coords = c('lon', 'lat'), crs = 5070)

  # Download DEM tiles covering the bounding box
  #z=13 is a common size resolution. Change as needed.
  dem_tiles <- elevatr::get_elev_raster(buff_poly, z = 13)

  return(dem_tiles)
}
#define the buffer distance (in meters)
#ensure the projected coordinate system has meters as its units. #if not,
#adjust the distance to reflect the correct units.
#NOTE:
#if the for loop produces " z[[i]] out of bounds" errors,
#all that means is that your buffer distance is not enough for that watershed.
#a bigger buffer distance is safer (i.e., lower chance of failing),
#but at the cost of time and processing power.

buffer_distance_m<-40000
#create an empty list in which to store the sheds
#crs 3338 is a good projection **for Alaska**
shed_list<-sf::st_sfc(crs=5070)
#this should be for (i in 1:nrow(x))
for(i in 1:nrow(x)){
  #print the current iteration
  message(i)
  #get just the lat/long out of x, but convert it to an sf object
  outlet=sf::st_as_sf(x[i,c('longitude','latitude')],coords=c('longitude','latitude'),crs=4269)
  #transform it to 3338
  outlet=sf::st_transform(outlet,crs=5070)
  #get just the coordinates out of the sf object and force it to be a data frame
  outlet_df=as.data.frame(sf::st_coordinates(outlet))
  #rename for simplicity
  names(outlet_df)<-c('longitude','latitude')
  #download the big raster
  DEM_Raster<-big_download_dem_tiles(outlet_coords=outlet_df[,1:2],buffer_distance_m)

  #this section checks to see if a watershed already exists.
  #if so, remove it from the environment to save space and for clarity
  if(exists("r")){
    rm(r)
  }


  old <- Sys.time() # get start time

  #let the user know that the shed extraction process is starting
  #...or at least, the attempt to
  message('attempting shed extraction')
  #tryCatch will help us
  tryCatch(
    {
      #extract the river/shed (you get both)
      #define arguments using the iterative objects from above
      r <- extract_river(outlet = c(outlet_df$longitude, outlet_df$latitude),
                         n_processes = 8,EPSG=5070,DEM = DEM_Raster)
      #if r was successfully created, then let the user know
      if(exists("r")){
        message('extraction done!')
      }
    },
    #else, jump back up to the outer loop and go onto the next iteration
    error=function(e){
      print(e)
      message('error - DEM is too small for the shed. Next iteration')



    }

  )
  #if r was successfully created, start stripping away all the fat
  #and keep just what we need: the watershed
  if(exists("r")){
    message('processing shed...')
  }else{
    next
  }


  #the river object stores a lot of neat stuff
  #but most of it is unncessary for just watershed delineation.
  #what we do want is "catchment" or CM.  *some regions of the world call watersheds catchments
  #but it is stored in a weird list format
  #so we will unlist it, then take the raw coords and make them into an sf object

  o<-unlist(r$CM)
  #this is synonymous with the LIKE
  #operator in SQL. We want only values
  #whose columns contain X or Y countour
  #these are the lines that make up the watershed
  xs<-o[ grepl( "XContour" , names( o ) ) ]
  ys<-o[ grepl( "YContour" , names( o ) ) ]
  #make a little data frame from the variables above
  raw_shed_coords<-data.frame(xs,ys)
  #change the little df names
  names(raw_shed_coords)<-c('X','Y');row.names(raw_shed_coords)<-NULL
  #make the shed into an sf object
  sf_shed <- raw_shed_coords %>%
    st_as_sf(coords = c("X", "Y"), crs = 5070) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
  #fill the list
  sf_shed$siteId<-x$siteId[i]
  #add the new shed to the list we created before the for loop
  shed_list<-rbind(shed_list,sf_shed)
  message(paste('finished iteration',i,'of', nrow(x)))
  # print elapsed time, just to see how long it took
  new <- Sys.time() - old # calculate difference
  print(new)
  #wipe the temporary directory's memory to save space
  #spatial file get big fast!
  if(i >=1){
    unlink(paste0(temp_RASTER_path,'/*'))
  }
} #i

#plot the sheds.
#if you see any odd sheds, like a small triangle or diamond,
#that means the watershed was partially delineated, but the DEM was not quite large
#enough.
#best to remove those sheds from the list, isolate the corresponding siteIds/coordinates
#and run the process again on just those sites.
#you can easily rbind any watershed lists together after the fact.
mapview::mapview(shed_list,lwd=3, color='red')+mapview::mapview(outlet)
