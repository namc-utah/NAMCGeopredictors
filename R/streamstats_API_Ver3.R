#YET ANOTHER Streamcat workaround.
#Streamcat is no longer available for 4.3.2+
#so we will just fetch the JSONs from their website. No... it is not stealing...
#As long as we are using the correct areas that StreamStats covers, we should
#not have any failed watersheds. The failures in the past are not from StreamStats,
#but the streamstats package, which USGS apparently had no affiliation with.
#Anomalies can happen with downloads, as per StreamStats Admin. They say to keep
#attempting downloads until it works (up to 5 times)

rm(list=ls())
#this function will handle the json formats that streamstats returns.
#it can either be the "expected" format, or a weird format
#with a single record that has all the Xs and Ys.
#just a try catch within a function.
#using trycatch outside of a function can lead to issues
#with variables not carrying over from outside the block
#the result is an sf object.
convert_to_sf <- function(jsonio) {
  tryCatch({
    # Attempt to convert data to sf object
    sf_object <- st_as_sf(as.data.frame(jsonio[["featurecollection"]][["feature"]][["features"]][[2]][["geometry"]][["coordinates"]][[1]][[2]]),
                          coords=c('V1','V2'),crs=4269)
  }, error = function(e) {
    message('Well, this is awkward. Unusual file format? Let us try this...')

    # Handle the error by importing geometry as data frame and converting to sf
    z<-as.data.frame(jsonio[["featurecollection"]][["feature"]][["features"]][[2]][["geometry"]][["coordinates"]][[1]])
    message('shed imported as DF!')
    zz<-data.frame(X=as.vector(as.matrix(z[,1:(ncol(z)/2)])),Y=as.vector(as.matrix(z[,(1+(ncol(z)/2)):ncol(z)])))
    message('shed shaped into 2 field DF')
    sf_object<-st_as_sf(zz,coords=c('X','Y'),crs=4269)
    message('shed is now an SF object. Phew!')

    # Return the sf object to be assigned
    return(sf_object)
  })

  # Return the sf object (either from try or error)
}
boxId<-9370
max_retries<-5 #max number of retries allowed

genpath<-'C://Users//andrew.caudillo//Box//NAMC//GIS//Watersheds//'
library(NAMCr)
library(mapview)
library(tidyverse)
library(sf)

if(exists("boxId")){
  points2process<-NAMCr::query(
    api_endpoint = "samples",
    args = list(boxId = boxId))
}else{
  points2process<-NAMCr::query(
    api_endpoint = "samples",
    args = list(projectId=projectId))

}
MS<-st_read(watershed_file_path)
points2process<-points2process[points2process$siteId %in% MS$siteId ==F,]

#assigning a state abbreviation
#This only encompasses NAMC's main study region, but if we were
#working in a different region, we would need to edit this.
points2process$STATE_ABBR<-ifelse(points2process$usState=='California','CA',
                                  ifelse(points2process$usState=='Oregon','OR',
                                         ifelse(points2process$usState=='Washington','WA',
                                                ifelse(points2process$usState=='Idaho','ID',
                                                       ifelse(points2process$usState=='Utah','UT',
                                                              ifelse(points2process$usState=='Colorado','CO',
                                                                     ifelse(points2process$usState=='Arizona','AZ',
                                                                            ifelse(points2process$usState=='New Mexico','NM',
                                                                                   ifelse(points2process$usState=='Alaska','AK',
                                                                                          ifelse(points2process$usState=='Nevada','NV',
                                                                                                 ifelse(points2process$usState=='Wyoming','WY','MT')))))))))))
#remove duplicate siteIds from a box (which does happen)
points2process<-points2process[!duplicated(points2process$siteId),]
points2process<-points2process[points2process$usState!='Alaska',]
#making the above oject an sf object
points2process= sf::st_as_sf(points2process,coords=c("siteLongitude","siteLatitude"),crs=4269)

#use this for rivnet or another delineation process
nonStStats<-points2process[points2process$STATE_ABBR %in% c('NV','WY','AZ','NM'),]
#these don't need sheds and we don't have stream stats grids for them anyway
points2process<-points2process[points2process$siteId %in% c(nonStStats$siteId)==F,]
#set buffer distance for snapping etc.
if (inherits(points2process, "sf")) n = nrow(points2process)
if (inherits(points2process, "sfc")) n = length(x)
max_dist=400
buffer_size=200

#snapping the points to streamstats flowgrids
# for each point in the points2process sf object get the streams and then snap the points to those lines
out = do.call(c,lapply(seq(n), function(i) {
  geometry_input_path=paste0(pred_geometry_base_path,"GIS/StreamStatsGrids/",points2process$STATE_ABBR[i],"_stream_stats_polyline.shp")
  # transform points from NAD83 to albers equal area conic USGS- all stream polylines are in albers and must be in albers (meters) not NAD83 (decimal degrees) to ensure that the buffer below gets the correct streams
  AOItrans<-sf::st_transform(points2process, 5070) # must use the same EPSG as in the shapefile
  #subset the sf object to only contain one point at a time
  AOItranss=AOItrans[i,"geometry"]
  #create well known text to query the stream layer by
  AOItrans_wkt <- AOItranss %>%
    sf::st_geometry() %>% # convert to sfc
    sf::st_buffer(buffer_size) %>% # buffer 200 meters
    sf::st_as_text() # convert to well known text
  #read in streams within a 200 m buffer of the point
  line_geometry<-sf::st_read(geometry_input_path, wkt_filter = AOItrans_wkt)


  # this part:
  # 1. loops through every input point
  # 2. snaps a point to the nearest line geometries
  # 3. calculates the distance from point to line geometries
  # 4. retains only the shortest distances and generates a point at that intersection
  nrst = st_nearest_points(st_geometry(AOItrans)[i], line_geometry)
  nrst_len = st_length(nrst)
  nrst_mn = which.min(nrst_len)
  if(length(nrst_len)==0) return(AOItrans$geometry[i])
  if (as.vector(nrst_len[nrst_mn]) > max_dist) return(AOItrans$geometry[i])
  return(st_cast(nrst[nrst_mn], "POINT")[2])
})
)


#convert list output to dataframe
out_xy <- st_coordinates(out) %>% as.data.frame()
#convert dataframe to sf object and tranform albers back to NAD83
out_xy <-out_xy %>%
  st_as_sf(coords=c("X","Y"), crs=5070, remove=FALSE)%>%
  st_transform(out_xy,crs=4269)
out_xy$SnpLONG=sf::st_coordinates(out_xy)[,1]
out_xy$SnpLAT=sf::st_coordinates(out_xy)[,2]
out_xy=subset(out_xy,select=-c(X,Y))
#Add in identifiers from orignial dataframe
out_xy=cbind(out_xy,st_drop_geometry(points2process[,c('siteId','STATE_ABBR')]),st_coordinates(points2process))
out_xy=dplyr::rename(out_xy,OrgLAT=Y)
out_xy=dplyr::rename(out_xy,OrgLONG=X)
#calculate the distance between the snapped point and the orignial point in meters
out_xy$distm=round(acos(sin(as.numeric(out_xy$OrgLAT)*3.141593/180)*sin(as.numeric(out_xy$SnpLAT)*3.141593/180) + cos(as.numeric(out_xy$OrgLAT)*3.141593/180)*cos(as.numeric(out_xy$SnpLAT)*3.141593/180)*cos(as.numeric(out_xy$SnpLONG)*3.141593/180-as.numeric(out_xy$OrgLONG)*3.141593/180)) * 6371000,digits=0)
#order points by siteId so that we get the points back from Streamstats in a consistent order
out_xy=out_xy[order(out_xy$siteId),]


#just getting coords (and nothing else) from out_xy
snapt<-as.data.frame(st_coordinates(out_xy))
#make an empty list with a crs so we can
#add sheds to it.

listy<-list()
#assign temp directory for the jsons.
shed_trashbin<-tempdir()

#This is the for loop that will download jsons via URL
#convert them to an sf object
#and save the outputs to list elements.
#it will automatically retry up to 5 times is a site fails.
for (i in 1:nrow(out_xy)) {
  # Remove any past sheds from previous iterations to avoid confusion
  if (exists('pp')) {
    message('Removing previous sheds...')
    rm(pp)
  }
  #remove any old json file objects that R stored from a previous iteration
  if (exists('jj')) {
    rm(jj)
  }
#set some values for the retrying part
#retries is the number of retries R has tried for a watershed
#streamstats admins say that the API can sometimes return
#failed objects and to try up to 5 times before quitting
  retries <- 0
#setting this value, which dictates whether the loop should retry a shed or
#jump to the next iteration. This gets re-written lower in the code
  success <- FALSE

  #the while loop decides if a shed needs to be re-download or if it successful
  #when we have fewer than 5 retries and still failed, then we restart the attempt
  #and increase the retry counter by 1
  while (retries < max_retries && !success) {
    withRestarts(
      retry = function() {
        retries <<- retries + 1
        message(sprintf("Retrying... Attempt %d of %d", retries, max_retries))
      },
      #this the actual json section
      #tryCatch allows an error handler that can be customized
      #instead of the code just breaking as soon as an error happens
      tryCatch({
        # Subset out the ith
        #and define some variables
        y <- snapt[i,]
        site <- out_xy$siteId[i]
        X <- y$X
        Y <- y$Y
        message('Accessing URL...')

        # Creating the URL
        url <- paste0('https://streamstats.usgs.gov/streamstatsservices/watershed.geojson?',
                      'rcode=', out_xy$STATE_ABBR[i], '&xlocation=', X, '&ylocation=', Y,
                      '&crs=4269&includeparameters=false&includeflowtypes=false&includefeatures=true&simplify=false')
        #download the file to a temp directory
        download.file(url, 'jsontest.geojson')
        message('GeoJSON downloaded')

        # Read in the json
        pp <- jsonlite::fromJSON('jsontest.geojson')
        message('GeoJSON imported')

        # convert the json to an sf object (see function at top of script for
        #details)
        jj <- convert_to_sf(jsonio = pp)

        # Coerce to polygon because the sheds come back as points for some reason
        polygon <- jj %>%
          summarize(do_union = FALSE) %>%
          st_cast("POLYGON")
        message('The shed is now a polygon')

        # Assign siteId
        polygon$siteId <- out_xy$siteId[i]

        # Assign to the list element
        listy[[i]] <- polygon

        # Take out the garbage
        unlink(paste0(shed_trashbin, '/*'))

        # Tell us that it is done
        message('End of iteration\n')
        message(paste('Processed shed ', i, ' of ', nrow(out_xy)))
        success <- TRUE
      }, #here is the error handler
      error = function(e) {
        message('Error occurred while processing. Attempting to retry...')
        invokeRestart("retry")
      })
    )
  }
#This message will appear if we have met all 5 retries and still failed.
  if (!success) {
    message(sprintf("Skipping iteration %d after %d failed attempts\n", i, retries))
    # Handle skipped iteration if necessary
    next  # Skip to next iteration
  }
}
#now for post-processing
#convert the list to a dataframe
listy<-do.call(rbind,listy)
#if we are missing sheds because of NEXT
if(nrow(listy)< nrow(out_xy)){
  warning('some of your sheds failed...')
  #create a vector of sites that failed and use rivnet or nhd for them
  #you can even call issue sheds from this script if you have the other
  #scripts open in this environment.
  issue_sheds<-out_xy$siteId[out_xy$siteId %in% listy$siteId==F]
  message('some sites still require watersheds. Try another method.')
  message('issue_sheds contains all sites that failed.')
  allsheds<-listy[!is.null(listy),]
}else{
  message('All watersheds were delineated! Lucky you!')
  allsheds<-listy
}
#view to check sheds against topographic map. do they make sense? Are there bugaboos?
#if yes, subset them manually into a new object!
mapview::mapview(allsheds,map.types='OpenTopoMap')+mapview::mapview(out_xy[out_xy$siteId %in% allsheds$siteId,])
if(length(nonStStats)>0){
  message(paste(nonStStats$siteId, 'need(s) watershed(s) delineated via rivnet or NHDplus'))
}


#old attempts, do not worry about these.

if(0){
  #this is the code that does the heavy lifting of watershed processing.
  #keep closed unless you need to edit some stuff.
  for(i in 1:nrow(out_xy)){
    #remove any past sheds from previous iterations to avoid confusion
    if(exists('pp')){
      message('removing previous sheds...')
      rm(pp)
    }
    if(exists('jj')){
      rm(jj)
    }
    #subset out the ith
    y<-snapt[i,]
    site<-out_xy$siteId[i]
    X<-y$X
    Y<-y$Y
    message('accessing URL...')
    #creating the url. We are just pasting together the skeleton of the url
    #plus the dynamic pieces from y-- not too difficult.
    url<-paste0('https://streamstats.usgs.gov/streamstatsservices/watershed.geojson?',
                'rcode=',out_xy$STATE_ABBR[i],'&xlocation=',X,'&ylocation=',Y,'&crs=4269&includeparameters=false&includeflowtypes=false&includefeatures=true&simplify=false')
    download.file(url,'jsontest.geojson')
    message('geoJSON downloaded')
    #read in the json
    tryCatch({
      pp<-jsonlite::fromJSON('jsontest.geojson')
      message('geoJSON imported')
      #run this function. #see the function at the top of the script for details
      jj<-convert_to_sf(jsonio = pp)
    },
    error=function(e){
      message('geoJSON failed to import. NEXT!')
    }
    )
    if(!exists('jj')){
      next
    }
    #coerce to polygon because the sheds come back as points for some reason.
    polygon <- jj %>%
      summarize(do_union = FALSE) %>%
      st_cast("POLYGON")
    message('the shed is now a polygon')
    #assign siteId
    polygon$siteId<-out_xy$siteId[i]
    #assign to the list element
    listy[[i]]<-polygon
    #take out the gahbage
    unlink(paste0(shed_trashbin,'/*'))
    #tell us that it is done
    message('end of iteration\n')
    message(paste('processed shed ', i,' of ',nrow(out_xy)))
  }


  #final coercion to a workable data frame
  #note conditions and the genesis of "issue_sheds"
  #if a site has failed.
  if(length(lapply(listy,is.null))>0){
    listy<-listy[!sapply(listy,is.null)]
    warning('some of your sheds failed...')
    issue_sheds<-out_xy$siteId[out_xy$siteId %in% allsheds$siteId==F]
    message('some sites still require watersheds. Try another method.')
    message('issue_sheds contains all sites that failed.')
    allsheds<-do.call(rbind,listy)
  }else{
    message('All watersheds were delineated! Lucky you!')
    allsheds<-do.call(rbind,listy)
  }
  #plot to make sure the sheds look good.
  #topo has some rivers and also shows a good indication of where the boundaries
  #of the shed fall. Do they match up to the topography?
  mapview::mapview(allsheds,map.types='OpenTopoMap')+mapview::mapview(out_xy[out_xy$siteId %in% allsheds$siteId,])
}

if(0){
  for(i in 1:nrow(snapt)){
    #subset out the ith row
    y<-snapt[i,]
    site<-out_xy$siteId[i]
    X<-y$X
    Y<-y$Y
    message('accessing URL...')
    #creating the url. We are just pasting together the skeleton of the url
    #plus the dynamic pieces from y-- not too difficult.
    url<-paste0('https://streamstats.usgs.gov/streamstatsservices/watershed.geojson?',
                'rcode=',out_xy$STATE_ABBR[i],'&xlocation=',X,'&ylocation=',Y,'&crs=4269&includeparameters=false&includeflowtypes=false&includefeatures=true&simplify=false')
    #now we just read the url using the jsonlite package
    jsonURL<-jsonlite::fromJSON(url,flatten = T)
    message('URL accessed. Processing into an object.')
    #now coerce that json's geometry to a dataframe.
    jsonDF<-as.data.frame(jsonURL[["featurecollection"]][["feature"]][["features"]][[2]][["geometry"]][["coordinates"]][[1]][[2]])
    #convert that data frame to an sf object with a coordinate system
    jsonSF<-sf::st_as_sf(jsonDF,coords=c('V1','V2'),crs=4269)
    #it is going to be points, for some reason, so we are just going
    #to coerce the points to a polygon in these two steps:
    if(st_geometry_type(jsonSF)[1]=='MULTIPOINT'){
      jsonSF<-st_cast(jsonSF,"POINT")
    }
    message('coercing to polygon...')
    polygon <- jsonSF %>%
      summarize(do_union = FALSE) %>%
      st_cast("POLYGON")
    message('watershed saved!')
    polygon$siteId=site
    #save that polygon to our list
    listy[[i]]<-polygon
    message('iteration done!')
    message(paste('Extracted shed ',i, ' of ',nrow(out_xy)))
  }




  #different attempt
  for(i in 1:nrow(out_xy)){
    if(exists('pp')){
      message('removing previous sheds...')
      rm(pp)
    }
    y<-snapt[i,]
    site<-out_xy$siteId[i]
    X<-y$X
    Y<-y$Y
    message('accessing URL...')
    #creating the url. We are just pasting together the skeleton of the url
    #plus the dynamic pieces from y-- not too difficult.
    url<-paste0('https://streamstats.usgs.gov/streamstatsservices/watershed.geojson?',
                'rcode=',out_xy$STATE_ABBR[i],'&xlocation=',X,'&ylocation=',Y,'&crs=4269&includeparameters=false&includeflowtypes=false&includefeatures=true&simplify=false')
    download.file(url,'jsontest.geojson')
    message('geoJSON downloaded')
    pp<-jsonlite::fromJSON('jsontest.geojson')
    message('geoJSON imported')
    tryCatch({
      pp<-st_as_sf(as.data.frame(pp[["featurecollection"]][["feature"]][["features"]][[2]][["geometry"]][["coordinates"]][[1]][[2]]),
                   coords=c('V1','V2'),crs=4269)
    },
    error=function(e){
      oops=1
      message('well, this is awkward. Unusual file format? Let us try this...')
      z<-as.data.frame(pp[["featurecollection"]][["feature"]][["features"]][[2]][["geometry"]][["coordinates"]][[1]])
      message('shed imported as DF!')
      zz<-data.frame(X=as.vector(as.matrix(z[,1:(ncol(z)/2)])),Y=as.vector(as.matrix(z[,(1+(ncol(z)/2)):ncol(z)])))
      message('shed shaped into 2 field DF')
      zzz<-st_as_sf(zz,coords=c('X','Y'),crs=4269)
      message('shed is now an SF object. Phew!')
      return(z)
      return(zz)
      return(zzz)
    }
    )
    if(exists('oops')){
      pp<-zzz} else{
        pp<-pp
      }
    polygon <- pp %>%
      summarize(do_union = FALSE) %>%
      st_cast("POLYGON")
    message('the shed is now a polygon')
    polygon$siteId<-out_xy$siteId[i]
    listy[[i]]<-polygon
    unlink(paste0(shed_trashbin,'/*'))
    message('end of iteration\n')
  }
  mapview::mapview(STshed_list[2])
}
