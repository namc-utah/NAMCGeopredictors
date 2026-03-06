# Quick Watershed Delineation using NHDPlusTools
  # adapted for NAMC use by Andrew Caudillo


  # 1.0 Setup Environment
#clear memory
rm(list=ls())

#Call relevant libraries
library(tidyverse)
library(mapview)
library(raster)
library(sf)
library(tigris)
library(nhdplusTools)
  #remotes::install_github('DOI-USGS/nhdplusTools')
#this was not loaded originally
library(NAMCr)

#options(tigris_use_cache=T)
#tigris_cache_dir('C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC WATS Department Files//GIS//Watersheds//nhdPlusTools//NHDShedPath')

#pred_geometry_base_path="C://Users//andrew.caudillo//Box//NAMC//"

mastershed<-st_read(watershed_file_path)

#nv<-st_read('C://Users//andrew.caudillo//Box//NAMC//GIS//Watersheds//SnappedPointsSentStreamStats//CA_2023-11-01.shp')
samps<-NAMCr::query(
  api_endpoint = "samples",
  projectId=6569)
#samps<-samps[samps$sampleId==213513,]

#samps<-samps[samps$siteId %in% lilMS$siteId[44:89],]
#samps<-samps[samps$siteId==46642,]

#for input from WS Delineation script...
#samps_For_sheds<-samps[samps$siteId==46455,]



missings_sheds<-samps[samps$siteId %in% mastershed$siteId==F,]
missings_sheds<-samps
#missings_sheds<-samps$siteId[samps$siteId %in% missings_sheds]
samps_For_sheds<-samps[samps$siteId %in% missings_sheds$siteId,]
#samps_For_sheds<-samps_For_sheds[samps_For_sheds$siteId==46642,]
samps_For_sheds<-missings_sheds[missings_sheds$usState!='Alaska',]
samps_For_sheds<-samps

#Load state shape
#it is imperative to change the state abbreviation!

bama <- states() %>% dplyr::filter((STUSPS %in% c('AZ','CA','CO','ID',
                                                  'MT','NV','NM','OR',
                                                  'SD','TX','UT','WA','WY')))

bama <-paste0(NHD_config,'cb_2023_us_state_20m.shp')

bama<-sf::st_read(bama)

#bama<- bama %>% dplyr::filter((STUSPS %in% c('WY')))


#bama<- bama %>% dplyr::filter((STUSPS %in% c('NV')))



#making df of just the site points
#remember that NAMC uses sites, not samples, for model application and predictors
site_info<-NAMCr::query('sites',
                        projectIds=6569)
site_info<-site_info[site_info$usState!='Alaska',]
query<- paste0("SELECT WsAreaSqKm,COMID FROM StreamCat_2022 WHERE COMID IN (", paste(site_info$waterbodyCode,collapse=','), ")")

mydb=DBI::dbConnect(RSQLite::SQLite(),'C://NAMC_S3//streamcat_dat//StreamCat2022.sqlite')
Area_info<-DBI::dbGetQuery(mydb,query)
names(site_info)[names(site_info)=='waterbodyCode']<-'COMID'
Area_info<-plyr::join(Area_info,site_info[,c('siteId','COMID')])
#samps_For_sheds=site_info
samps_For_sheds<-samps_For_sheds[!duplicated(samps_For_sheds$siteId),]
samps_For_sheds=plyr::join(samps_For_sheds,Area_info[,c('siteId','COMID')],by='siteId')
#sites_for_COMIDS=NAMCr::query('sites',sampleIds=missings_sheds$sampleId)
samps_For_sheds_Coords<-samps_For_sheds[,c("siteLatitude","siteLongitude",'COMID')]


#this is the "pour point" essentially

#outlet<-sf::st_as_sf(samps_For_sheds_Coords, coords = c("siteLongitude","siteLatitude"),crs=4269)
#outlet$siteId<-samps_For_sheds$siteId

outlet<-sf::st_as_sf(samps_For_sheds_Coords, coords = c("siteLongitude","siteLatitude"),crs=4326)
outlet$siteId<-samps_For_sheds$siteId


# outlet<-outlet[outlet$siteId %in% c(46298,
#                                     46299,
#                                     46300,
#                                     46301,
#                                     46302,
#                                     46303,
#                                     46304,
#                                     46305,
#                                     46306,
#                                     46307,
#                                     46308,
#                                     46309,
#                                     46310),]



#Rerpoject and clip to continental US
bama <- st_transform(bama, 5070)
outlet <- st_transform(outlet, 5070)
#outlet<-st_transform(missings_sheds,5070)
library(purrr)


NHDsheds_sf2 <- outlet$COMID %>%
  # Use map_df to iterate and bind results into one sf object automatically
  map_df(~ {
    tryCatch({
      Sys.sleep(1)
      # Call get_nldi_basin for each COMID
      get_nldi_basin(list(featureSource = "comid", featureID = as.character(.x)),split=T) %>%
        st_transform(5070) %>%
        sfheaders::sf_remove_holes() %>%
        # Keep the COMID to join back to your original data later
        mutate(COMID = .x)
    }, error = function(e) {
      message(paste("Error with COMID", .x, ":", e$message))
      return(NULL) # map_df will skip NULLs
    })
  })


if(0){
NHDshed_list<-st_sfc(crs=5070)
results_list=st_sfc(crs=5070)
#this is just a check for failed sites
siteIds<-as.data.frame(matrix(ncol=1,nrow = nrow(outlet)))
NHDshed_list <- list()

for (i in 1:nrow(outlet)) {
  print(paste("Processing iteration:", i))

  # Use tryCatch to capture the result of the block
  # We return NULL on error so it doesn't break the loop
  iteration_result <- tryCatch({

    # --- Your Spatial Logic ---
    point_sf <- outlet$geometry[i, , drop = FALSE]
    point_geo <- st_geometry(point_sf)[[1]]

    point_geo_sfc <- st_sfc(st_point(c(st_coordinates(point_geo)[1, 1],
                                       st_coordinates(point_geo)[1, 2])))
    st_crs(point_geo_sfc) <- 5070

    start_comid <- outlet$COMID[i]

    flowlines <- navigate_nldi(list(featureSource = "comid",
                                    featureID = start_comid),
                               mode = "upstreamTributaries",
                               distance_km = 1000)

    subset_file <- tempfile(fileext = ".gpkg")
    subset <- subset_nhdplus(comids = as.integer(flowlines$UT$nhdplus_comid),
                             output_file = subset_file,
                             nhdplus_data = "download",
                             flowline_only = FALSE,
                             return_data = TRUE,
                             overwrite = TRUE)

    catchment <- sf::read_sf(subset_file, "CatchmentSP")
    catchment <- sf::st_union(catchment)
    catchment <- sfheaders::sf_remove_holes(catchment)
    catchment <- st_transform(catchment, 5070)

    # Convert to sf and attach the siteId immediately
    # This keeps your data linked and organized
    res <- st_as_sf(catchment)
    res$siteId <- outlet$siteId[i]

    # Return the successful object
   # res

  }, error = function(e) {
    message(paste("Error at iteration", i, ":", e$message))
    message('the next iteration is starting')
    return(NULL)
    # Return NULL so this index in the list stays empty
  })

  # 2. Store the result (even if it's NULL)
  NHDshed_list[[i]] <- iteration_result
}

# 3. Combine everything at once (much faster than repeated rbind)
# Filter out the NULLs first
final_NHDsheds <- do.call(rbind,   NHDshed_list[!sapply(NHDshed_list, is.null)])

for (i in 1:nrow(outlet)) {
  print(i)

  # Wrap the whole iteration in tryCatch
  tryCatch({
    # isolate the geometry
    point_sf <- outlet$geometry[i, , drop = FALSE]

    # make it a point
    point_geo <- st_geometry(point_sf)[[1]]

    # make it an sf object
    point_geo_sfc <- st_sfc(st_point(c(st_coordinates(point_geo)[1, 1],
                                       st_coordinates(point_geo)[1, 2])))
    st_crs(point_geo_sfc) <- 5070
    print(point_geo_sfc)

    # determine comid
    start_comid <- outlet$COMID[i]#discover_nhdplus_id(st_sfc(point_geo_sfc))
    print(start_comid)

    # isolating flowlines of interest
    flowlines <- navigate_nldi(list(featureSource = "comid",
                                    featureID = start_comid),
                               mode = "upstreamTributaries",
                               distance_km = 1000)

    # get NHDplus files
    subset_file <- tempfile(fileext = ".gpkg")
    subset=get_nldi_basin(list(featureSource='comid',featureID=paste(start_comid)))
    # subset <- subset_nhdplus(comids = as.integer(flowlines$UT$nhdplus_comid),
    #                          output_file = subset_file,
    #                          nhdplus_data = "download",
    #                          flowline_only = FALSE,
    #                          return_data = TRUE,
    #                          overwrite = TRUE)

    # get the watershed (catchment)
    catchment <- subset#sf::read_sf(subset_file, "CatchmentSP")

    # dissolve files to single catchment
    catchment <- sf::st_union(catchment)

    # fill any holes from geoprocessing
    catchment <- sfheaders::sf_remove_holes(catchment)

    # transform CRS to match
    catchment <- st_transform(catchment, 5070)

    # append to list
    results_list[[i]] = st_as_sf(catchment)
    #NHDshed_list <- rbind(NHDshed_list, st_as_sf(catchment))

    # record successful siteId
    siteIds$siteId[i] <- outlet$siteId[i]

  }, error = function(e) {
    message(paste("Error at iteration", i, ":", e$message))
    # Skip to the next iteration
    next
  })
}
NHDshed_list=do.call(rbind,results_list)
samps_For_sheds$siteId[samps_For_sheds$siteId %in% shed_list$siteId==F]

st_geometry(NHDshed_list)<-'geometry'
NHDshed_list$siteId=siteIds$siteId

#samps_For_sheds$siteId[samps_For_sheds$siteId %in% shed_list$siteId==F]

#plot just to see everything looks alright
#if a shed looks incorrect, use a different method like whitebox.
#could also use global watersheds for a quick look.
mapview(list(outlet,NHDshed_list))+ #this is NHD and pour point
  mapview(AIM_diffs[AIM_diffs$siteId==20232,],col.regions='red') #this is the streamstats
mapview(outlet, layer.name='Pour Point')+
  mapview(NHDshed_list[1],layer.name='StreamCat',
          col.regions='blue')+
  mapview(AIM_diffs[AIM_diffs$siteId == 20232, ],
          col.regions = 'red',
          layer.name = "StreamStats")
mapview(NHDshed_list)


NHDshed_list$siteId<-outlet$siteId
mapview(list(NHDshed_list,shed_list))
mapview(NHDshed_list[10,])
mapview(shed_list)
pal=mapviewPalette("mapviewTopoColors")
mapview(shed_list,col.regions=pal(11))




#st_write(NHDshed_list,dsn='C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC WATS Department Files//GIS//Watersheds//nhdPlusTools//WY_10163.shp',append=F)

if(0){for(i in 1:nrow(outlet)){
  print(i)
  #isolate the geometry
  point_sf<-outlet$geometry[i, , drop=F]
  #make it a point
  point_geo<-st_geometry(point_sf)[[1]]
  #make it an sf object
  point_geo_sfc<-st_sfc(st_point(c(st_coordinates(point_geo)[1,1],st_coordinates(point_geo)[1,2])))
  st_crs(point_geo_sfc)<-5070
  #coerce the coordinate system
  print(point_geo_sfc)
  #determine comid
  start_comid <- discover_nhdplus_id(st_sfc(point_geo_sfc))
  #make sure it worked
  print(start_comid)


  #isolating flowlines of interest
  flowlines <- navigate_nldi(list(featureSource = "comid",
                                  featureID = start_comid),
                             mode = "upstreamTributaries",
                             distance_km = 1000)


  # Note, the next step pulls NHDplus data from NLNDI server.

  # https://www.usgs.gov/national-hydrography/access-national-hydrography-products

  #get nhdplus files
  subset_file <- tempfile(fileext = ".gpkg")
  subset <- subset_nhdplus(comids = as.integer(flowlines$UT$nhdplus_comid),
                           output_file = subset_file,
                           nhdplus_data = "download",
                           flowline_only = FALSE,
                           return_data = TRUE, overwrite = TRUE)
  #Get the watershed (catchment)
  catchment <- sf::read_sf(subset_file, "CatchmentSP")

  #Dissovle files to single catchment
  #making all subwatersheds into one
  catchment <- sf::st_union(catchment)
  #fill any holes from the geoprocessing
  catchment<-sfheaders::sf_remove_holes(catchment)
  #transform it to match the other crs
  catchment<-st_transform(catchment,5070)

  #make a list of sheds that grows with each iteration
  NHDshed_list = rbind(NHDshed_list, st_as_sf(catchment))
  #keep a list of the sites that made it through
  siteIds$siteId[i]<-outlet$siteId[i]
}
}


st_write(NHDshed_list,dsn='C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC WATS Department Files//GIS//Watersheds//nhdPlusTools//problem_sheds3_241008.shp',append=F)
NHDshed_list$x
st_geometry(NHDshed_list)<-'geometry'

}

#sf::st_write(NHDsheds_sf,'C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC WATS Department Files//GIS//Watersheds//nhdPlusTools//6965_sheds//6965_sheds.shp')
