library(sf)
library(dplyr)

### 1. get sample info and coordinates
if (exists("boxId")){
  def_samples=NAMCr::query("samples",include = c("sampleId", "usState","siteId", "sampleDate"),
                           boxId=boxId)
}else {def_samples=NAMCr::query("samples",include = c("sampleId", "siteId", "sampleDate"),
                                projectId=projectId)
}

siteIds=unlist(unique(def_samples$siteId))

def_sites=list()
# for each site in def_predictors get site coordinates and comid from database
# store as a list of lists referenced by "x" plus the siteId
for (t in 1:length(siteIds)){
  if(t==1){
    def_sites= unlist(NAMCr::query(
      api_endpoint = "siteInfo",
      include = c("siteId", "siteName", "usState", "location","waterbodyCode",'longitude','latitude'),
      siteId = siteIds[t]
    ))
    #forcing named vector to be dataframe for subsetting
    #if only 1 site, the code above forces it to named vector via unlist()
    def_sites<-data.frame(as.list(def_sites))
  } else {def_sites1= unlist(NAMCr::query(
    api_endpoint = "siteInfo",
    include = c("siteId", "siteName", "usState", "location","waterbodyCode",'longitude','latitude'),
    siteId = siteIds[t]
  ))
  def_sites=as.data.frame(rbind(def_sites,def_sites1))
  }

}

sites_sf=st_as_sf(def_sites,coords=c('longitude','latitude'),crs=4326)
#write_sf(sites_sf,"sites.shp")


#### 2. Load NHD and HydroBASINS shapefile https://www.hydrosheds.org/products/hydrosheds-v2
###these files are massive and might need to be downloaded and read in from your computer rather than box
NHD <- st_make_valid(st_read(paste0(pred_geometry_base_path,"GIS/GIS_Stats/Alaska/hydrosheds/blm_natl_admu_field_pol_ClipNHD.shp")))
basins<-st_make_valid(st_read(paste0(pred_geometry_base_path,"GIS/GIS_Stats/Alaska/hydrosheds/AKhydrobasins.gdb")))

##### 3. intersect sites with basins to get STRM_ID
sitesinter=st_join(sites_sf,basins)


##### 4. Function to recursively find all upstream HYBAS_IDs
get_upstream_ids <- function(target_id, df) {
  # Find immediate upstream basins where NEXT_DOWN equals the target ID
  up_ids <- df$STRM_ID[df$STRM_DN == target_id]

  if (length(up_ids) == 0) {
    return(target_id)
  } else {
    # Recursively collect further upstream IDs
    return(c(target_id, unlist(lapply(up_ids, get_upstream_ids, df = df))))
  }
}


##### 5. apply function in a loop to get watersheds
#### not all samples will be in areas with coverage (North Slope, for example, has no coverage)
### so we need to remove those blank IDs
target_outlet_ids <- sitesinter$STRM_ID[!is.na(sitesinter$STRM_ID)]  # Replace with your specific basin ID
combined_upstream_polygon=list()
###for the rest of the code, use target_outlet_ids instead of sitesinter$STRM_ID
for (i in 1:length(target_outlet_ids)){
# Get unique list of all upstream IDs including the outlet itself
  all_upstream_ids <- unique(get_upstream_ids(target_outlet_ids[i], NHD))

# Subset the spatial polygons dataframe
  upstream_subset <- basins %>% filter(HYBAS_ID %in% all_upstream_ids)

# Combine (dissolve) all upstream polygons into a single geometry
  combined_upstream_polygon[[i]] <- st_union(upstream_subset)

}

### 6. format file for database
watershedsdf=do.call(c,combined_upstream_polygon)
sfge=st_sf(geometry=watershedsdf)
sfge$STRM_ID=target_outlet_ids
sfge$siteId=target_outlet_ids
st_write(sfge,paste0(userpath,"GIS//Watersheds//AK//AKhydrosheds_14788_20261105.shp"))

#### 7. load watershed shapefile into ArcPro and the sites
# load above NHD file into arcPro
# load aerial imagery into arcPro https://geoportal.alaska.gov/arcgis/rest/services/ahri_2020_rgb_cache/MapServer
# load ESRI hillshade map
# using the above layers to guide your decisions, use the "Modify" tool under the edit menu to manually chop off the bottom of any watershed as needed to align with the sampled location.


# Optional: Plot the unified catchment boundary
plot(st_geometry(sfge), col = "lightblue", border = "blue")
