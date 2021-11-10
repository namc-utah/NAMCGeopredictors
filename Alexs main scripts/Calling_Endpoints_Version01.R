#install.packages("remotes")
remotes::install_github("namc-utah/NAMCr",force=TRUE)

library(NAMCr)
library(sf)
library(geojsonio)
library(geojsonR)
#library(geo)
library(mapview)
library(geojsonsf)

### list of watersheds

AIMCatch<-read.csv("C:/Users/jenni/OneDrive - USU/Desktop/final geospatial scripts/data/2020_AIM_sites_with_catchments.csv",header = TRUE)
AIMCatch<-as.data.frame(AIMCatch[,1])

# Loop over all samples that you want to process
nrow(AIMCatch)

for (sample in 1:50){
  identifier<-AIMCatch[sample,1]
  sampleInfo<-NAMCr::query('samples', sampleIds = identifier)
  sample_date<-samples[['sampleDate']]
  predictors <- NAMCr::query('samplePredictorValues', sampleId=identifier)
  print(predictors)
}


## insert predictor values

NAMCr::query('setSitePredictorValue', siteId =104843, predictorId=1, value=4  )


testpredictors.df<-NAMCr::query('samplePredictorValues', sampleId=164756)

predictors.df<-NAMCr::query('predictors')

project:
  
  # Retrieve the sample information (including sample date and site ID etc)
  sampleInfo = NAMCr::query('sampleInfo', sampleId = sample.sampleId)
sample_date = sampleInfo.sampleDate

# Retrieve the list of relevant predictors for this sample (both temporal and non-temporal)
predictors = NAMCr::query('samplePredictors, sampleId=sample.sampleId)





pirin<-list()
# Start the clock!
ptm <- proc.time()
for (i in 1:nrow(AIMCatch)){
  ii<-AIMCatch[i,]
  print(ii)
  ttcobjecto<-NAMCr::query("siteInfo", siteId=ii)
  geobjecto<-ttcobjecto[["catchment"]] # Extract the geojson object from the list
  siteident<-ttcobjecto[["siteName"]] # Extract the site identifier
  sfttc<-geojson_sf(geobjecto) # convert the geojson to sf object
  sfttc$nombre<-NA 
  sfttc$nombre<-siteident # add the watershed identifier as an attribute to the sf object
  pirin[[i]]<-sfttc # fill the empty list
}
# Stop the clock
proc.time() - ptm

sftotal<-do.call(rbind, pirin) # get the list components and turn them into a full sf object

mapview(sftotal)













ttc<-NAMCr::query("siteInfo", siteId=c(28305,27902,27992))
ttc1<-ttc[["catchment"]]

# file_js<-FROM_GeoJson(url_file_string = ttc1)
sf01<-geojson_sf(ttc1)
