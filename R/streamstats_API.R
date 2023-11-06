library(NAMCr)
library(sf)
boxnum<-7621
#query the box in question
x<-query(
  api_endpoint = "sites",
  args = list(boxIds = boxnum))
#special case for CA
x<-x[x$sampleId %in% c(213513),]

#edit to make more robust for regio code (state). Will just need an ifelse statement for abbreviations.

sheds<-list()
for(i in 1:nrow(x)){


ws<-streamstats::delineateWatershed(xlocation = x$longitude[i],
                                    ylocation = x$latitude[i],
                                    includefeatures = 'true',includeparameters = 'false',
                                    includeflowtypes = 'false',
                                    crs=4326,rcode = 'WY')
print('iteration done')
sheds[[i]]<-ws

}
streamstats::leafletWatershed(sheds[[4]])

setwd('C://Users//andrew.caudillo//Box//NAMC//GIS//Watersheds//streamstats_R')


for(i in 1:length(sheds)){
  streamstats::writeShapefile(watershed = sheds[[i]], layer = paste(x$siteId[i],'shed',sep='_'), dir = "ws_shp", what = "boundary")
}

#add in routine that combines them into one shapefile here
#likely will need to read them back in, merge, then write again.
#because streamstats makes them a "watershed" file, not a shape.

setwd('C://Users//andrew.caudillo//Box//NAMC//GIS//Watersheds//streamstats_R//ws_shp')
filenames <- list.files(pattern='*.shp')
filepaths <-filenames

# Read each shapefile and return a list of sf objects
listOfShp <- lapply(filepaths, st_read)

# Look to make sure they're all in the same CRS
unique(sapply(listOfShp, st_crs))

# Combine the list of sf objects into a single object
combinedShp <- do.call(what = sf:::rbind.sf, args=listOfShp)

yy<-do.call(rbind,sheds)

plot(combinedShp$geometry)

nhdsheds<-read.csv('C://Users//andrew.caudillo//Box//NAMC//GIS//Watersheds//Mastersheds//NHDsheds.csv')

samp_info<-query(api_endpoint = 'samples',
      args=list(siteIds=nhdsheds$NHDsheds))
samp_info<-samp_info[samp_info$boxId>5000,]
table(samp_info$boxId)
