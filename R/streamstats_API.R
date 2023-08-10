library(NAMCr)
library(sf)
boxnum<-4453
#query the box in question
x<-query(
  api_endpoint = "samples",
  args = list(boxId = boxnum))
#special case for CA
x<-x[x$sampleId %in% c(211231,211233,211234,211235),]

#edit to make more robust for regio code (state). Will just need an ifelse statement for abbreviations.

sheds<-list()
for(i in 1:nrow(x)){


ws<-streamstats::delineateWatershed(xlocation = x$sampleLongitude[i],
                                    ylocation = x$sampleLatitude[i],
                                    includefeatures = 'true',includeparameters = 'false',
                                    includeflowtypes = 'false',
                                    crs=4326,rcode = 'CA')
print('iteration done')
sheds[[i]]<-ws

}
streamstats::leafletWatershed(sheds[[4]])
