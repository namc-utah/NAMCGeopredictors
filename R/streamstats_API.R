library(NAMCr)
library(sf)
boxnum<-4453
#query the box in question
x<-query(
  api_endpoint = "samples",
  args = list(boxId = boxnum))
#special case for CA
x<-x[x$sampleId %in% c(211231,211233,211234,211235),]


sheds<-list()
for(i in 1:nrow(x)){

url<-paste0('https://streamstats.usgs.gov/streamstatsservices/watershed.geojson?',
            'rcode=CA&xlocation=',x$sampleLongitude[i],'&ylocation=',x$sampleLatitude[i],'&crs=4326&includeparameters=false&includeflowtypes=false&includefeatures=true&simplify=false')
sheds[[i]]<-st_read(url)
print('iteration done')
}
