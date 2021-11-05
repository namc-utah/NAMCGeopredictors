library(sf)
library(dplyr)
AIM_2020=st_read('Z:/GIS/GIS_Stats/MasterSheds/AIM2020Sheds/All2020Sheds.shp')
AIM_2019=st_read("Z:/GIS/GIS_Stats/MasterSheds/AIM2019Sheds/Wsheds_Total_before07_Merge.shp")
AIM_2018=st_read("Z:/GIS/GIS_Stats/MasterSheds/AIM2018_Sheds.shp")
Nicole_2014=st_read("Z:/GIS/GIS_Stats/MasterSheds/Nicolesheds_9_18_14.shp")
older_AIM=st_read("Z:/GIS/GIS_Stats/MasterSheds/MasterSheds30August2018.shp")

AIM_2019=st_make_valid(AIM_2019)
AIM_2020=st_make_valid(AIM_2020)
AIM_2018=st_make_valid(AIM_2018)
Nicole_2014=st_make_valid(Nicole_2014)
older_AIM=st_make_valid(older_AIM)


lookup=read.csv("C:/Users/jenni/OneDrive - USU/Documents/AIM_site_lookup_for_watersheds.csv")
lookup$UID=lookup$VisitId

Nicole_2014$Station=Nicole_2014$SiteCode
NicoleJoin=left_join(Nicole_2014,lookup, by='Station')
NicoleJoin=NicoleJoin[,c('SampleID','newsiteID.or.station','UID.y')]
NicoleJoin=subset(NicoleJoin,is.na(SampleID)==FALSE)
NicoleJoin$UID=NicoleJoin$UID.y
NicoleJoin=NicoleJoin[,c('SampleID','newsiteID.or.station','UID')]

AIM_2018$Station=AIM_2018$reachid
AIM_2018Join=left_join(AIM_2018,lookup, by='Station')
AIM_2018Join=AIM_2018Join[,c('SampleID','newsiteID.or.station','UID.x')]
AIM_2018Join$UID=AIM_2018Join$UID.x
AIM_2018Join=left_join(AIM_2018Join,lookup, by='UID')
AIM_2018Join$SampleID=ifelse(is.na(AIM_2018Join$SampleID.x)==TRUE,AIM_2018Join$SampleID.y,AIM_2018Join$SampleID.x)
AIM_2018Join$newsiteID.or.station=ifelse(is.na(AIM_2018Join$newsiteID.or.station.x)==TRUE,AIM_2018Join$newsiteID.or.station.y,AIM_2018Join$newsiteID.or.station.x)
AIM_2018Join=AIM_2018Join[,c('SampleID','newsiteID.or.station','UID')]

AIM_2019$UID=AIM_2019$OLD_UIDT
AIM_2019Join=left_join(AIM_2019,lookup,by='UID')
AIM_2019Join=AIM_2019Join[,c('SampleID','newsiteID.or.station','UID')]

AIM_2020Join=left_join(AIM_2020,lookup,by='UID')
AIM_2020Join=AIM_2020Join[,c('SampleID','newsiteID.or.station','UID')]
AIM_list=read.csv("C:/Users/jenni/OneDrive - USU/Documents/AIM_list.csv")
AIM_2020Join=left_join(AIM_2020Join,AIM_list, by='UID')
AIM_2020Join$newsiteID.or.station=ifelse(is.na(AIM_2020Join$newsiteID.or.station)==TRUE,AIM_2020Join$Station,AIM_2020Join$newsiteID.or.station)
AIM_2020Join=AIM_2020Join[,c('SampleID','newsiteID.or.station','UID')]

older_AIMJoin=left_join(older_AIM,lookup, by='UID')
older_AIMJoin=older_AIMJoin[,c('SampleID','newsiteID.or.station','UID','REACHID','SAMPLEID','STATION','Station','SITECODE')]
older_AIMJoin$SampleID=ifelse(is.na(older_AIMJoin$SampleID)==TRUE,older_AIMJoin$SAMPLEID,older_AIMJoin$SampleID)
older_AIMJoin$newsiteID.or.station=ifelse(older_AIMJoin$SampleID==0,older_AIMJoin$STATION,older_AIMJoin$newsiteID.or.station)
older_AIMJoin=older_AIMJoin[,c('SampleID','newsiteID.or.station','UID')]
older_AIMJoin$SampleID=ifelse(older_AIMJoin$SampleID==0,NA,older_AIMJoin$SampleID)
older_AIMJoin=subset(older_AIMJoin,is.na(older_AIMJoin$SampleID)==FALSE|is.na(older_AIMJoin$newsiteID.or.station)==FALSE)



Allwatersheds=rbind(AIM_2020Join,AIM_2018Join,AIM_2019Join,older_AIMJoin,NicoleJoin)
rm(AIM_2020Join,AIM_2018Join,AIM_2019Join,older_AIMJoin)
rm(AIM_2020,AIM_2018,AIM_2019,older_AIM,NicoleJoin,Nicole_2014)

AllwatershedsSub=subset(Allwatersheds,newsiteID.or.station %in% c('VE-LS-15565',	'VE-SS-13005',	'VE-SS-10556',	'PR-LS-11260',	'VE-RV-10397',	'PR-RV-10684',	'VE-LS-10589',	'PR-SS-10044',	'PR-RV-13948',	'VE-RV-11357',	'VE-LS-11577',	'VE-RV-12637',	'PR-RV-13820',	'PR-RV-12988',	'PR-LS-14012',	'PR-RV-10940',	'PR-LS-11836',	'PR-RV-13180',	'PR-RV-11132',	'PR-RV-10876',	'VE-RV-11165',	'VE-RV-13213',	'PR-RV-11004',	'PR-RV-10124',	'PR-SS-13036',	'PR-LS-13628',	'PR-LS-13308',	'PR-RV-10492',	'VE-SS-19433',	'VE-LS-14685',	'PR-RV-13756',	'VE-LS-15517',	'VE-RV-13149',	'PR-LS-11596',	'PR-RV-13644',	'VE-LS-15673',	'PR-RV-13900',	'PR-RV-10428',	'VE-SS-12845',	'PR-RV-10172',	'VE-RV-10845',	'VE-LS-12445',	'PR-RV-13388',	'VE-RV-12381',	'VE-SS-15337',	'VE-RV-10797',	'PR-SS-10108',	'PR-RV-10572',	'VE-SS-12601',	'VE-SS-21037',	'VE-RV-10333',	'PR-SS-13372',	'VE-SS-16941',	'PR-RV-10300',	'VE-RV-12189') )
st_write(Allwatersheds,dsn='C:/Users/jenni/OneDrive - USU/Documents',layer='all_watersheds6',driver='ESRI Shapefile')
Allwatersheds$duplciate=duplicated(st_geometry(Allwatersheds))
