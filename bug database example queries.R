
#update and install package
detach("NAMCr", unload=TRUE)
remotes::install_github("namc-utah/NAMCr", force=TRUE)
#get credentials
query("auth")


#call package and all possible API's
library("NAMCr")
NAMCr::cli()
info("samples")# replace "samples" with desired endpoint to get parameters needed for any endpoint


# query site and samples by project, site, sample, box, customer, or geographic area
samples = query("samples",projectIds=49)
samples =query("samples",boxIds=1603)
sites = query("sites", filter=list( 'DEERDEER-01'))
sites=query("sites",projectIds=49)
boxes=query("boxes",boxIds=1603)
boxInfo=query("boxInfo",boxId=70)
org=query("organizations",searchTerm="BLM")

#create a project
projects = query("projects")
NAMCr::save("createProject",projectName="Intermittent stream",description="Jennifers work with Chuck to develop OE and MMI for intermittent streams")
NAMCr::save("addProjectSamples",projectId=90,sampleIds=c(115217,	115218,	115219,	115222,	115223,	115224,	115226,	115227,	115228,	115230,	115232,	115233,	115238,	115239,	115244,	115246,	115248,	115249,	115250,	115253,	115254,	115255,	115256,	115258,	115260,	115261,	115263,	115266,	115267,	115275,	115279,	115282,	115283,	115285,	115292,	115296,	115306,	115307,	115309,	115310,	115311,	115312,	115324,	115327,	115328,	115329,	115331,	115333,	115334,	115337,	115338,	115341,	115342,	115343,	115345,	115346,	115347,	115358,	115359,	115371,	115379,	115382,	115384,	115385,	115386,	115389,	116066,	116067,	116068,	116071,	116072,	116073,	116074,	116078,	116079,	116080,	116083,	116087,	116088,	116089,	116090,	116110,	116111,	116112,	116118,	116123,	116125,	116128,	116396,	116397,	116402,	116405,	116407,	116412,	116413,	116415,	116416,	116417,	116419,	116420,	116446,	116447,	116448,	116451,	116457,	116458,	116462,	116464,	116465,	116468,	116469,	116482,	116890,	117377,	117380,	117382,	117383,	117387,	117388,	117389,	117931,	117932,	117934,	117939,	117990,	117993,	118568,	118570,	118572,	118573,	118575,	118576,	118581,	118582,	118591,	118592,	118593,	118594,	118596,	118598,	118600,	118601,	118602,	118605,	118613,	118614,	118615,	118616,	118663,	118664,	118665,	118668,	118669,	118671,	118672,	118673,	118675,	118676,	118678,	118679,	118681,	118683,	118684,	118687,	118689,	118690,	118692,	120364,	120370,	120372,	120373,	120376,	120377,	120378,	120382,	120387,	120388,	120389,	120390,	120391,	120392,	120394,	120395,	123842,	128687,	131866,	131890,	131909,	131912,	131913,	131914,	131916,	131918,	131971,	132154,	132158,	132159,	132274,	132275,	132276,	132277,	132282,	132285,	132300,	132313,	132316,	132319,	132323,	132329,	132332,	132335,	132357,	132364,	132843,	132844,	132846,	132852,	132856,	132864,	132866,	132869,	132875,	132880,	132887,	132904,	133060,	133078,	133081,	133385,	133387,	133389,	133390,	133396,	133444,	133446,	133477,	133478,	133498,	133499,	133501,	133525,	133552,	133553,	133572,	133755,	133760,	133766,	133831,	133930,	133932,	133933,	133934,	133983,	134024,	134258,	136716,	136724,	137418,	138566,	140230,	140253,	140281,	142362,	142364,	142500,	142502,	144567,	144582,	144715,	144716,	144770,	144775,	144796,	144806,	144808,	144839,	144860,	144866,	144881,	144890,	144908,	144922,	144932,	144945,	144946,	144958,	144972,	144973,	144976,	144977,	144978,	144984,	144990,	144991,	144994,	144995,	144996,	144998,	145001,	145004,	145048,	145059,	145061,	145098,	145113,	145130,	145131,	145136,	145168,	146267,	148259,	148271,	149888,	151876,	151901,	156110,	171626,	113982,	114492,	114493,	116119,	117070,	118950,	119904,	120436,	122028,	122038,	123771,	123775,	123782,	123873,	123874,	123876,	123879,	123880,	123881,	123882,	123883,	123884,	123885,	123886,	131855,	132278,	132287,	132289,	132302,	132305,	132360,	132372,	132847,	132860,	132865,	132868,	133481,	137218,	140238,	140276,	141634,	142093,	142096,	142110,	142527,	142546,	145927,	146277,	151516,	151940,	152914,	152916,	152922,	156569,	156574,	156576,	163650,	163657,	164424,	164431,	164689,	164707,	164710,	169053,	172213,	111285,	111817,	115235,	118585,	118923,	123878,	124472,	124884,	128672,	130003,	131434,	131677,	131816,	131968,	132286,	132288,	132315,	132365,	132849,	132886,	133221,	133420,	133547,	133548,	140360,	140541,	140545,	140549,	141618,	141987,	142106,	142922,	144548,	145233,	146850,	147012,	147056,	147340,	148155,	150153,	150244,	151091,	151481,	151523,	151956,	155973,	157613,	157759,	158362,	158426,	164313,	164325,	164339,	164422,	164451,	167425,	167557,	167625,	168997,	169052,	169165,	169178,	169180,	169183,	171654))
NAMCr::save("deleteProject",projectId=89)
NAMCr::save("removeProjectSamples",projectId=90, sampleIds=c(115217))
NAMCr::save("addProjectBoxes",projectId=90,boxIds=1)


#model info
models=query("models")
modelInfo=query("modelInfo",modelId=27)
modelConditions=query("modelConditions",modelId=3)
predictors = query("predictors")
modelResults=query("modelResults", sampleIds=150807)


#predictor geoprocessing
siteInfo=query("siteInfo",siteId=100)
sampleInfo=query("sampleInfo",sampleId=152406)
sitePredictorValues = query("sitePredictorValues",siteId=1) # no data for siteId1 need a list of ids in database
samplePredictorValues=query("samplePredictorValues") #need list of samples in database with values
setSitePredictorValue
setSamplePredictorValue
setSiteCatchment


# general taxonomy info
taxonomy = query("taxonomy",searchTerm="coleopt")
taxonomytree= query("taxonomyTree",taxonomyId=c(121))
attributes=query("attributes") #how do we edit these? need to add new taxa table
taxaAttributes=query("taxaAttributes",taxonomyId=69)
translations=query("translations")
translationTaxa=query("translationTaxa",translationId=3)
createTranslation=save("createTranslation", translationName="OTUCODE_test_JC", description="testing API")
setTranslationTaxa=save("setTranslationTaxa") # need to see format of translationTaxa first before I can test this
deleteTranslationTaxa ## need to see format of translationTaxa first before I can test this
setTaxonomy=save("setTaxonomy")
createTaxonomy
deleteTaxonomy

#get bug data
sampleTaxa=query("sampleTaxa",sampleIds=115217)
sampleTaxaAttributes=query("sampleTaxaAttributes",sampleIds=115217)
sampleTaxaInfo=query("sampleTaxaInfo")
sampleTaxaTranslation=query("sampleTaxaTranslation",sampleId=115217,translationId=3)
sampleTaxaTranslationRarefied=query("sampleTaxaTranslationRarefied",sampleId=115217,translationId=3,fixedCount=300)

#getting a dataset with raw data, OTU rolling, and taxonomy attributes so that we can manually calculate metrics
library(dplyr)
raw_bug_data=left_join(sampleTaxa,sampleTaxaTranslation, by='taxonomyId') 
FFG=subset(sampleTaxaAttributes,attributeName=='FFG')
raw_bug_data=left_join(raw_bug_data,FFG, by='taxonomyId')
raw_bug_data=left_join(raw_bug_data,rarefiedOTUTaxa, by='taxonomyId')

# get bug data spatially
library(sf)
tusca=st_read("C:/Users/jenni/OneDrive/Documents/tuscaroraFO.shp")
library(geojsonio)
tusca2<-st_transform(tusca, crs = 4326)
tusca_json2=geojson_json(tusca2)
pointTaxaRaw=query("pointTaxaRaw", latitude=41.731951,longitude=-111.748569,distance=2500)
polygonTaxaRaw=query("polygonTaxaRaw",polygon=tusca_json)

# bug metrics
metrics=query("metrics")
sampleMetrics=query("sampleMetrics",translationId=3,fixedCount=300,sampleIds=c(115217))
rarefiedOTUTaxa=subset(sampleMetrics,metricName=="Rarefied Taxa")
rarefiedOTUTaxa = NAMCr::json.expand(rarefiedOTUTaxa, "metricValue")


# special sample types
fishDiet=query("fishDiet")
fishSamples=query("fishSamples")
massSamples=query("massSamples")
driftSamples=query("driftSamples")
planktonSamples=query("planktonSamples")

