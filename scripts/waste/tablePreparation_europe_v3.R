## libraries
library(eurostat)
library(vegan)
library(rgeos)
library(rgdal)
library(tidyr)
library(sf)

# get NUTS geodata
shpNUTS2 <- readOGR("spatial","NUTS2_europe")
names(shpNUTS2)
head(shpNUTS2@data)
plot(shpNUTS2, col = ifelse(shpNUTS2@data$NUTS_ID == "SE12",'red','white'))

## crop specific nutrient file
dfNutrient <- read.csv("datasets/targetCrops_europe.csv")
head(dfNutrient)
names(dfNutrient)[4] <- "kcal"

#### get production file 
dfAgricultureOld <- get_eurostat("apro_cpnhr_h",time_format = "raw")
head(dfAgricultureOld)
unique(dfAgricultureOld$strucpro)
dfAgricultureNew <- get_eurostat("apro_cpnhr",time_format = "raw")
head(dfAgricultureNew)
unique(dfAgricultureNew$strucpro)

dfAgriculture <- rbind(dfAgricultureOld,dfAgricultureNew)

head(dfAgriculture)
head(dfAgriculture)
unique(dfAgriculture$crops)
unique(dfAgriculture$strucpro)
unique(dfAgriculture$geo)
unique(dfAgriculture$time)
dfAgriculture$time <- as.numeric(dfAgriculture$time) # change year to numeric

###### harvested areas and production for target period
# only include harvested area (AR; 1000ha) and production 
dfAgriculture <- dfAgriculture[which(dfAgriculture$strucpro%in%c("AR","PR")&
                                       dfAgriculture$time>=1977&dfAgriculture$time<=2016),]
head(dfAgriculture)

# remove NA
dfAgriculture <- dfAgriculture[which(!is.na(dfAgriculture$values)),]

# change structure
dfAgriculturer <- dfAgriculture %>% spread(strucpro, values)
head(dfAgriculturer)

## only keep crops where calorie data is available
dfNutrient <- dfNutrient[which(!is.na(dfNutrient$kcal)),c("crops","kcal")]
dfAgricultureReduced <- merge(dfAgriculturer,dfNutrient[,c("crops","kcal")],by="crops")
head(dfAgricultureReduced)
length(unique(dfAgricultureReduced$crops)) # 29 final crops 

## change production to calories (production to tons: x1000)
dfAgricultureReduced$PR <- dfAgricultureReduced$PR*1000*dfAgricultureReduced$kcal
# keep necessary columns only 
dfAgricultureReduced <- dfAgricultureReduced[,c("crops","geo","time","AR","PR")]

## make area and production consistent
# set 0 areas to NA, set produciton in NA areas to NA
dfAgricultureReduced[which(dfAgricultureReduced$AR<=0),"AR"] <- NA
dfAgricultureReduced[which(is.na(dfAgricultureReduced$AR)),"PR"] <- NA
# set production to 0 where area is reported
dfAgricultureReduced[which(dfAgricultureReduced$AR> 0 & is.na(dfAgricultureReduced$PR)),"PR"] <- 0


## reduce dataset: only include crops with at least 15 entries per region, remove 0 at the beginning and end of time series!; maximize  number of crops and years per time period
names(dfAgricultureReduced)
vecGeo <- unique(dfAgricultureReduced$geo)
# iterate through each region
lsRelevant <- lapply(vecGeo,function(g){
  show(as.character(g))
  ## subset area
  dfGeo <- dfAgricultureReduced[which(dfAgricultureReduced$geo==g),]
  
  ## iterate through crops
  lsCrops <- lapply(unique(dfGeo$crops),function(c){
    # show(as.character(c))
    dfCrop <- dfGeo[which(dfGeo$crops==c),]
    # fill empty rows (missing years)
    dfCrop <- merge(data.frame(time=1977:2016,crops=c, geo=g),dfCrop,all.x=T)
    # only consider crops with at least 15 data points for detrending
    if (sum(!is.na(dfCrop$PR))>=15&sum(dfCrop$PR>0,na.rm=T)>0){
      minYear <- min(dfCrop[which(dfCrop$PR>0),"time"])
      maxYear <- max(dfCrop[which(dfCrop$PR>0),"time"])
      dfCrop[which(dfCrop$time<minYear),c("AR","PR")] <- NA
      dfCrop[which(dfCrop$time>maxYear),c("AR","PR")] <- NA
      dfCrop[which(dfCrop$time>minYear&is.na(dfCrop$PR)&dfCrop$time<maxYear),"PR"] <- 0
      # detrend production
      if (sum(!is.na(dfCrop$PR))>=15){
        dfCrop$PR_Det <- NA
        dfCrop[which(!is.na(dfCrop$PR)),"PR_Det"] <-resid(loess(PR ~ time,data= dfCrop))
        # dfCrop[which(!is.na(dfCrop$PR)),]
        dfCrop
      }
    }
  })  

  dfCrops <- do.call(rbind,lsCrops)
  if (!is.null(dfCrops))
  {
    lsWindow <- lapply(seq(1977,2009,8),function(yearStart){
      ## keep maximum number of crops if at least 5 years are covered
      # moving window 
      lsWidth <- lapply(5:8,function(w){
        # show(w)
        dfCropsR <- dfCrops[which(dfCrops$time>=yearStart&dfCrops$time<=(yearStart+7)),c("time","crops","PR")] %>% spread(time,PR)
        by <- 8-(w-1)
        lsPos <- lapply(1:by,function(p){
          # show(p)
          noCrop <- sum(apply(dfCropsR[,(1+p):(1+p+w-1)],1,function(r){sum(!is.na(r))==w}))
          data.frame(width = w, year = (yearStart+p-1), num = noCrop)
        })
        do.call(rbind,lsPos)
      })
      dfFreq <- do.call(rbind,lsWidth)
      # get time window with maximum number of crops, then largest time periods and minimum starting years
      maxN <- max(dfFreq$num)
      maxW <- max(dfFreq[which(dfFreq$num==maxN),"width"])
      minYear <- min(dfFreq[which(dfFreq$num==maxN&dfFreq$width==maxW),"year"])
      maxYear <- minYear+maxW-1
      
      # remove crops with NA in focal time frame 
      dfFocal <- dfCrops[which(dfCrops$time>=minYear&dfCrops$time<=maxYear),]
      unfocalCrops <- unique(dfFocal[which(is.na(dfFocal$PR)),"crops"])
      dfCropsTotal <- dfFocal[which(!dfFocal$crops%in%unfocalCrops&!is.na(dfFocal$PR)),]
      dfCropsTotal
    })
    do.call(rbind,lsWindow)
  }
})
dfRelevant <- do.call(rbind,lsRelevant)
nrow(dfRelevant)/nrow(dfAgricultureReduced) # 40% of original data
head(dfRelevant)
sort(as.character(unique(dfRelevant$geo)))

sum(is.na(dfRelevant$AR))
sum(is.na(dfRelevant$PR))
# set NA areas to 0 (this is where we have 0 production within detrend window)
dfRelevant[which(is.na(dfRelevant$AR)),"AR"] <- 0

length(unique(dfRelevant$crops)) ##29 crops

# change names
names(dfRelevant)[4:6] <- c("area","productionCal","productionCalDet")


# keep relevant regions (highest resolution and with most entries)
dfRelevant$geo <- as.character(dfRelevant$geo)
sort(unique(dfRelevant$geo))
sum(dfRelevant$geo=="BE1")
sum(dfRelevant$geo=="BE10")
sum(dfRelevant$geo=="CY")
sum(dfRelevant$geo=="CY0")
sum(dfRelevant$geo=="CY00")
sum(dfRelevant$geo=="DK")
sum(dfRelevant$geo=="DK0")
sum(dfRelevant$geo=="EE")
sum(dfRelevant$geo=="EE0")
sum(dfRelevant$geo=="EL3")
sum(dfRelevant$geo=="EL30")
sum(dfRelevant$geo=="ES3")
sum(dfRelevant$geo=="ES30")
sum(dfRelevant$geo=="ES7")
sum(dfRelevant$geo=="ES70")
sum(dfRelevant$geo=="FR1")
sum(dfRelevant$geo=="FR10")
sum(dfRelevant$geo=="FR3")
sum(dfRelevant$geo=="FR30")
sum(dfRelevant$geo=="HU1")
sum(dfRelevant$geo=="HU10")
sum(dfRelevant$geo=="LT")
sum(dfRelevant$geo=="LT0")
sum(dfRelevant$geo=="LU")
sum(dfRelevant$geo=="LU0")
sum(dfRelevant$geo=="LV")
sum(dfRelevant$geo=="LV0")
sum(dfRelevant$geo=="MT")
sum(dfRelevant$geo=="MT0")
sum(dfRelevant$geo=="MT00")
sum(dfRelevant$geo=="PT2")
sum(dfRelevant$geo=="PT20")
sum(dfRelevant$geo=="PT3")
sum(dfRelevant$geo=="PT30")
sum(dfRelevant$geo=="SI")
sum(dfRelevant$geo=="SI0")
sum(dfRelevant$geo=="UKH")
sum(dfRelevant$geo=="UKH1")
sum(dfRelevant$geo=="UKN")
sum(dfRelevant$geo=="UKN0")


## only keep subnational resolution
dfRelevantRed <- dfRelevant[which(dfRelevant$geo%in% 
                                    c("AT11","AT12","AT13","AT21","AT22","AT31","AT32","AT33","AT34",
                                      "BE10","BE21","BE22","BE23","BE24","BE25","BE31","BE32","BE33","BE34","BE35",
                                      "CZ01","CZ02","CZ03","CZ04","CZ05","CZ06","CZ07","CZ08",
                                      "DE1","DE2","DE4","DE5","DE6","DE7","DE8","DE9","DEA","DEB","DEC","DED","DEE","DEF","DEG", # only few entries with higher resolution
                                      "EL11","EL12","EL13","EL14","EL21","EL22","EL23","EL24","EL25","EL30","EL41","EL42","EL43","EL51","EL52","EL53","EL54","EL61","EL62","EL63","EL64","EL65",
                                      "ES11","ES12","ES13","ES21","ES22","ES23","ES24","ES30","ES41","ES42","ES43","ES51","ES52","ES53","ES61","ES62","ES70",
                                      "FI1","FI2", # only few entries in higher resolution
                                      "FR10","FR21","FR22","FR23","FR24","FR25","FR26","FR30","FR41","FR42","FR43","FR51","FR52","FR53","FR61","FR62","FR63","FR71","FR72","FR81","FR82","FR83","FR91","FR92","FR93","FR94",
                                      "HU1","HU21","HU22","HU23","HU31","HU32","HU33", # HU1 has more entries than HU10
                                      "IE01","IE02",
                                      "ITC1","ITC2","ITC3","ITC4","ITD5","ITE3","ITF1","ITF2","ITF3","ITF4","ITF5","ITF6","ITG1","ITG2","ITH3","ITH4","ITI1","ITI2","ITI4",
                                      "NL11","NL12","NL13","NL21","NL22","NL23","NL31","NL32","NL33","NL34","NL41","NL42",
                                      "PL11","PL12","PL21","PL22","PL31","PL32","PL33","PL34","PL41","PL42","PL43","PL51","PL52","PL61","PL62","PL63",
                                      "PT11","PT15","PT16","PT17","PT18","PT20","PT30",
                                      "RO11","RO12","RO21","RO22","RO31","RO32","RO41","RO42",
                                      "SE11","SE12","SE21","SE22","SE23","SE31","SE32","SE33",
                                      "SK01","SK02","SK03","SK04",
                                      "UKC","UKD","UKE","UKF","UKG","UKH","UKJ","UKK","UKL","UKM","UKN")),] # no higher resolution

nrow(dfRelevantRed)/nrow(dfAgricultureReduced) # 22% of original data
sort(as.character(unique(dfRelevantRed$geo)))

#### get spatial resolution
sort(as.character(unique(shpNUTS2@data$NUTS_ID)))
sort(as.character(unique(dfRelevantRed$geo)))

unique(dfRelevantRed[-which(dfRelevantRed$geo%in%shpNUTS2@data$NUTS_ID),"geo"])
# remove non matching regions of higher resolution
dfRelevantRed <- dfRelevantRed[-which(dfRelevantRed$geo%in%c("ITD5","ITE3","FR93","FR94","FR91","FR92",
                                                             "EL11","EL12","EL13","EL14","EL21","EL22","EL23","EL24","EL25")),]

# add every row to closest resolution
dfNUTS2 <- shpNUTS2@data
dfGeo <- dfNUTS2
dfGeo$geo <- NA
r <- 0
repeat {
  r <- r +1 
  if (dfGeo[r,"NUTS_ID"]%in%unique(dfRelevantRed$geo))
  {
    dfGeo[r,"geo"] <- as.character(dfGeo[r,"NUTS_ID"])
  }
  if (is.na(dfGeo[r,"geo"])&substr(dfGeo[r,"NUTS_ID"],1,3)%in%unique(dfRelevantRed$geo))
  {
    dfGeo[r,"geo"]<- substr(as.character(dfGeo[r,"NUTS_ID"]),1,3)
  }  
  if (is.na(dfGeo[r,"geo"])&substr(dfGeo[r,"NUTS_ID"],1,2)%in%unique(dfRelevantRed$geo))
  {
    dfGeo[r,"geo"] <- substr(as.character(dfGeo[r,"NUTS_ID"]),1,2)
  }    
  if (r==nrow(dfGeo))
    break;
}
head(dfGeo)
nrow(dfGeo)
dfGeo <- dfGeo[order(dfGeo$NUTS_ID),]
dfGeo[,c("NUTS_ID","geo")]
dfGeo <- unique(dfGeo[!is.na(dfGeo$geo),c("NUTS_ID","geo")])

## only keep mathing entries
dfRelevantFinal <- dfRelevantRed[which(dfRelevantRed$geo%in%unique(dfGeo$geo)),]
head(dfRelevantFinal)
sort(as.character(unique(dfRelevantFinal$geo)))
length(unique(dfRelevantFinal$geo))
nrow(dfRelevantFinal)/nrow(dfAgricultureReduced) # 23% of original data

## export necessary geoinfo
nrow(dfGeo)
shpNUTS2 <- merge(shpNUTS2,dfGeo,by="NUTS_ID")
sum(shpNUTS2@data$NUTS_ID==shpNUTS2@data$geo,na.rm=T)
## disolve shapefile by availabe resolution
shpGeo <- gUnaryUnion(shpNUTS2, id = shpNUTS2@data$geo)
plot(shpGeo)

IDs <- data.frame(NUTS_ID=sapply(slot(shpGeo, "polygons"), function(x) slot(x, "ID")))
rownames(IDs)  <- IDs$NUTS_ID
shpGeo <- SpatialPolygonsDataFrame(shpGeo,IDs)
sort(as.character(shpGeo@data$NUTS_ID))
plot(shpGeo, col = ifelse(shpGeo@data$NUTS_ID == "SE12",'red','white'))
plot(shpGeo, col = ifelse(shpGeo@data$NUTS_ID == "CY",'red','white'))

writeOGR(shpGeo, dsn = 'spatial', layer = 'regions_europe', driver = "ESRI Shapefile",overwrite_layer = T)
#### go to arcgis and intersect with polygonClimateID (Data/DataPreparation folder), project to WGS_1984_World_Mercator and calculate area in ha ("area" field (float))

#### total production area per region
dfProductionTotal <- aggregate(productionCal~geo+time,dfRelevantFinal,sum)
head(dfProductionTotal)
range(dfProductionTotal$time)

#### harvested area 
dfHA <- dfRelevantFinal[,c("geo","crops","time","area")]
dfHA$harvesedAreaHectares <- dfHA$area*1000 # convert to hectares

#### total harvested area per region
dfHAtotal <- aggregate(harvesedAreaHectares~geo+time,dfHA,sum)

#### calculate total yields
length(unique(dfProductionTotal$geo))
length(unique(dfHAtotal$geo))

## get total yields
dfYield <- merge(dfProductionTotal,dfHAtotal,all.y=T)
head(dfYield)
dfYield[is.na(dfYield)] <- 0 # set na to 0 (e.g. no production)

dfYield$yield <- dfYield$productionCal/dfYield$harvesedAreaHectares
sum(is.na(dfYield$yield))
# set NA to 0
dfYield[which(is.na(dfYield$yield)),"yield"] <- 0
head(dfYield)
dfYield <- dfYield[,c("geo","time","productionCal","harvesedAreaHectares","yield")]

#### crop diversity per region and year
dfDiversity <- aggregate(harvesedAreaHectares~geo+time,dfHA,function(i){diversity(i, index = "shannon")})
names(dfDiversity)[3] <- "shannon"
dfDiversity$effectiveDiversity <- exp(dfDiversity$shannon)
head(dfDiversity)



###### climate
# intersect file
shpClimateID <- readOGR("spatial","regionsClimateID_europe")
head(shpClimateID@data)
dfClimateID <- shpClimateID@data
dfClimateIDsum <- aggregate(area~cellID,dfClimateID,function(i){sum(i,na.rm=T)})
names(dfClimateIDsum)[2] <- "areaTot"
dfClimateID <- merge(dfClimateID,dfClimateIDsum,by="cellID")
head(dfClimateID)

# climate file
load("datasetsDerived/climate_global.RData")
names(dfClimateFinalPrint)
head(dfClimateFinalPrint)
dfClimateFinal <- dfClimateFinalPrint[,c(1,34:121)]

# merge
dfClimateFinalArea <- merge(dfClimateID,dfClimateFinal,by="cellID")
head(dfClimateFinalArea)
length(unique(dfClimateFinalArea$cellID)) / nrow(dfClimateFinalArea)
# get cropland by region
dfCroplandTot <- aggregate(cbind(cropland1980AD,cropland1990AD,cropland2000AD,cropland2010AD)~NUTS_ID,dfClimateFinalArea,sum)
head(dfCroplandTot)
names(dfCroplandTot)[2:5] <- paste0(names(dfCroplandTot)[2:5],"Tot")
dfClimateFinalArea <- merge(dfClimateFinalArea,dfCroplandTot,by="NUTS_ID")
head(dfClimateFinalArea)
names(dfClimateFinalArea)
## weighted average: get are of cropland attributed to a cell segment (i.e. cropland area multiplied by area share of the segment), divide it by total cropland area across region
dfClimateFinalArea$weight1 <- (((dfClimateFinalArea$area/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland1980AD)/dfClimateFinalArea$cropland1980ADTot)
dfClimateFinalArea$weight2 <- (((dfClimateFinalArea$area/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland1990AD)/dfClimateFinalArea$cropland1990ADTot)
dfClimateFinalArea$weight3 <- (((dfClimateFinalArea$area/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland1990AD)/dfClimateFinalArea$cropland1990ADTot)
dfClimateFinalArea$weight4 <- (((dfClimateFinalArea$area/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland2000AD)/dfClimateFinalArea$cropland2000ADTot)
dfClimateFinalArea$weight5 <- (((dfClimateFinalArea$area/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland2010AD)/dfClimateFinalArea$cropland2010ADTot)
hist(dfClimateFinalArea$weight5)

## multiply weight by temeperature values
names(dfClimateFinalArea)
dfClimateFinalArea[,7:22] <- dfClimateFinalArea[,7:22]*dfClimateFinalArea$weight1
dfClimateFinalArea[,23:38] <- dfClimateFinalArea[,23:38]*dfClimateFinalArea$weight2
dfClimateFinalArea[,39:54] <- dfClimateFinalArea[,39:54]*dfClimateFinalArea$weight3
dfClimateFinalArea[,55:70] <- dfClimateFinalArea[,55:70]*dfClimateFinalArea$weight4
dfClimateFinalArea[,71:86] <- dfClimateFinalArea[,71:86]*dfClimateFinalArea$weight5

# sum weighted values to get overall weighted average
names(dfClimateFinalArea)
dfClimateFinalAreaAgg <- aggregate(dfClimateFinalArea[,7:86],by=list(dfClimateFinalArea$NUTS_ID),FUN=function(i){sum(i,na.rm=T)})
head(dfClimateFinalAreaAgg)
names(dfClimateFinalAreaAgg)[1] <- "NUTS_ID"
min(dfClimateFinalAreaAgg[2:81]) # check for negative values -> would be problematic for instability calculation
# dfClimateFinalAreaAgg$neg <- apply(dfClimateFinalAreaAgg[,2:81],1,function(r){sum(r<0)})
sum(dfClimateFinalAreaAgg[2:81]<0)
# dfClimateFinalAreaAgg <- dfClimateFinalAreaAgg[which(dfClimateFinalAreaAgg$neg==0),1:81]


# change structure
names(dfClimateFinalAreaAgg)
dfClimateFinalr <- dfClimateFinalAreaAgg %>% gather(climYear, Value, names(dfClimateFinalAreaAgg)[2:81])
head(dfClimateFinalr)
dfClimateFinalr$Year <- as.numeric(substr(dfClimateFinalr$climYear,9,12))
dfClimateFinalr$Element <- substr(dfClimateFinalr$climYear,1,8)
dfClimateFinalr <- dfClimateFinalr[,c("NUTS_ID","Year","Element","Value")]
dfClimateFinalr <- dfClimateFinalr %>% spread(Element, Value)
head(dfClimateFinalr)
nrow(dfClimateFinalr)
nrow(unique(dfClimateFinalr[,c("NUTS_ID","Year")])) == nrow(dfClimateFinalr) # check duplicates



#### combine all datasets
names(dfYield) <- c("NUTS_ID","Year","Production","AreaHarvested","Yield")
dfRelevantFinal <- dfRelevantFinal[,c("geo","time","crops","productionCalDet")]
names(dfRelevantFinal) <- c("NUTS_ID","Year","Crop","ProductionDet")
dfDiversity <- dfDiversity[,c("geo","time","effectiveDiversity")]
names(dfDiversity) <- c("NUTS_ID","Year","diversity")
names(dfClimateFinalr)

vecRegionFinal <- Reduce(intersect,list(dfYield$NUTS_ID,dfRelevantFinal$NUTS_ID,dfDiversity$NUTS_ID,dfClimateFinalr$NUTS_ID))


# calculate global detrended production & Yield
head(dfYield)
dfProductionEurope <- aggregate(cbind(Production,AreaHarvested)~Year,dfYield[which(dfYield$NUTS_ID%in%vecRegionFinal),],sum)
dfProductionEurope$ProductionDet <- resid(loess(Production ~ Year,data=dfProductionEurope))
dfProductionEurope$Yield <- dfProductionEurope$Production/dfProductionEurope$AreaHarvested


# calculate regional detrended production
head(dfYield)
dfYield$Country <- substr(dfYield$NUTS_ID,1,2)
dfProductionCountry <- aggregate(cbind(Production,AreaHarvested)~Year+Country,dfYield[which(dfYield$NUTS_ID%in%vecRegionFinal),],sum)
lsProductionCountry <- lapply(unique(dfProductionCountry$Country),function(c){
  dfCountry <- dfProductionCountry[which(dfProductionCountry$Country==c),]
  dfCountry$ProductionDet <- resid(loess(Production ~ Year,data=dfCountry))
  dfCountry$Yield <- dfCountry$Production/dfCountry$AreaHarvested
  dfCountry
})
dfProductionCountry <- do.call(rbind,lsProductionCountry)



## summarize per time frame 
lsAll <- lapply(vecRegionFinal,function(g){
  # total production
  show(as.character(g))
  dfProductionRegion <-  dfRelevantFinal[which(dfRelevantFinal$NUTS_ID==g),c("NUTS_ID","Crop","Year","ProductionDet")]
  dfProductionSumRegion <- dfYield[which(dfYield$NUTS_ID==g),]
  dfProductionSumRegion$ProductionDet <- resid(loess(Production ~ Year,data=dfProductionSumRegion))
  dfYieldRegion <- dfYield[which(dfYield$NUTS_ID==g),]
  dfDiversityRegion <- dfDiversity[which(dfDiversity$NUTS_ID==g),]
  dfClimateRegion <- dfClimateFinalr[which(dfClimateFinalr$NUTS_ID==g),]
  ctry <- substr(g,1,2)
  
  lsAggregate <- lapply(c(1977,1985,1993,2001,2009),function(yearStart){
    # print(yearStart)
    # global cv
    dfSummary <- data.frame(Area=g, timePeriod= yearStart)
    dfSummary$cvG <- sd(dfProductionEurope[which(dfProductionEurope$Year>=yearStart&dfProductionEurope$Year<=(yearStart+7)),"ProductionDet"],na.rm=T)/mean(dfProductionEurope[which(dfProductionEurope$Year>=yearStart&dfProductionEurope$Year<=(yearStart+7)),"Production"],na.rm=T)
    dfSummary$cvR <- sd(dfProductionCountry[which(dfProductionCountry$Country==ctry&dfProductionCountry$Year>=yearStart&dfProductionCountry$Year<=(yearStart+7)),"ProductionDet"],na.rm=T)/mean(dfProductionCountry[which(dfProductionCountry$Country==ctry&dfProductionCountry$Year>=yearStart&dfProductionCountry$Year<=(yearStart+7)),"Production"],na.rm=T)
    dfSummary$cvL <- sd(dfProductionSumRegion[which(dfProductionSumRegion$Year>=yearStart&dfProductionSumRegion$Year<=(yearStart+7)),"ProductionDet"],na.rm=T)/mean(dfProductionSumRegion[which(dfProductionSumRegion$Year>=yearStart&dfProductionSumRegion$Year<=(yearStart+7)),"Production"],na.rm=T)
    
    dfSummary$yieldG <- mean(dfProductionEurope[which(dfProductionEurope$Year>=yearStart&dfProductionEurope$Year<=(yearStart+7)),"Yield"],na.rm=T)
    dfSummary$yieldR <- mean(dfProductionCountry[which(dfProductionCountry$Country==ctry&dfProductionCountry$Year>=yearStart&dfProductionCountry$Year<=(yearStart+7)),"Yield"],na.rm=T)
    dfSummary$yieldL <- mean(dfYieldRegion[which(dfYieldRegion$Year>=yearStart&dfYieldRegion$Year<=(yearStart+7)),"Yield"],na.rm=T)
    
    dfSummary$diversity <- mean(dfDiversityRegion[which(dfDiversityRegion$Year>=yearStart&dfDiversityRegion$Year<=(yearStart+7)),"diversity"],na.rm=T)
    
    # asynchrony
    dfProductionRegionTime <- dfProductionRegion[which(dfProductionRegion$Year>=yearStart&dfProductionRegion$Year<=(yearStart+7)),]
    noCrop <- length(unique(dfProductionRegionTime$Crop))
    dfSummary$asynchrony <- NA
    if (noCrop == 1) {  dfSummary$asynchrony <- 0}
    if (noCrop > 1) {
      dfProductionRegionTimer <- dfProductionRegionTime %>% spread(Crop, ProductionDet)
      det.w.dec <- dfProductionRegionTimer[,3:ncol(dfProductionRegionTimer)]
      diff.cov <- cov(data.frame(det.w.dec))
      diag.cov <- diag(diff.cov)
      sqrt.diag.cov <- sqrt(diag.cov)
      diff.var.glob <-  sum(diff.cov)
      diff.sd.local <- sum(sqrt.diag.cov)
      diff.async <- 1-diff.var.glob/(diff.sd.local^2) #asynchrony
      dfSummary$asynchrony <- diff.async
    }
    dfSummary$instabilityTemp <- -(mean(dfClimateRegion[which(dfClimateRegion$Year>=yearStart&dfClimateRegion$Year<=(yearStart+7)),"meanTemp"],na.rm=T)/sd(dfClimateRegion[which(dfClimateRegion$Year>=yearStart&dfClimateRegion$Year<=(yearStart+7)),"meanTemp"],na.rm=T))
    dfSummary$instabilityPrec <- -(mean(dfClimateRegion[which(dfClimateRegion$Year>=yearStart&dfClimateRegion$Year<=(yearStart+7)),"meanPrec"],na.rm=T)/sd(dfClimateRegion[which(dfClimateRegion$Year>=yearStart&dfClimateRegion$Year<=(yearStart+7)),"meanPrec"],na.rm=T))
    na.omit(dfSummary)
  })
  do.call(rbind,lsAggregate)
})
dfAll <- do.call(rbind,lsAll)
head(dfAll)
nrow(unique(dfAll[,c("Area","timePeriod")])) == nrow(dfAll) # check duplicates
hist(dfAll$asynchrony)
unique(dfAll$timePeriod)
head(dfAll)

# check NA
sum(is.na(dfAll))
## omit NA
dfAll <- na.omit(dfAll)
nrow(dfAll) ## 751 data points
dfAll <- dfAll[which(dfAll$yieldL>0),]
length(unique(dfAll$Area)) ## 187 regions

# ratio CV
dfAll$ratioStabilityG <- dfAll$cvG/dfAll$cvL
dfAll$ratioStabilityR <- dfAll$cvR/dfAll$cvL

# ratio yield
dfAll$ratioYieldG <- dfAll$yieldL/dfAll$yieldG
dfAll$ratioYieldR <- dfAll$yieldL/dfAll$yieldR

hist(dfAll$ratioStabilityG)
hist(dfAll$ratioStabilityR)
hist(dfAll$ratioYieldG)
hist(dfAll$ratioYieldR)

dfAll$benefitStabilityG <- "winner"
dfAll[which(dfAll$ratioStabilityG<1),"benefitStabilityG"] <- "loser"
table(dfAll$benefitStabilityG)
dfAll$benefitStabilityR <- "winner"
dfAll[which(dfAll$ratioStabilityR<1),"benefitStabilityR"] <- "loser"
table(dfAll$benefitStabilityR)

dfAll$benefitYieldG <- "winner"
dfAll[which(dfAll$ratioYieldG<1),"benefitYieldG"] <- "loser"
table(dfAll$benefitYieldG)
dfAll$benefitYieldR <- "winner"
dfAll[which(dfAll$ratioYieldR<1),"benefitYieldR"] <- "loser"
table(dfAll$benefitYieldR)



### add cntry code and NUTS names
names(dfAll)
names(dfAll)[1] <- "NUTS_ID"

## add NUTS names and country codes and harmonize them
dfAll <- merge(dfAll,shpNUTS2@data[,c("NUTS_ID","NUTS_NAME","CNTR_CODE")],by="NUTS_ID",all.x=T)
head(dfAll)
unique(dfAll[which(is.na(dfAll$NUTS_ID)),"NUTS_ID"])
unique(dfAll[which(is.na(dfAll$NUTS_NAME)),"NUTS_ID"])
levels(dfAll$NUTS_NAME) <- c(levels(dfAll$NUTS_NAME),"Germany","Finnland","Hungary","United Kingdom")
levels(dfAll$CNTR_CODE) <- c(levels(dfAll$CNTR_CODE),"DE","FI","HU","UK")
dfAll[which(substr(dfAll$NUTS_ID,1,2)=="DE"),c("NUTS_NAME")] <- "Germany"
dfAll[which(substr(dfAll$NUTS_ID,1,2)=="DE"),c("CNTR_CODE")] <- "DE"
dfAll[which(substr(dfAll$NUTS_ID,1,2)=="FI"),c("NUTS_NAME")] <- "Finnland"
dfAll[which(substr(dfAll$NUTS_ID,1,2)=="FI"),c("CNTR_CODE")] <- "FI"
dfAll[which(dfAll$NUTS_ID=="HU1"),c("NUTS_NAME")] <- "Hungary"
dfAll[which(dfAll$NUTS_ID=="HU1"),c("CNTR_CODE")] <- "HU"
dfAll[which(substr(dfAll$NUTS_ID,1,2)=="UK"),c("NUTS_NAME")] <- "United Kingdom"
dfAll[which(substr(dfAll$NUTS_ID,1,2)=="UK"),c("CNTR_CODE")] <- "UK"
head(dfAll)
unique(dfAll[which(is.na(dfAll$NUTS_NAME)),"NUTS_ID"])


## add country names consistent with the global dataset
dfCountryCodes <- read.csv("datasets/countryCodes_europe.csv")
head(dfCountryCodes)

dfAll <- merge(dfAll,dfCountryCodes,by="CNTR_CODE")
nrow(dfAll)

## save dataframe
names(dfAll)
dfAll <- dfAll[,c("NUTS_NAME","NUTS_ID","Area","CNTR_CODE","timePeriod","cvG","cvR","cvL","ratioStabilityG","ratioStabilityR","benefitStabilityG","benefitStabilityR",
                  "yieldG","yieldR","yieldL","ratioYieldG","ratioYieldR","benefitYieldG","benefitYieldR",
                  "asynchrony","diversity",
                  "instabilityTemp","instabilityPrec")]
names(dfAll)[1:4] <- c("Region","RegionCode","Country","CountryCode")


write.csv(dfAll, "datasetsDerived/dataFinal_europe_v3.csv",row.names=F)







rm(list=ls())

