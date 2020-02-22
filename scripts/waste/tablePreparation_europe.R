## libraries
# library(eurostat)
library(vegan)
# library(rgeos)
library(rgdal)
library(tidyr)
# library(sf)

# get NUTS geodata
shpNUTS2 <- readOGR("spatial","NUTS2_europe")
names(shpNUTS2)

#### get production file 
dfAgriculture <- read.csv("datasets/agriculturalProduction_europe.csv")
head(dfAgriculture)
unique(dfAgriculture$strucpro)
range(dfAgriculture$time)

# change structure
dfAgriculturer <- dfAgriculture %>% spread(strucpro, values)
head(dfAgriculturer)


## crop specific nutrient file
dfNutrient <- read.csv("datasets/targetCrops_europe.csv")
head(dfNutrient)

## only keep crops where calorie data is available
dfNutrient <- dfNutrient[which(!is.na(dfNutrient$calories)),c("crops","calories")]
dfAgricultureReduced <- merge(dfAgriculturer,dfNutrient[,c("crops","calories")],by="crops")
head(dfAgricultureReduced)
length(unique(dfAgricultureReduced$crops)) # 29 final crops 

## change production to calories (production to tons: x1000)
dfAgricultureReduced$PR <- dfAgricultureReduced$PR*1000*dfAgricultureReduced$calories
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
    if (sum(!is.na(dfCrop$PR))>=10&sum(dfCrop$PR>0,na.rm=T)>0){
      minYear <- min(dfCrop[which(dfCrop$PR>0),"time"])
      maxYear <- max(dfCrop[which(dfCrop$PR>0),"time"])
      dfCrop[which(dfCrop$time<minYear),c("AR","PR")] <- NA
      dfCrop[which(dfCrop$time>maxYear),c("AR","PR")] <- NA
      dfCrop[which(dfCrop$time>minYear&is.na(dfCrop$PR)&dfCrop$time<maxYear),"PR"] <- 0
      # detrend production
      if (sum(!is.na(dfCrop$PR))>=10){
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
    dfFinal <- do.call(rbind,lsWindow)
    # only keep entries with at least 10 years in total
    if (length(unique(dfFinal$time))>=10)
    {dfFinal}
  }
})
dfRelevant <- do.call(rbind,lsRelevant)
nrow(dfRelevant)/nrow(dfAgricultureReduced) # 48% of original data
head(dfRelevant)
sort(as.character(unique(dfRelevant$geo)))

sum(is.na(dfRelevant$AR))
sum(is.na(dfRelevant$PR))
# set NA areas to 0 (this is where we have 0 production within detrend window)
dfRelevant[which(is.na(dfRelevant$AR)),"AR"] <- 0

length(unique(dfRelevant$crops)) ##20 crops

# change names
names(dfRelevant)[4:6] <- c("area","productionCal","productionCalDet")
unique(dfRelevant$geo)
#area to ha
dfRelevant$area <- dfRelevant$area*1000


#### calculate yields per farmer and year
dfYieldCalories <- aggregate(cbind(productionCal,area)~geo+time,dfRelevant,sum)
head(dfYieldCalories)
dfYieldCalories$Yield <- dfYieldCalories$productionCal/dfYieldCalories$area
dfYieldCalories <- dfYieldCalories[,c("geo","time","productionCal","area","Yield")]
nrow(unique(dfYieldCalories[,c("geo","time")])) == nrow(dfYieldCalories) # check duplicates

# no production
sum(is.na(dfYieldCalories$Yield))
# set NA to 0
dfYieldCalories[which(is.na(dfYieldCalories$Yield)),"Yield"] <- 0
head(dfYieldCalories)

#### crop diversity per region and year
dfShannon <- aggregate(area~geo+time,dfRelevant,function(i){diversity(i, index = "shannon")})
names(dfShannon)[3] <- "shannon"
dfShannon$effectiveDiversity <- exp(dfShannon$shannon)
head(dfShannon)



###### climate
# intersect file
shpClimateID <- readOGR("spatial","regionsClimateID_europe")
head(shpClimateID@data)
dfClimateID <- shpClimateID@data
dfClimateIDsum <- aggregate(areaHA~cellID,dfClimateID,function(i){sum(i,na.rm=T)})
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
dfClimateFinalArea$weight1 <- (((dfClimateFinalArea$areaHA/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland1980AD)/dfClimateFinalArea$cropland1980ADTot)
dfClimateFinalArea$weight2 <- (((dfClimateFinalArea$areaHA/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland1990AD)/dfClimateFinalArea$cropland1990ADTot)
dfClimateFinalArea$weight3 <- (((dfClimateFinalArea$areaHA/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland1990AD)/dfClimateFinalArea$cropland1990ADTot)
dfClimateFinalArea$weight4 <- (((dfClimateFinalArea$areaHA/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland2000AD)/dfClimateFinalArea$cropland2000ADTot)
dfClimateFinalArea$weight5 <- (((dfClimateFinalArea$areaHA/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland2010AD)/dfClimateFinalArea$cropland2010ADTot)
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
names(dfYieldCalories) <- c("NUTS_ID","Year","Production","AreaHarvested","Yield")
dfRelevant <- dfRelevant[,c("geo","time","crops","productionCalDet")]
names(dfRelevant) <- c("NUTS_ID","Year","Crop","ProductionDet")
dfShannon <- dfShannon[,c("geo","time","effectiveDiversity")]
names(dfShannon) <- c("NUTS_ID","Year","diversity")
names(dfClimateFinalr)

vecRegionFinal <- Reduce(intersect,list(dfYieldCalories$NUTS_ID,dfRelevant$NUTS_ID,dfShannon$NUTS_ID,dfClimateFinalr$NUTS_ID))


# calculate global detrended production & Yield
head(dfYieldCalories)
dfProductionEurope <- aggregate(cbind(Production,AreaHarvested)~Year,dfYieldCalories[which(dfYieldCalories$NUTS_ID%in%vecRegionFinal),],sum)
dfProductionEurope$ProductionDet <- resid(loess(Production ~ Year,data=dfProductionEurope))
dfProductionEurope$Yield <- dfProductionEurope$Production/dfProductionEurope$AreaHarvested
dfProductionEurope$YieldDet <- resid(loess(Yield ~ Year,data=dfProductionEurope))


## summarize per time frame 
lsAll <- lapply(vecRegionFinal,function(g){
  # total production
  show(as.character(g))
  dfProductionRegion <-  dfRelevant[which(dfRelevant$NUTS_ID==g),c("NUTS_ID","Crop","Year","ProductionDet")]
  dfProductionSumRegion <- dfYieldCalories[which(dfYieldCalories$NUTS_ID==g),]
  dfProductionSumRegion$ProductionDet <- resid(loess(Production ~ Year,data=dfProductionSumRegion))
  dfProductionSumRegion$YieldDet <- resid(loess(Yield ~ Year,data=dfProductionSumRegion))
  
  dfShannonRegion <- dfShannon[which(dfShannon$NUTS_ID==g),]
  dfClimateRegion <- dfClimateFinalr[which(dfClimateFinalr$NUTS_ID==g),]
  ctry <- substr(g,1,2)
  
  lsAggregate <- lapply(c(1977,1985,1993,2001,2009),function(yearStart){
    # print(yearStart)
    # cv production
    dfSummary <- data.frame(Area=g, timePeriod= yearStart)
    dfSummary$cvPG <- sd(dfProductionEurope[which(dfProductionEurope$Year>=yearStart&dfProductionEurope$Year<=(yearStart+7)),"ProductionDet"],na.rm=T)/mean(dfProductionEurope[which(dfProductionEurope$Year>=yearStart&dfProductionEurope$Year<=(yearStart+7)),"Production"],na.rm=T)
    dfSummary$cvPL <- sd(dfProductionSumRegion[which(dfProductionSumRegion$Year>=yearStart&dfProductionSumRegion$Year<=(yearStart+7)),"ProductionDet"],na.rm=T)/mean(dfProductionSumRegion[which(dfProductionSumRegion$Year>=yearStart&dfProductionSumRegion$Year<=(yearStart+7)),"Production"],na.rm=T)
    # cv yield
    dfSummary$cvYG <- sd(dfProductionEurope[which(dfProductionEurope$Year>=yearStart&dfProductionEurope$Year<=(yearStart+7)),"YieldDet"],na.rm=T)/mean(dfProductionEurope[which(dfProductionEurope$Year>=yearStart&dfProductionEurope$Year<=(yearStart+7)),"Yield"],na.rm=T)
    dfSummary$cvYL <- sd(dfProductionSumRegion[which(dfProductionSumRegion$Year>=yearStart&dfProductionSumRegion$Year<=(yearStart+7)),"YieldDet"],na.rm=T)/mean(dfProductionSumRegion[which(dfProductionSumRegion$Year>=yearStart&dfProductionSumRegion$Year<=(yearStart+7)),"Yield"],na.rm=T)
    # mean yield
    dfSummary$yieldG <- mean(dfProductionEurope[which(dfProductionEurope$Year>=yearStart&dfProductionEurope$Year<=(yearStart+7)),"Yield"],na.rm=T)
    dfSummary$yieldL <- mean(dfProductionSumRegion[which(dfProductionSumRegion$Year>=yearStart&dfProductionSumRegion$Year<=(yearStart+7)),"Yield"],na.rm=T)
    
    dfSummary$diversity <- mean(dfShannonRegion[which(dfShannonRegion$Year>=yearStart&dfShannonRegion$Year<=(yearStart+7)),"diversity"],na.rm=T)
    
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
dfAll <- dfAll[which(dfAll$yieldL>0),]
length(unique(dfAll$Area)) ## 187 regions
nrow(dfAll) ## 743 data points

# ratio CV
# dfAll$ratioStabilityG <- dfAll$cvG/dfAll$cvL

# ratio yield
# dfAll$ratioYieldG <- dfAll$yieldL/dfAll$yieldG

# hist(dfAll$ratioStabilityG)
# hist(dfAll$ratioYieldG)
# 
# dfAll$benefitStabilityG <- "winner"
# dfAll[which(dfAll$ratioStabilityG<1),"benefitStabilityG"] <- "loser"
# table(dfAll$benefitStabilityG)
# 
# dfAll$benefitYieldG <- "winner"
# dfAll[which(dfAll$ratioYieldG<1),"benefitYieldG"] <- "loser"
# table(dfAll$benefitYieldG)


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
dfAll <- dfAll[,c("NUTS_NAME","NUTS_ID","Area","CNTR_CODE","timePeriod",
                  "cvPG","cvPL","cvYG","cvYL",
                  "yieldG","yieldL",
                  "asynchrony","diversity",
                  "instabilityTemp","instabilityPrec")]
names(dfAll)[1:4] <- c("Region","RegionCode","Country","CountryCode")


write.csv(dfAll, "datasetsDerived/dataFinal_europe.csv",row.names=F)







rm(list=ls())

