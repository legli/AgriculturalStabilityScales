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
unique(dfAgriculture$crops)
unique(dfAgriculture$strucpro)
range(dfAgriculture$time)


# only keep highest 98%
dfCropland <- dfAgriculture[which(dfAgriculture$crops=="UAA"),]
sum(is.na(dfCropland))
dfCroplandMean <- aggregate(values~geo,dfCropland,mean)
head(dfCroplandMean)
dfCroplandMean$propArea <- dfCroplandMean$values/sum(dfCroplandMean$values)
dfCroplandMean <- dfCroplandMean[order(dfCroplandMean$propArea,decreasing = T),]
dfCroplandMean$cumArea <- cumsum(dfCroplandMean$propArea)
ind <- which(dfCroplandMean$cumArea>=0.999)[1]


# change structure
dfAgriculture <- dfAgriculture[which(dfAgriculture$strucpro%in%c("AR","PR")),]
dfAgriculture <- dfAgriculture[which(dfAgriculture$geo%in%dfCroplandMean[1:ind,"geo"]),]
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
  dfArea <- dfAgricultureReduced[which(dfAgricultureReduced$geo==g),]
  dfAreaSum <- aggregate(PR~time,dfArea,sum)
  minYear <- min(dfAreaSum[which(dfAreaSum$PR>0),"time"])
  maxYear <- max(dfAreaSum[which(dfAreaSum$PR>0),"time"])
  dfFinal <- dfArea[which(dfArea$time%in%minYear:maxYear),]
  
  # only keep entries with at least 15 years in total
  if (length(unique(dfFinal$time))>=15)
  {dfFinal}
})
dfProductionFullFinal <- do.call(rbind,lsRelevant)
nrow(dfProductionFullFinal)/nrow(dfAgricultureReduced) # 97% of original data
head(dfProductionFullFinal)
sort(as.character(unique(dfProductionFullFinal$geo)))

## remove NA
sum(is.na(dfProductionFullFinal$AR))
sum(is.na(dfProductionFullFinal$PR))
dfProductionFullFinal <- na.omit(dfProductionFullFinal)

length(unique(dfProductionFullFinal$crops)) ##28 crops

# change names
names(dfProductionFullFinal)[4:5] <- c("area","productionCal")
unique(dfProductionFullFinal$geo)
#area to ha
dfProductionFullFinal$area <- dfProductionFullFinal$area*1000


#### calculate yields per farmer and year
dfYieldCalories <- aggregate(cbind(productionCal,area)~geo+time,dfProductionFullFinal,sum)
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
dfShannon <- aggregate(area~geo+time,dfProductionFullFinal,function(i){diversity(i, index = "shannon")})
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
dfClimateFinal <- dfClimateFinalPrint[,c(1,36:121)]

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
dfClimateFinalArea$weight3 <- (((dfClimateFinalArea$areaHA/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland1990AD)/dfClimateFinalArea$cropland2000ADTot)
dfClimateFinalArea$weight4 <- (((dfClimateFinalArea$areaHA/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland2000AD)/dfClimateFinalArea$cropland2010ADTot)
hist(dfClimateFinalArea$weight4)

## multiply weight by temeperature values
names(dfClimateFinalArea)
dfClimateFinalArea[,7:26] <- dfClimateFinalArea[,7:26]*dfClimateFinalArea$weight1
dfClimateFinalArea[,27:46] <- dfClimateFinalArea[,27:46]*dfClimateFinalArea$weight2
dfClimateFinalArea[,47:66] <- dfClimateFinalArea[,47:66]*dfClimateFinalArea$weight3
dfClimateFinalArea[,67:86] <- dfClimateFinalArea[,67:86]*dfClimateFinalArea$weight4

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
names(dfYieldCalories) <- c("Area","Year","Production","AreaHarvested","Yield")
dfProductionFullFinal <- dfProductionFullFinal[,c("geo","time","crops","area","productionCal")]
names(dfProductionFullFinal) <- c("Area","Year","Crop","AreaHarvested","Production")
dfShannon <- dfShannon[,c("geo","time","effectiveDiversity")]
names(dfShannon) <- c("Area","Year","diversity")
names(dfClimateFinalr) <- c("Area","Year","meanPrec","meanTemp")

vecRegionFinal <- Reduce(intersect,list(dfYieldCalories$Area,dfProductionFullFinal$Area,dfShannon$Area,dfClimateFinalr$Area))

# remove area
dfYieldCalories <- dfYieldCalories[which(dfYieldCalories$Area%in%vecRegionFinal),]
dfProductionFullFinal <- dfProductionFullFinal[which(dfProductionFullFinal$Area%in%vecRegionFinal),]

save(dfYieldCalories, file="datasetsDerived/dfYieldCalories_europe.RData")
save(dfProductionFullFinal, file="datasetsDerived/dfProductionFullFinal_europe.RData")

## summarize per time frame 
lsAll <- lapply(vecRegionFinal,function(g){
  # total production
  show(as.character(g))
  lsAggregate <- lapply(c(1978,1988,1998,2008),function(yearStart){

    sumRegion <- sum(dfYieldCalories$Area==g&dfYieldCalories$Year>=yearStart&dfYieldCalories$Year<=(yearStart+9))
    if(sumRegion==10){   
      # subset data for the target country
      dfProductionSumRegion <- dfYieldCalories[which(dfYieldCalories$Area==g&dfYieldCalories$Year>=yearStart&dfYieldCalories$Year<=(yearStart+9)),]
      dfProductionSumRegion$YieldDet <- resid(lm(Yield ~ Year^2,data=dfProductionSumRegion))
      dfShannonRegion <- dfShannon[which(dfShannon$Area==g&dfShannon$Year>=yearStart&dfShannon$Year<=(yearStart+9)),]
      dfClimateRegion <- dfClimateFinalr[which(dfClimateFinalr$Area==g&dfClimateFinalr$Year>=yearStart&dfClimateFinalr$Year<=(yearStart+9)),]
      
      dfSummary <- data.frame(Area=g, timePeriod= yearStart)
      dfSummary$stability <- mean(dfProductionSumRegion$Yield,na.rm=T)/sd(dfProductionSumRegion$YieldDet,na.rm=T)
      dfSummary$yield <- mean(dfProductionSumRegion$Yield,na.rm=T)
      dfSummary$areaHarvested <- mean(dfProductionSumRegion$AreaHarvested,na.rm=T)
      dfSummary$diversity <- mean(dfShannonRegion$diversity,na.rm=T)
      dfSummary$instabilityTemp <- -(mean(dfClimateRegion$meanTemp,na.rm=T)/sd(dfClimateRegion$meanTemp,na.rm=T))
      dfSummary$instabilityPrec <- -(mean(dfClimateRegion$meanPrec,na.rm=T)/sd(dfClimateRegion$meanPrec,na.rm=T))
      na.omit(dfSummary)
    }
  })
  do.call(rbind,lsAggregate)
})
dfAll <- do.call(rbind,lsAll)
head(dfAll)
nrow(unique(dfAll[,c("Area","timePeriod")])) == nrow(dfAll) # check duplicates
unique(dfAll$timePeriod)
head(dfAll)

# check NA
sum(is.na(dfAll))
## omit NA
dfAll <- na.omit(dfAll)
# dfAll <- dfAll[which(dfAll$yield>0),]
length(unique(dfAll$Area)) ## 180 regions
nrow(dfAll) ## 433 data points


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

## save dataframe
names(dfAll)
dfAll <- dfAll[,c("NUTS_NAME","NUTS_ID","Area","CNTR_CODE","timePeriod",
                  "stability","yield","areaHarvested",
                  "diversity",
                  "instabilityTemp","instabilityPrec")]
names(dfAll)[1:4] <- c("Region","RegionCode","Country","CountryCode")


write.csv(dfAll, "datasetsDerived/dataFinal_europe.csv",row.names=F)







rm(list=ls())

