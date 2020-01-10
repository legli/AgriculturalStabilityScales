## import libraries
library(haven)
library(vegan)
library(tidyr)
library(dplyr)
library(rgeos)
library(rgdal)

#### read Testbetriebsdaten
dfFull <- as.data.frame(read_sas("P:/egli20191219.sas7bdat"))
head(dfFull)
names(dfFull)
str(dfFull)


# remove columns not needed (fertilizer animals, separate labor)
dfFull <- dfFull[,-which(names(dfFull)%in%c("z0019s02","z0021s02","z0023s02","z1031s02","z2559s02","z5239s06","z5239s07","z5240s06","z5240s07","z7099s03","z7089s03","z7098s03","z8014s02","z8015s02","z8150s02","z8153s02","z8156s02"))]

# adapt names
names(dfFull)[which(names(dfFull)=="jahr")] <- "year"
names(dfFull)[which(names(dfFull)=="z0003s02")] <- "state"
names(dfFull)[which(names(dfFull)=="z0004s02")] <- "district"
names(dfFull)[which(names(dfFull)=="z0024s02")] <- "date"
names(dfFull)[which(names(dfFull)=="z2539s02")] <- "fertilizer_general_costs"
names(dfFull)[which(names(dfFull)=="z2540s02")] <- "fertilizer_organic_costs"
names(dfFull)[which(names(dfFull)=="z6119s07")] <- "agriculturalArea"
names(dfFull)[which(names(dfFull)=="z7099s03")] <- "labor_costs"
names(dfFull)

## remove farms with zero area
dfFull <- dfFull[which(dfFull$agriculturalArea>0),]

## add spatial id
dfDistrict <- read.csv("datasets/districts_farmlevel.csv",sep=";")
head(dfDistrict)

dfFull <- merge(dfFull,dfDistrict[,c("state","district","districtName","REGION_ID")],by=c("state","district"))
head(dfFull)
names(dfFull)
unique(dfFull$REGION_ID)

# count occurences of variables per year
dfCount <- aggregate(dfFull,by=list(dfFull$year),function(i){sum(!is.na(i))})
colSums(dfCount)

# only consider year from 1.7-30.6 (same time period for all farmers)
sum(is.na(dfFull$date))
unique(substr(dfFull$date,1,4))
dfFullRed <- dfFull[which(substr(dfFull$date,1,4)=="3006"|substr(dfFull$date,1,4)=="3106"|
                       substr(dfFull$date,1,3)=="306"|substr(dfFull$date,1,3)=="697"),]
sort(unique(dfFullRed$date))

## remove state, district and date (not needed anymore)
dfFullRed <- dfFullRed[,-which(names(dfFullRed)%in%c("state","district","date"))]
names(dfFullRed)

## fertilizer costs: change NA to 0 (assumption that they are no inputs if not reported)
sum(is.na(dfFullRed$fertilizer_general_costs)) # 18713
dfFullRed[which(is.na(dfFullRed$fertilizer_general_costs)),"fertilizer_general_costs"] <- 0
sum(is.na(dfFullRed$fertilizer_organic_costs)) # 214524
dfFullRed[which(is.na(dfFullRed$fertilizer_organic_costs)),"fertilizer_organic_costs"] <- 0
sum(is.na(dfFullRed$agriculturalArea))# 0

## recalcualte some variables
hist(dfFullRed$fertilizer_general_costs)
hist(dfFullRed$fertilizer_organic_costs)
dfFullRed$fertilizer_costs <- rowSums(dfFullRed[,c("fertilizer_general_costs","fertilizer_organic_costs")],na.rm=T) # total costs for fertilizer

dfFullRed <- dfFullRed[,-which(names(dfFullRed)%in%c("fertilizer_general_costs","fertilizer_organic_costs"))]
hist(dfFullRed$fertilizer_costs)
dfFullRed$fertilizer_costs <- dfFullRed$fertilizer_costs*(-1) # invert income-investment: investment should be postive
min(dfFullRed$fertilizer_costs)

## keep management variables separate
dfManagement <- dfFullRed[,c("year","key","REGION_ID","districtName","agriculturalArea","fertilizer_costs")]

## resturcture
dfFullRedR <-  dfFullRed[,c(1:92)] %>% gather(cropVar, values,names(dfFullRed)[3:92])
head(dfFullRedR)
dfFullRedR$strucpro <- "AR"
dfFullRedR[which(substr(dfFullRedR$cropVar,7,8)=="03"),"strucpro"] <- "PR"
dfFullRedR$crop <- substr(dfFullRedR$cropVar,1,5)
dfFullRedS <- dfFullRedR[,-which(names(dfFullRedR)=="cropVar")] %>% spread(strucpro,values)
head(dfFullRedS)
nrow(dfFullRedS)/nrow(dfFullRedR)==0.5

## read calorie data
dfTargetCrops <- read.csv("datasets/targetCrops_farmlevel.csv")
# only include crops available across scales
dfTargetCrops <- unique(dfTargetCrops[,c("codeTestbetrieb","calories")])

## only keep crops where calorie data is available
dfActual <- merge(dfFullRedS,dfTargetCrops,by.x="crop",by.y="codeTestbetrieb")
head(dfActual)

## change yield to production and then to calories (change production from dt to t)
dfActual$PR <- dfActual$PR*dfActual$AR*0.1*dfActual$calories
# revome "calories" column 
dfActual <- dfActual[,-which(names(dfActual)=="calories")]

## sum crops by testbetrieb and year
names(dfActual)
dfActualFinal <- as.data.frame(dfActual %>%
  group_by(crop,year,key) %>%
  summarize_all(sum, na.rm = TRUE))

head(dfActualFinal)
table(dfActualFinal$crop)


## make area and production consistent
# set 0 areas to NA, set production in NA areas to NA
dfActualFinal[which(dfActualFinal$AR<=0),"AR"] <- NA
dfActualFinal[which(is.na(dfActualFinal$AR)),"PR"] <- NA
# set production to 0 where area is reported
dfActualFinal[which(dfActualFinal$AR> 0 & is.na(dfActualFinal$PR)),"PR"] <- 0

## remove zero areas
dfRelevant <- dfActualFinal[!is.na(dfActualFinal$AR),]
dfRelevant <- dfActualFinal[!is.na(dfActualFinal$PR),]
table(dfRelevant$crop)
length(unique(dfRelevant$crop)) ##34 crops
length(unique(dfRelevant$key)) ## 23078 farmers

## reduce dataset: only include crops with at least 15 entries per region, remove 0 at the beginning and end of time series!; maximize  number of crops and years per time window
# iterate through countries
vecKey <- unique(dfRelevant$key)
lsProductionFull <- lapply(vecKey,function(k){
  show(k)
  ## subset key
  dfKey <- dfRelevant[which(dfRelevant$key==k),]
  
  ## iterate through crops
  lsCrops <- lapply(unique(dfKey$crop),function(c){
    dfCrop <- merge(data.frame(year=2001:2016,key=k,crop=c),dfKey[which(dfKey$crop==c),c("crop","year","AR","PR")],by=c("crop","year"),all.x=T) # create full data frame
    # only consider crops with at least 10 data points for detrending
    if (sum(!is.na(dfCrop$PR))>=10&sum(dfCrop$PR>0,na.rm=T)>0){
      minYear <- min(dfCrop[which(dfCrop$PR>0),"year"])
      maxYear <- max(dfCrop[which(dfCrop$PR>0),"year"])
      dfCrop[which(dfCrop$year<minYear),c("AR","PR")] <- NA
      dfCrop[which(dfCrop$year>maxYear),c("AR","PR")] <- NA
      dfCrop[which(dfCrop$year>minYear&is.na(dfCrop$PR)&dfCrop$time<maxYear),"PR"] <- 0
      # detrend crop-specific production
      if (sum(!is.na(dfCrop$PR))>=10){
        dfCrop$PRDet <- NA
        dfCrop[which(!is.na(dfCrop$PR)),"PRDet"] <-resid(loess(PR ~ year,data= dfCrop))
        dfCrop
      }
    }
  })
  dfCrops <- do.call(rbind,lsCrops)
  if (!is.null(dfCrops))
  {
    lsWindow <- lapply(c(2001,2009),function(yearStart){
      ## keep maximum number of crops if at least 5 years are covered
      # moving window 
      lsWidth <- lapply(5:8,function(w){ # w = width of time window (>=5)
        dfCropsR <- dfCrops[which(dfCrops$year>=yearStart&dfCrops$year<=(yearStart+7)),c("year","crop","PR")] %>% spread(year,PR)
        by <- 8-(w-1)
        lsPos <- lapply(1:by,function(p){ # iterate through all possible windows by given width
          noCrop <- sum(apply(dfCropsR[,(1+p):(1+p+w-1)],1,function(r){sum(!is.na(r))==w}))
          data.frame(width = w, year = (yearStart+p-1), num = noCrop)
        })
        do.call(rbind,lsPos)
      })
      dfFreq <- do.call(rbind,lsWidth)
      # get time window with maximum number of crops, then largest time period and minimum starting year
      maxN <- max(dfFreq$num)
      maxW <- max(dfFreq[which(dfFreq$num==maxN),"width"])
      minYear <- min(dfFreq[which(dfFreq$num==maxN&dfFreq$width==maxW),"year"])
      maxYear <- minYear+maxW-1
      
      # remove crops with NA in focal time frame 
      dfFocal <- dfCrops[which(dfCrops$year>=minYear&dfCrops$year<=maxYear),]
      unfocalCrops <- unique(dfFocal[which(is.na(dfFocal$PR)),"crop"])
      dfCropsTotal <- dfFocal[which(!dfFocal$crop%in%unfocalCrops&!is.na(dfFocal$PR)),]
      dfCropsTotal
    })
    dfFinal <- do.call(rbind,lsWindow)
    # only keep entries with at least 10 years in total
    if (length(unique(dfFinal$year))>=10)
    {dfFinal}
  }
})
dfProductionFull_Final <- do.call(rbind,lsProductionFull)
min(table(dfProductionFull_Final$key))==10
head(dfProductionFull_Final)
length(unique(dfProductionFull_Final$key))
sum(is.na(dfProductionFull_Final$AR))
sum(is.na(dfProductionFull_Final$PR))

# check number of unique crops
length(unique(dfProductionFull_Final$crop)) ## 29 crops

#### calculate yields per farmer and year
dfYieldCalories <- aggregate(cbind(PR,AR)~key+year,dfProductionFull_Final,sum)
head(dfYieldCalories)
dfYieldCalories$Yield <- dfYieldCalories$PR/dfYieldCalories$AR
dfYieldCalories <- dfYieldCalories[,c("key","year","PR","AR","Yield")]
nrow(unique(dfYieldCalories[,c("key","year")])) == nrow(dfYieldCalories) # check duplicates

#### calculate effective diversity (exp of shannon)
dfShannon <- aggregate(AR~key+year,dfProductionFull_Final,function(x){exp(diversity(x,index="shannon"))})
head(dfShannon)
names(dfShannon)[3] <- "diversity"
nrow(dfShannon)==nrow(dfYieldCalories)
nrow(unique(dfShannon[,c("key","year")])) == nrow(dfShannon) # check duplicates


###### climate
# intersect file
shpClimateID <- readOGR("spatial","districtClimateID_farmlevel")

head(shpClimateID@data)
dfClimateID <- shpClimateID@data
names(dfClimateID)
dfClimateID <- dfClimateID[,c("district","cellID","areaHA")]
unique(dfClimateID$district)
names(dfClimateID)[1] <- "REGION_ID"
dfClimateIDsum <- aggregate(areaHA~cellID,dfClimateID,function(i){sum(i,na.rm=T)})
names(dfClimateIDsum)[2] <- "areaTot"
dfClimateID <- merge(dfClimateID,dfClimateIDsum,by="cellID")
head(dfClimateID)

# climate file
load("datasetsDerived/climate_global.RData")

names(dfClimateFinalPrint)
head(dfClimateFinalPrint)
dfClimateFinal <- dfClimateFinalPrint[,c(1,80:121)]

# merge
dfClimateFinalArea <- merge(dfClimateID,dfClimateFinal,by="cellID")
head(dfClimateFinalArea)
length(unique(dfClimateFinalArea$cellID)) / nrow(dfClimateFinalArea)
# get cropland by region
sum(is.na(dfClimateFinalArea))
dfCroplandTot <- aggregate(cbind(cropland2000AD,cropland2010AD)~REGION_ID,dfClimateFinalArea,sum)
head(dfCroplandTot)
names(dfCroplandTot)[2:3] <- paste0(names(dfCroplandTot)[2:3],"Tot")
dfClimateFinalArea <- merge(dfClimateFinalArea,dfCroplandTot,by="REGION_ID")
head(dfClimateFinalArea)
names(dfClimateFinalArea)
## weighted average: get area of cropland attributed to a cell segment (i.e. cropland area multiplied by area share of the segment), divide it by total cropland area across region
dfClimateFinalArea$weight1 <- (((dfClimateFinalArea$areaHA/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland2000AD)/dfClimateFinalArea$cropland2000ADTot)
dfClimateFinalArea$weight2 <- (((dfClimateFinalArea$areaHA/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland2010AD)/dfClimateFinalArea$cropland2010ADTot)
hist(dfClimateFinalArea$weight2)

## multiply weight by temeperature values
names(dfClimateFinalArea)
dfClimateFinalArea[,5:20] <- dfClimateFinalArea[,5:20]*dfClimateFinalArea$weight1
dfClimateFinalArea[,21:36] <- dfClimateFinalArea[,21:36]*dfClimateFinalArea$weight2


# sum weighted values to get overall weighted average
dfClimateFinalAreaAgg <- aggregate(dfClimateFinalArea[,5:36],by=list(dfClimateFinalArea$REGION_ID),FUN=function(i){sum(i,na.rm=T)})
head(dfClimateFinalAreaAgg)
names(dfClimateFinalAreaAgg)[1] <- "REGION_ID"
min(dfClimateFinalAreaAgg[2:33]) #  0.3643278 -> check for negative values -> would be problematic for instability calculation
# dfClimateFinalAreaAgg$neg <- apply(dfClimateFinalAreaAgg[,2:33],1,function(r){sum(r<0)})
sum(dfClimateFinalAreaAgg[2:33]<0) # 0
# dfClimateFinalAreaAgg <- dfClimateFinalAreaAgg[which(dfClimateFinalAreaAgg$neg==0),1:33]


# change structure
names(dfClimateFinalAreaAgg)
dfClimateFinalr <- dfClimateFinalAreaAgg %>% gather(climYear, Value, names(dfClimateFinalAreaAgg)[2:33])
head(dfClimateFinalr)
dfClimateFinalr$year <- as.numeric(substr(dfClimateFinalr$climYear,9,12))
dfClimateFinalr$Element <- substr(dfClimateFinalr$climYear,1,8)
dfClimateFinalr <- dfClimateFinalr[,c("REGION_ID","year","Element","Value")]
head(dfClimateFinalr)
dfClimateFinals <- dfClimateFinalr %>% spread(Element, Value)
head(dfClimateFinals)
nrow(dfClimateFinals)
nrow(unique(dfClimateFinals[,c("REGION_ID","year")])) == nrow(dfClimateFinals) # check duplicates


#### combine all datasets
names(dfManagement)
head(dfManagement)
sum(is.na(dfManagement))
names(dfClimateFinals)
sum(is.na(dfClimateFinals))

# aggregate over time periods
vecKeyFinal <- Reduce(intersect,list(dfManagement$key,dfProductionFull_Final$key,dfYieldCalories$key,dfShannon$key))
length(vecKeyFinal) # 4124

# calculate national detrended production & Yield
head(dfYieldCalories)
sum(is.na(dfYieldCalories))
dfProductionGlobal <- aggregate(cbind(PR,AR)~year,dfYieldCalories[which(dfYieldCalories$key%in%vecKeyFinal),],sum)
dfProductionGlobal$ProductionDet <- resid(loess(PR ~ year,data=dfProductionGlobal))
dfProductionGlobal$Yield <- dfProductionGlobal$PR/dfProductionGlobal$AR

## aggregate for each time fame
lsAllDet <- lapply(vecKeyFinal,function(g){
  # total production
  show(g)
  dfProductionSumKey <- dfYieldCalories[which(dfYieldCalories$key==g),]
  dfProductionKey <- dfProductionFull_Final[which(dfProductionFull_Final$key==g),c("key","crop","year","PRDet")]
  dfProductionSumKey$ProductionDet <- resid(loess(PR ~ year,data=dfProductionSumKey))      
  dfManagementKey <- dfManagement[which(dfManagement$key==g),]
  dfShannonKey <- dfShannon[which(dfShannon$key==g),]
  reg <- unique(dfManagementKey$REGION_ID)
  regName <- unique(dfManagementKey$districtName)
    
  if (length(reg)==1)
  {
    # climate only if it is unique region
    dfClimateRegion <- dfClimateFinals[which(dfClimateFinals$REGION_ID==reg),]
    
    lsAggregate <- lapply(c(2001,2009),function(yearStart){
    # summarize
    dfSummary <- data.frame(id=g,districtCode=reg, districtName=regName,timePeriod= yearStart)
    dfSummary$cvG <- sd(dfProductionGlobal[which(dfProductionGlobal$year>=yearStart&dfProductionGlobal$year<=(yearStart+7)),"ProductionDet"],na.rm=T)/mean(dfProductionGlobal[which(dfProductionGlobal$year>=yearStart&dfProductionGlobal$year<=(yearStart+7)),"PR"],na.rm=T)
    dfSummary$cvL <- sd(dfProductionSumKey[which(dfProductionSumKey$year>=yearStart&dfProductionSumKey$year<=(yearStart+7)),"ProductionDet"],na.rm=T)/mean(dfProductionSumKey[which(dfProductionSumKey$year>=yearStart&dfProductionSumKey$year<=(yearStart+7)),"PR"],na.rm=T)

    dfSummary$yieldG <- mean(dfProductionGlobal[which(dfProductionGlobal$year>=yearStart&dfProductionGlobal$year<=(yearStart+7)),"Yield"],na.rm=T)
    dfSummary$yieldL <- mean(dfProductionSumKey[which(dfProductionSumKey$year>=yearStart&dfProductionSumKey$year<=(yearStart+7)),"Yield"],na.rm=T)
    
    dfSummary$diversity <- mean(dfShannonKey[which(dfShannonKey$year>=yearStart&dfShannonKey$year<=(yearStart+7)),"diversity"],na.rm=T)
    
    # asynchrony (Code snippet from Mehrabi & Ramankutty 2019, supplement p. 29)
    dfProductionKeyTime <- dfProductionKey[which(dfProductionKey$year>=yearStart&dfProductionKey$year<=(yearStart+7)),]
    noCrop <- length(unique(dfProductionKeyTime$crop))
    dfSummary$asynchrony <- NA
    if (noCrop == 1) {  dfSummary$asynchrony <- 0}
    if (noCrop > 1) {
      dfProductionKeyTimer <- dfProductionKeyTime %>% spread(crop, PRDet)
      det.w.dec <- dfProductionKeyTimer[,3:ncol(dfProductionKeyTimer)]
      diff.cov <- cov(data.frame(det.w.dec))
      diag.cov <- diag(diff.cov)
      sqrt.diag.cov <- sqrt(diag.cov)
      diff.var.glob <-  sum(diff.cov,na.rm=T)
      diff.sd.local <- sum(sqrt.diag.cov,na.rm=T)
      diff.async <- 1-diff.var.glob/(diff.sd.local^2) #asynchrony
      dfSummary$asynchrony <- diff.async
    }
    
    dfSummary$meanArea <- mean(dfManagementKey[which(dfManagementKey$year>=yearStart&dfManagementKey$year<=(yearStart+7)),"agriculturalArea"],na.rm=T)
    dfSummary$meanFertilizer <- mean(dfManagementKey[which(dfManagementKey$year>=yearStart&dfManagementKey$year<=(yearStart+7)),"fertilizer_costs"],na.rm=T)
    # climate: start one year earlier -> as current harvest are influenced by the year before
    dfSummary$instabilityTemp <- -(mean(dfClimateRegion[which(dfClimateFinals$year>=(yearStart-1)&dfClimateFinals$year<=(yearStart+7-1)),"meanTemp"],na.rm=T)/sd(dfClimateRegion[which(dfClimateFinals$year>=(yearStart-1)&dfClimateFinals$year<=(yearStart+7)),"meanTemp"],na.rm=T))
    dfSummary$instabilityPrec <- -(mean(dfClimateRegion[which(dfClimateFinals$year>=(yearStart-1)&dfClimateFinals$year<=(yearStart+7-1)),"meanPrec"],na.rm=T)/sd(dfClimateRegion[which(dfClimateFinals$year>=(yearStart-1)&dfClimateFinals$year<=(yearStart+7)),"meanPrec"],na.rm=T))
    na.omit(dfSummary)
    })
    do.call(rbind,lsAggregate)      
  }
})
dfAllDet <- do.call(rbind,lsAllDet)
head(dfAllDet)
nrow(unique(dfAllDet[,c("id","timePeriod")])) == nrow(dfAllDet) # check duplicates
length(unique(dfAllDet$id))

### add irrigation on the state level
dfIrrigation <- read.csv("datasets/irrigation_europe.csv")
head(dfIrrigation)
dfIrrigationAgg <- aggregate(values~geo,dfIrrigation,function(i){mean(i,na.rm=T)})
head(dfIrrigationAgg)
names(dfIrrigationAgg) <- c("stateEurostat","meanIrrigation")
dfIrrigationAgg <- merge(dfIrrigationAgg,dfDistrict[,c("stateEurostat","REGION_ID")],by="stateEurostat")
dfIrrigationAgg <- unique(dfIrrigationAgg[,c("REGION_ID","meanIrrigation")])

dfAll <- merge(dfAllDet,dfIrrigationAgg[,c("REGION_ID","meanIrrigation")],by.x="districtCode",by.y="REGION_ID")
sum(is.na(dfAll))


# ratio CV
dfAll$ratioStabilityG <- dfAll$cvG/dfAll$cvL

# ratio yield
dfAll$ratioYieldG <- dfAll$yieldL/dfAll$yieldG

## calculate nitrogen per ha
dfAll$meanFertilizer_ha <- dfAll$meanFertilizer/dfAll$meanArea

# check NA
sum(is.na(dfAll))
## omit NA
dfAll <- na.omit(dfAll)
hist(dfAll$ratioStabilityG)
hist(dfAll$ratioYieldG)
length(unique(dfAll$id)) ## 4091 farmers

dfAll$benefitStabilityG <- "winner"
dfAll[which(dfAll$ratioStabilityG<1),"benefitStabilityG"] <- "loser"
table(dfAll$benefitStabilityG)

dfAll$benefitYieldG <- "winner"
dfAll[which(dfAll$ratioYieldG<1),"benefitYieldG"] <- "loser"
table(dfAll$benefitYieldG)

## save the dataframe 
names(dfAll)
dfAll <- dfAll[,c("id","districtCode","districtName","timePeriod",
                        "cvG","cvL","ratioStabilityG","benefitStabilityG",
                        "yieldG","yieldL","ratioYieldG","benefitYieldG",
                        "asynchrony","diversity","meanFertilizer_ha","meanIrrigation",
                        "instabilityTemp","instabilityPrec"
                        )]

write.csv(dfAll,"C:/Users/egli/Nextcloud/Cloud/PhD_Leipzig/Publications/Globalization/datasetsDerived/dataFinal_farmlevel.csv",row.names=F)


rm(list=ls())

