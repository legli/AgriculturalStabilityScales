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

# only keep highest 99.9%
dfCropland <- na.omit(dfFull[,c("key","year","agriculturalArea")])
dfCroplandMean <- aggregate(agriculturalArea~key,dfCropland,mean)
head(dfCroplandMean)
dfCroplandMean$propArea <- dfCroplandMean$agriculturalArea/sum(dfCroplandMean$agriculturalArea)
dfCroplandMean <- dfCroplandMean[order(dfCroplandMean$propArea,decreasing = T),]
dfCroplandMean$cumArea <- cumsum(dfCroplandMean$propArea)
ind <- which(dfCroplandMean$cumArea>=0.999)[1]

dfFull <- dfFull[which(dfFull$key%in%dfCroplandMean[1:ind,"key"]),]



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
  ## subset country
  dfKey <- dfRelevant[which(dfRelevant$key==k),]
  dfKeySum <- aggregate(PR~year,dfKey,sum)
  if (sum(dfKeySum$PR)>0){
    minYear <- min(dfKeySum[which(dfKeySum$PR>0),"year"])
    maxYear <- max(dfKeySum[which(dfKeySum$PR>0),"year"])
    dfFinal <- dfKey[which(dfKey$year%in%minYear:maxYear),]
    
    # only keep entries with at least 15 years in total
    if (length(unique(dfFinal$year))>=15)
    {dfFinal}
  }
})
dfProductionFullFinal <- do.call(rbind,lsProductionFull)
min(table(dfProductionFullFinal$key))==15
head(dfProductionFullFinal)
length(unique(dfProductionFullFinal$key))
sum(is.na(dfProductionFullFinal$AR))
sum(is.na(dfProductionFullFinal$PR))

# check number of unique crops
length(unique(dfProductionFullFinal$crop)) ## 34 crops

#### calculate yields per farmer and year
dfYieldCalories <- aggregate(cbind(PR,AR)~key+year,dfProductionFullFinal,sum)
head(dfYieldCalories)
dfYieldCalories$Yield <- dfYieldCalories$PR/dfYieldCalories$AR
dfYieldCalories <- dfYieldCalories[,c("key","year","PR","AR","Yield")]
nrow(unique(dfYieldCalories[,c("key","year")])) == nrow(dfYieldCalories) # check duplicates

#### calculate effective diversity (exp of shannon)
dfShannon <- aggregate(AR~key+year,dfProductionFullFinal,function(x){exp(diversity(x,index="shannon"))})
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
dfClimateFinal <- dfClimateFinalPrint[,c(1,76:121)]

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
dfClimateFinalArea[,5:24] <- dfClimateFinalArea[,5:24]*dfClimateFinalArea$weight1
dfClimateFinalArea[,25:44] <- dfClimateFinalArea[,25:44]*dfClimateFinalArea$weight2


# sum weighted values to get overall weighted average
dfClimateFinalAreaAgg <- aggregate(dfClimateFinalArea[,5:44],by=list(dfClimateFinalArea$REGION_ID),FUN=function(i){sum(i,na.rm=T)})
head(dfClimateFinalAreaAgg)
names(dfClimateFinalAreaAgg)[1] <- "REGION_ID"
min(dfClimateFinalAreaAgg[2:41]) #  0.3643278 -> check for negative values -> would be problematic for instability calculation
# dfClimateFinalAreaAgg$neg <- apply(dfClimateFinalAreaAgg[,2:33],1,function(r){sum(r<0)})
sum(dfClimateFinalAreaAgg[2:41]<0) # 0
# dfClimateFinalAreaAgg <- dfClimateFinalAreaAgg[which(dfClimateFinalAreaAgg$neg==0),1:33]


# change structure
names(dfClimateFinalAreaAgg)
dfClimateFinalr <- dfClimateFinalAreaAgg %>% gather(climYear, Value, names(dfClimateFinalAreaAgg)[2:41])
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
names(dfYieldCalories) <- c("Area","Year","Production","AreaHarvested","Yield")
dfProductionFullFinal <- dfProductionFullFinal[,c("key","year","crop","AR","PR")]
names(dfProductionFullFinal) <- c("Area","Year","Crop","AreaHarvested","Production")
names(dfShannon) <- c("Area","Year","diversity")
names(dfClimateFinals)
sum(is.na(dfClimateFinals))
names(dfClimateFinals) <- c("REGION_ID","Year","meanPrec","meanTemp")
names(dfManagement) <- c("Year","Area","REGION_ID","districtName","agriculturalArea","fertilzer_costs")
head(dfManagement)
sum(is.na(dfManagement))


# aggregate over time periods
vecKeyFinal <- Reduce(intersect,list(dfManagement$Area,dfProductionFullFinal$Area,dfYieldCalories$Area,dfShannon$Area))
length(vecKeyFinal) # 4934



# remove countries
dfYieldCalories <- dfYieldCalories[which(dfYieldCalories$Area%in%vecKeyFinal),]
dfProductionFullFinal <- dfProductionFullFinal[which(dfProductionFullFinal$Area%in%vecKeyFinal),]

save(dfYieldCalories, file="datasetsDerived/dfYieldCalories_farm.RData")
save(dfProductionFullFinal, file="P:/dfProductionFullFinal_farm.RData")


## aggregate for each time fame
lsAllDet <- lapply(vecKeyFinal,function(g){
  # total production
  show(g)
  reg <- unique(dfManagement[which(dfManagement$Area==g),"REGION_ID"])
  if (length(reg)==1)
  {
    lsAggregate <- lapply(c(1998,2008),function(yearStart){
      
      sumRegion <- sum(dfYieldCalories$Area==g&dfYieldCalories$Year>=yearStart&dfYieldCalories$Year<=(yearStart+9))
      if(sumRegion==10){
        # subset data for the target country
        dfProductionSumRegion <- dfYieldCalories[which(dfYieldCalories$Area==g&dfYieldCalories$Year>=yearStart&dfYieldCalories$Year<=(yearStart+9)),]
        dfProductionSumRegion$YieldDet <- resid(lm(Yield ~ Year^2,data=dfProductionSumRegion))
        dfShannonRegion <- dfShannon[which(dfShannon$Area==g&dfShannon$Year>=yearStart&dfShannon$Year<=(yearStart+9)),]
        dfManagementRegion <- dfManagement[which(dfManagement$Area==g&dfManagement$Year>=yearStart&dfManagement$Year<=(yearStart+9)),]
        dfClimateRegion <- dfClimateFinals[which(dfClimateFinals$REGION_ID==reg&dfClimateFinals$Year>=yearStart&dfClimateFinals$Year<=(yearStart+9)),]
        
        dfSummary <- data.frame(Area=g, timePeriod= yearStart, REGION_ID=unique(dfManagementRegion$REGION_ID))
        dfSummary$stability <- mean(dfProductionSumRegion$Yield,na.rm=T)/sd(dfProductionSumRegion$YieldDet,na.rm=T)
        dfSummary$yield <- mean(dfProductionSumRegion$Yield,na.rm=T)
        
        dfSummary$diversity <- mean(dfShannonRegion$diversity,na.rm=T)
        dfSummary$meanCropland <- mean(dfManagementRegion$agriculturalArea,na.rm=T)
        dfSummary$meanNitrogen <- mean(dfManagementRegion$fertilzer_costs,na.rm=T)
        dfSummary$instabilityTemp <- -(mean(dfClimateRegion$meanTemp,na.rm=T)/sd(dfClimateRegion$meanTemp,na.rm=T))
        dfSummary$instabilityPrec <- -(mean(dfClimateRegion$meanPrec,na.rm=T)/sd(dfClimateRegion$meanPrec,na.rm=T))
        na.omit(dfSummary)
      }
    })    
    do.call(rbind,lsAggregate) 
  }    
})
dfAllDet <- do.call(rbind,lsAllDet)
head(dfAllDet)
nrow(unique(dfAllDet[,c("Area","timePeriod")])) == nrow(dfAllDet) # check duplicates
length(unique(dfAllDet$Area))

### add irrigation on the state level
dfIrrigation <- read.csv("datasets/irrigation_europe.csv")
head(dfIrrigation)
dfIrrigationAgg <- aggregate(values~geo,dfIrrigation,function(i){mean(i,na.rm=T)})
head(dfIrrigationAgg)
names(dfIrrigationAgg) <- c("stateEurostat","meanIrrigation")
dfIrrigationAgg <- merge(dfIrrigationAgg,dfDistrict[,c("stateEurostat","REGION_ID")],by="stateEurostat")
dfIrrigationAgg <- unique(dfIrrigationAgg[,c("REGION_ID","meanIrrigation")])

dfAll <- merge(dfAllDet,dfIrrigationAgg[,c("REGION_ID","meanIrrigation")],by="REGION_ID")
sum(is.na(dfAll))

## calculate nitrogen per ha
dfAll$meanFertilizer <- dfAll$meanNitrogen/dfAll$meanCropland

# check NA
sum(is.na(dfAll))
## omit NA
dfAll <- na.omit(dfAll)
length(unique(dfAll$Area)) ## 4290 farmers


## save the dataframe 
names(dfAll)[2] <- "Farmer"
dfAll <- dfAll[,c("Farmer","REGION_ID","timePeriod",
                        "stability","yield",
                        "diversity","meanFertilizer","meanIrrigation",
                        "instabilityTemp","instabilityPrec"
                        )]

write.csv(dfAll,"P:/dataFinal_farmlevel.csv",row.names=F)


rm(list=ls())

