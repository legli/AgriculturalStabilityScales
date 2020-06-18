library(haven)
library(vegan)
library(tidyr)
library(rgdal)

######## DATA PREPARATION

#### read Testbetriebsdaten
dfFull <- as.data.frame(read_sas("P:/egli20191219.sas7bdat"))
head(dfFull)
names(dfFull)
str(dfFull)

# remove columns not needed (fertilizer animals, separate labor)
dfFull <- dfFull[,-which(names(dfFull)%in%c("z0019s02","z0021s02","z0023s02","z1031s02","z2559s02","z5239s06","z5239s07","z5240s06","z5240s07","z7099s03","z7089s03","z7098s03","z8014s02","z8015s02","z8150s02","z8153s02","z8156s02"))]

# adapt names
names(dfFull)[which(names(dfFull)=="key")] <- "Level"
names(dfFull)[which(names(dfFull)=="jahr")] <- "Year"
names(dfFull)[which(names(dfFull)=="z0003s02")] <- "state"
names(dfFull)[which(names(dfFull)=="z0004s02")] <- "district"
names(dfFull)[which(names(dfFull)=="z0024s02")] <- "date"
names(dfFull)[which(names(dfFull)=="z2539s02")] <- "fertilizer_general_costs"
names(dfFull)[which(names(dfFull)=="z2540s02")] <- "fertilizer_organic_costs"
names(dfFull)[which(names(dfFull)=="z6119s07")] <- "croplandArea"
names(dfFull)[which(names(dfFull)=="z7099s03")] <- "labor_costs"
names(dfFull)

## remove regions with zero area
dfFull <- dfFull[which(dfFull$croplandArea>0),]

# only keep the regions covering 99.9% of the total cropland area 
dfCropland <- na.omit(dfFull[,c("Level","Year","croplandArea")])
dfCroplandMean <- aggregate(croplandArea~Level,dfCropland,mean)
head(dfCroplandMean)
dfCroplandMean$propArea <- dfCroplandMean$croplandArea/sum(dfCroplandMean$croplandArea)
dfCroplandMean <- dfCroplandMean[order(dfCroplandMean$propArea,decreasing = T),]
dfCroplandMean$cumArea <- cumsum(dfCroplandMean$propArea)
ind <- which(dfCroplandMean$cumArea>=0.999)[1]

dfFull <- dfFull[which(dfFull$Level%in%dfCroplandMean[1:ind,"Level"]),]

# add spatial id
dfDistrict <- read.csv("datasets/districts_farmlevel.csv",sep=";")
head(dfDistrict)

dfFull <- merge(dfFull,dfDistrict[,c("state","district","districtName","REGION_ID")],by=c("state","district"))
head(dfFull)
names(dfFull)
unique(dfFull$REGION_ID)

# only consider Year from 1.7-30.6 (same time period for all farmers)
sum(is.na(dfFull$date))
unique(substr(dfFull$date,1,4))
dfProduction <- dfFull[which(substr(dfFull$date,1,4)=="3006"|substr(dfFull$date,1,4)=="3106"|
                       substr(dfFull$date,1,3)=="306"|substr(dfFull$date,1,3)=="697"),]
sort(unique(dfProduction$date))

## remove state, district and date (not needed anymore)
dfProduction <- dfProduction[,-which(names(dfProduction)%in%c("state","district","date"))]
names(dfProduction)

## fertilizer costs: change NA to 0 (assumption that they are no inputs if not reported)
sum(is.na(dfProduction$fertilizer_general_costs)) # 17607
dfProduction[which(is.na(dfProduction$fertilizer_general_costs)),"fertilizer_general_costs"] <- 0
sum(is.na(dfProduction$fertilizer_organic_costs)) # 207956
dfProduction[which(is.na(dfProduction$fertilizer_organic_costs)),"fertilizer_organic_costs"] <- 0
sum(is.na(dfProduction$croplandArea))# 0

## recalcualte some variables
hist(dfProduction$fertilizer_general_costs)
hist(dfProduction$fertilizer_organic_costs)
dfProduction$fertilizer <- rowSums(dfProduction[,c("fertilizer_general_costs","fertilizer_organic_costs")],na.rm=T) # total costs for fertilizer

dfProduction <- dfProduction[,-which(names(dfProduction)%in%c("fertilizer_general_costs","fertilizer_organic_costs"))]
hist(dfProduction$fertilizer)
dfProduction$fertilizer <- dfProduction$fertilizer*(-1) # invert income-investment: investment should be postive
min(dfProduction$fertilizer)

# keep management variables separate
dfManagement <- dfProduction[,c("Year","Level","REGION_ID","districtName","croplandArea","fertilizer")]

# change dataset structure (frow wide to long)
dfProduction <-  dfProduction[,c(1:92)] %>% gather(cropVar, values,names(dfProduction)[3:92])
head(dfProduction)
dfProduction$strucpro <- "AreaHarvested"
dfProduction[which(substr(dfProduction$cropVar,7,8)=="03"),"strucpro"] <- "Production"
dfProduction$Item <- substr(dfProduction$cropVar,1,5)
# split area harvested and production in two columns (from long to wide)
dfProduction <- dfProduction[,-which(names(dfProduction)=="cropVar")] %>% spread(strucpro,values)
head(dfProduction)

# set NA for harvested data for which there is no production data and vice versa 
dfProduction[is.na(dfProduction$Production),"AreaHarvested"] <- NA
dfProduction[is.na(dfProduction$AreaHarvested),"Production"] <- NA
dfProduction <- dfProduction[which(!is.na(dfProduction$AreaHarvested) & !is.na(dfProduction$Production)),]
# remove zero areas and production
dfProduction <- dfProduction[-which(dfProduction$AreaHarvested==0 |  dfProduction$Production==0),]

# add calories
dfCalories <- read.csv("datasets/targetCrops_farmlevel.csv")
names(dfCalories)[6] <- "Item"
      
# subset crops without calories
dfProduction <- merge(dfProduction,dfCalories[,c("Item","Calories")],by="Item")
names(dfProduction)

# change production to calories (dt to t)
dfProduction$Production <- dfProduction$Production*dfProduction$AreaHarvested*0.1*dfProduction$Calories
# calculate individual yields and remove very unrealistic values
dfProduction$Yield <-dfProduction$Production / dfProduction$AreaHarvested
hist(dfProduction$Yield)
dfProduction<-dfProduction[dfProduction$Yield<1e+08,]

# keep necessary columns only 
dfProduction <- dfProduction[,c("Level","Item","Year","AreaHarvested","Production")]

# target Period
dfProduction <- dfProduction[which(dfProduction$Year%in%c(1998:2017)),]
# only keep crops with 10 entries per time period
dfProduction$timePeriod=0
dfProduction[dfProduction$Year%in%c(1998:2007),"timePeriod"] = 1998
dfProduction[dfProduction$Year%in%c(2008:2017),"timePeriod"] = 2008

sum(is.na(dfProduction))
dfProduction$sum <- 1
dfCount <- aggregate(sum~Level+timePeriod+Item,dfProduction,sum)
head(dfCount)

dfProduction <- merge(dfProduction[,c("Level","Item","Year","AreaHarvested","Production","timePeriod")],dfCount)
dfProduction <- dfProduction[which(dfProduction$sum==10),c("Level","Item","Year","AreaHarvested","Production")]
length(unique(dfProduction$Item)) ## 28 crops
length(unique(dfProduction$Level)) ## 28 5215 farmers

nrow(unique(dfProduction[,c("Level","Year","Item")])) == nrow(dfProduction) # check duplicates


#### calculate yields
sum(is.na(dfProduction))
dfYield <- aggregate(cbind(Production,AreaHarvested)~Level+Year,dfProduction,sum)
head(dfYield)
dfYield$Yield <- dfYield$Production/dfYield$AreaHarvested
nrow(unique(dfYield[,c("Level","Year")])) == nrow(dfYield) # check duplicates


#### calculate effective diversity (exp of shannon)
dfShannon <- aggregate(AreaHarvested~Level+Year,dfProduction,function(x){exp(diversity(x,index="shannon"))})
head(dfShannon)
names(dfShannon)[3] <- "diversity"
nrow(dfShannon)==nrow(dfYield)
nrow(unique(dfShannon[,c("Level","Year")])) == nrow(dfShannon) # check duplicates


#### Climate
dfClimateFinal <- read.csv("datasetsDerived/climate_farm.csv")
head(dfClimateFinal)
names(dfClimateFinal)[1] <- "REGION_ID"



######## CALCULATE VARIABLES FOR THE 2 TIME PERIOS

# aggregate over time periods
vecLevelFinal <- Reduce(intersect,list(dfManagement$Level,dfProduction$Level,dfYield$Level,dfShannon$Level))
length(vecLevelFinal) # 5215

# adapt dfProduction
dfProduction <- dfProduction[which(dfProduction$Level%in%vecLevelFinal),]

## summarize per time frame
lsAll <- lapply(vecLevelFinal,function(lev){
  # total production
  show(lev)
  reg <- unique(dfManagement[which(dfManagement$Level==lev),"REGION_ID"])
  if (length(reg)==1)
  {
    lsAggregate <- lapply(c(1998,2008),function(yearStart){
      
      sumLevel <- sum(dfYield$Level==lev&dfYield$Year>=yearStart&dfYield$Year<=(yearStart+9))
      if(sumLevel==10){
        # get global production data for target country
        dfProductionOverall <- dfProduction[which(dfProduction$Year>=yearStart&dfProduction$Year<=(yearStart+9)),]
        vecItem <- unique(dfProductionOverall[which(dfProductionOverall$Level==lev),"Item"])
        dfProductionOverall <- dfProductionOverall[which(dfProductionOverall$Item%in%vecItem),]
        dfProductionOverallAgg <- aggregate(cbind(Production,AreaHarvested)~Year,dfProductionOverall,sum)
        dfProductionOverallAgg$Yield <- dfProductionOverallAgg$Production/dfProductionOverallAgg$AreaHarvested
        dfProductionOverallAgg$YieldDet <-resid(lm(Yield ~ Year^2,data=dfProductionOverallAgg)) 

        # global production withouth the target country
        dfProductionReduced <- dfProductionOverall[-which(dfProductionOverall$Level==lev),]
        dfProductionReducedAgg <- aggregate(cbind(Production,AreaHarvested)~Year,dfProductionReduced,sum)
        dfProductionReducedAgg$Yield <- dfProductionReducedAgg$Production/dfProductionReducedAgg$AreaHarvested
        dfProductionReducedAgg$YieldDet <-resid(lm(Yield ~ Year^2,data=dfProductionReducedAgg)) 
        
        # subset data for the target country
        dfProductionSumLevel <- dfYield[which(dfYield$Level==lev&dfYield$Year>=yearStart&dfYield$Year<=(yearStart+9)),]
        dfProductionSumLevel$YieldDet <- resid(lm(Yield ~ Year^2,data=dfProductionSumLevel))
        dfShannonLevel <- dfShannon[which(dfShannon$Level==lev&dfShannon$Year>=yearStart&dfShannon$Year<=(yearStart+9)),]
        dfManagementLevel <- dfManagement[which(dfManagement$Level==lev&dfManagement$Year>=yearStart&dfManagement$Year<=(yearStart+9)),]
        dfClimateLevel <- dfClimateFinal[which(dfClimateFinal$REGION_ID==reg&dfClimateFinal$Year>=yearStart&dfClimateFinal$Year<=(yearStart+9)),]
        
        dfSummary <- data.frame(Level=lev, timePeriod= yearStart, REGION_ID=unique(dfManagementLevel$REGION_ID))
        dfSummary$stability <- mean(dfProductionSumLevel$Yield,na.rm=T)/sd(dfProductionSumLevel$YieldDet,na.rm=T)
        dfSummary$stabilityOverall <- mean(dfProductionOverallAgg$Yield,na.rm=T)/sd(dfProductionOverallAgg$YieldDet,na.rm=T)
        dfSummary$stabilityReduced <- mean(dfProductionReducedAgg$Yield,na.rm=T)/sd(dfProductionReducedAgg$YieldDet,na.rm=T)
        dfSummary$yield <- mean(dfProductionSumLevel$Yield,na.rm=T)
        dfSummary$yieldOverall <- mean(dfProductionOverallAgg$Yield,na.rm=T)
        dfSummary$yieldReduced <- mean(dfProductionReducedAgg$Yield,na.rm=T)
        dfSummary$areaHarvested <- mean(dfProductionSumLevel$AreaHarvested,na.rm=T)
        dfSummary$areaHarvestedOverall <-  mean(dfProductionOverallAgg$AreaHarvested) 
        dfSummary$areaHarvestedReduced <-  mean(dfProductionReducedAgg$AreaHarvested)  
        dfSummary$diversity <- mean(dfShannonLevel$diversity,na.rm=T)
        dfSummary$meanCropland <- mean(dfManagementLevel$croplandArea,na.rm=T)
        dfSummary$meanFertilizer <- mean(dfManagementLevel$fertilizer,na.rm=T)
        dfSummary$instabilityTemp <- -(mean(dfClimateLevel$meanTemp,na.rm=T)/sd(dfClimateLevel$meanTemp,na.rm=T))
        dfSummary$instabilityPrec <- -(mean(dfClimateLevel$meanPrec,na.rm=T)/sd(dfClimateLevel$meanPrec,na.rm=T))
        dfSummary$sdTemp <- mean(dfClimateLevel$sdTemp,na.rm=T)
        dfSummary$sdPrec <- mean(dfClimateLevel$sdPrec,na.rm=T)
        na.omit(dfSummary)
      }
    })    
    do.call(rbind,lsAggregate) 
  }    
})
dfAll <- do.call(rbind,lsAll)
head(dfAll)
nrow(unique(dfAll[,c("Level","timePeriod")])) == nrow(dfAll) # check duplicates
length(unique(dfAll$Level))

### add irrigation on the state level
dfIrrigation <- read.csv("datasets/irrigation_europe.csv")
head(dfIrrigation)
dfIrrigationAgg <- aggregate(values~geo,dfIrrigation,function(i){mean(i,na.rm=T)})
head(dfIrrigationAgg)
names(dfIrrigationAgg) <- c("stateEurostat","irrigation")
dfIrrigationAgg <- merge(dfIrrigationAgg,dfDistrict[,c("stateEurostat","REGION_ID")],by="stateEurostat")
dfIrrigationAgg <- unique(dfIrrigationAgg[,c("REGION_ID","irrigation")])

dfAll <- merge(dfAll,dfIrrigationAgg[,c("REGION_ID","irrigation")],by="REGION_ID")
sum(is.na(dfAll))

## calculate nitrogen per ha
dfAll$fertilizer <- dfAll$meanFertilizer/dfAll$meanCropland

# check NA
sum(is.na(dfAll))
## omit NA
dfAll <- na.omit(dfAll)
length(unique(dfAll$Level)) ## 5183 farmers

# remove very implausible values (> 1e15) 
dfAll[which(dfAll$stability>1e15),c("Level","timePeriod","stability")] # two different farms in 2008-2017
dfAll <- dfAll[-which(dfAll$stability>1e15),]
mean(dfAll$stability) # 10.97222
## save the dataframe 
names(dfAll)[2] <- "Farm"
dfAll <- dfAll[,c("Farm","REGION_ID","timePeriod",
                        "stability","stabilityOverall","stabilityReduced","yield","yieldOverall","yieldReduced","areaHarvested","areaHarvestedOverall","areaHarvestedReduced",
                        "diversity","fertilizer","irrigation",
                        "instabilityTemp","instabilityPrec","sdTemp","sdPrec"
                        )]

write.csv(dfAll,"P:/dataFinal_farm.csv",row.names=F)


rm(list=ls())

