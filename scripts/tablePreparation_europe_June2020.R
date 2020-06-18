library(vegan)
library(rgdal)
library(tidyr)
library(countrycode)

######## DATA PREPARATION

#### agricultural production data
dfProduction <- read.csv("datasets/agriculturalProduction_europe.csv")
head(dfProduction,)
names(dfProduction)[1] <- "Item"
names(dfProduction)[3] <- "Level"
names(dfProduction)[4] <- "Year"
unique(dfProduction$Item)
unique(dfProduction$strucpro)
range(dfProduction$Year)


# only keep the regions covering 99.9% of the total cropland area 
dfCropland <- dfProduction[which(dfProduction$Item=="UAA"),]
sum(is.na(dfCropland))
dfCroplandMean <- aggregate(values~Level,dfCropland,mean)
head(dfCroplandMean)
dfCroplandMean$propArea <- dfCroplandMean$values/sum(dfCroplandMean$values)
dfCroplandMean <- dfCroplandMean[order(dfCroplandMean$propArea,decreasing = T),]
dfCroplandMean$cumArea <- cumsum(dfCroplandMean$propArea)
ind <- which(dfCroplandMean$cumArea>=0.999)[1]
dfCropland <- dfCropland[which(dfCropland$Level%in%dfCroplandMean[1:ind,"Level"]),] # this are the target regions
vecLevel <- unique(dfCropland$Level)

# only keep area harvested and production
dfProduction <- dfProduction[which(dfProduction$strucpro%in%c("AR","PR")),]
dfProduction <- dfProduction[which(dfProduction$Level%in%vecLevel),] # only keep current regions
# split area harvested and production in two columns (from long to wide)
dfProduction <- dfProduction %>% spread(strucpro, values)
head(dfProduction)
names(dfProduction)[4] <- "AreaHarvested"
names(dfProduction)[5] <- "Production"

# set NA for harvested data for which there is no production data and vice versa 
dfProduction[is.na(dfProduction$Production),"AreaHarvested"] <- NA
dfProduction[is.na(dfProduction$AreaHarvested),"Production"] <- NA
dfProduction <- dfProduction[which(!is.na(dfProduction$AreaHarvested) & !is.na(dfProduction$Production)),]
# remove zero areas and production
dfProduction <- dfProduction[-which(dfProduction$AreaHarvested==0 |  dfProduction$Production==0),]

# add calories and make crops consistent with target crop file
dfCalories <- read.csv("datasets/targetCrops_europe.csv")
head(dfCalories) # Group2 is the target crop name
names(dfCalories)[1] <- "Item"
dfCalories <- dfCalories[which(!is.na(dfCalories$Item)&!is.na(dfCalories$Calories)),]
setdiff(dfProduction$Item,dfCalories$Item)

# subset crops without calories
dfProduction <- merge(dfProduction,dfCalories[,c("Item","Calories")],by="Item")
names(dfProduction)

# change production to calories (from 1000t to t)
dfProduction$Production <- dfProduction$Production*dfProduction$Calories
# calculate individual yields and remove very unrealistic values
dfProduction$Yield <-dfProduction$Production / dfProduction$AreaHarvested
hist(dfProduction$Yield)
dfProduction<-dfProduction[dfProduction$Yield<1e+08,]

# keep necessary columns only 
dfProduction <- dfProduction[,c("Level","Item","Year","AreaHarvested","Production")]

# only keep crops with 10 entries per time period
dfProduction$timePeriod=0
dfProduction[dfProduction$Year%in%c(1978:1987),"timePeriod"] = 1978
dfProduction[dfProduction$Year%in%c(1988:1997),"timePeriod"] = 1988
dfProduction[dfProduction$Year%in%c(1998:2007),"timePeriod"] = 1998
dfProduction[dfProduction$Year%in%c(2008:2017),"timePeriod"] = 2008

sum(is.na(dfProduction))
dfProduction$sum <- 1
dfCount <- aggregate(sum~Level+timePeriod+Item,dfProduction,sum)
head(dfCount)

dfProduction <- merge(dfProduction[,c("Level","Item","Year","AreaHarvested","Production","timePeriod")],dfCount)
dfProduction <- dfProduction[which(dfProduction$sum==10),c("Level","Item","Year","AreaHarvested","Production")]
length(unique(dfProduction$Item)) ## 19 crops

nrow(unique(dfProduction[,c("Level","Year","Item")])) == nrow(dfProduction) # check duplicates

#area to ha
dfProduction$AreaHarvested <- dfProduction$AreaHarvested*1000


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
dfClimateFinal <- read.csv("datasetsDerived/climate_regional.csv")
head(dfClimateFinal)


######## CALCULATE VARIABLES FOR THE 4 TIME PERIOS

# get regions across datasets
vecLevelFinal <- Reduce(intersect,list(dfProduction$Level,dfYield$Level,dfShannon$Level,dfClimateFinal$Level))

# adapt dfProduction
dfProduction <- dfProduction[which(dfProduction$Level%in%vecLevelFinal),]

## summarize per time frame 
lsAll <- lapply(vecLevelFinal,function(lev){
  # total production
  show(as.character(lev))
  lsAggregate <- lapply(c(1978,1988,1998,2008),function(yearStart){

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
      dfClimateLevel <- dfClimateFinal[which(dfClimateFinal$Level==lev&dfClimateFinal$Year>=yearStart&dfClimateFinal$Year<=(yearStart+9)),]
      
      dfSummary <- data.frame(Level=lev, timePeriod= yearStart)
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
      dfSummary$instabilityTemp <- -(mean(dfClimateLevel$meanTemp,na.rm=T)/sd(dfClimateLevel$meanTemp,na.rm=T))
      dfSummary$instabilityPrec <- -(mean(dfClimateLevel$meanPrec,na.rm=T)/sd(dfClimateLevel$meanPrec,na.rm=T))
      dfSummary$sdTemp <- mean(dfClimateLevel$sdTemp,na.rm=T)
      dfSummary$sdPrec <- mean(dfClimateLevel$sdPrec,na.rm=T)
      na.omit(dfSummary)
    }
  })
  do.call(rbind,lsAggregate)
})
dfAll <- do.call(rbind,lsAll)
head(dfAll)
nrow(unique(dfAll[,c("Level","timePeriod")])) == nrow(dfAll) # check duplicates
unique(dfAll$timePeriod)
head(dfAll)

# check NA
sum(is.na(dfAll))
## omit NA
dfAll <- na.omit(dfAll)
length(unique(dfAll$Level)) ## 177 regions
nrow(dfAll) ## 424 data points


### add cntry code and NUTS names
names(dfAll)[1] <- "Region"
dfAll$ISO2 <- substr(dfAll$Region,1,2)
dfAll$Country <- countrycode(dfAll$ISO2, 'iso2c', 'iso3c') 
dfAll[which(dfAll$ISO2=="EL"),"ISO2"] <- "GR"
dfAll[which(dfAll$ISO2=="UK"),"ISO2"] <- "GB"
dfAll$Country <- countrycode(dfAll$ISO2, 'iso2c', 'iso3c') 

## add data from national level
dfCountry <- read.csv("datasetsDerived/dataFinal_national.csv")
setdiff(unique(dfAll[,c("Country","timePeriod")]),dfCountry[,c("Country","timePeriod")])
dfAll <- merge(dfAll,dfCountry[,c("Country","timePeriod","fertilizer","irrigation")],by=c("Country","timePeriod"))



## save dataframe
names(dfAll)
dfAll <- dfAll[,c("Region","Country","timePeriod",
                  "stability","stabilityOverall","stabilityReduced","yield","yieldOverall","yieldReduced","areaHarvested","areaHarvestedOverall","areaHarvestedReduced",
                  "diversity","fertilizer","irrigation",
                  "instabilityTemp","instabilityPrec","sdTemp","sdPrec")]

write.csv(dfAll, "datasetsDerived/dataFinal_regional.csv",row.names=F)

rm(list=ls())

