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
dfClimateFinalPrint <- dfClimateFinalPrint[,c(1,36:121)]

# merge
dfClimateFinalArea <- merge(dfClimateID,dfClimateFinalPrint,by="cellID")
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
dfClimateFinal <- dfClimateFinalAreaAgg %>% gather(climYear, Value, names(dfClimateFinalAreaAgg)[2:81])
head(dfClimateFinal)
dfClimateFinal$Year <- as.numeric(substr(dfClimateFinal$climYear,9,12))
dfClimateFinal$Element <- substr(dfClimateFinal$climYear,1,8)
dfClimateFinal <- dfClimateFinal[,c("NUTS_ID","Year","Element","Value")]
dfClimateFinal <- dfClimateFinal %>% spread(Element, Value)
head(dfClimateFinal)
nrow(dfClimateFinal)

# target columns
dfClimateFinal <- dfClimateFinal[,c("NUTS_ID","Year","meanTemp","meanPrec")]
names(dfClimateFinal)[1] <- "Level"
nrow(unique(dfClimateFinal[,c("Level","Year")])) == nrow(dfClimateFinal) # check duplicates




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
      
      # subset data for the target country
      dfProductionSumLevel <- dfYield[which(dfYield$Level==lev&dfYield$Year>=yearStart&dfYield$Year<=(yearStart+9)),]
      dfProductionSumLevel$YieldDet <- resid(lm(Yield ~ Year^2,data=dfProductionSumLevel))
      dfShannonLevel <- dfShannon[which(dfShannon$Level==lev&dfShannon$Year>=yearStart&dfShannon$Year<=(yearStart+9)),]
      dfClimateLevel <- dfClimateFinal[which(dfClimateFinal$Level==lev&dfClimateFinal$Year>=yearStart&dfClimateFinal$Year<=(yearStart+9)),]
      
      dfSummary <- data.frame(Level=lev, timePeriod= yearStart)
      dfSummary$stability <- mean(dfProductionSumLevel$Yield,na.rm=T)/sd(dfProductionSumLevel$YieldDet,na.rm=T)
      dfSummary$stabilityOverall <- mean(dfProductionOverallAgg$Yield,na.rm=T)/sd(dfProductionOverallAgg$YieldDet,na.rm=T)
      dfSummary$yield <- mean(dfProductionSumLevel$Yield,na.rm=T)
      dfSummary$yieldOverall <- mean(dfProductionOverallAgg$Yield,na.rm=T)
      dfSummary$areaHarvested <- mean(dfProductionSumLevel$AreaHarvested,na.rm=T)
      dfSummary$areaHarvestedOverall <-  mean(dfProductionOverallAgg$AreaHarvested) 
      dfSummary$diversity <- mean(dfShannonLevel$diversity,na.rm=T)
      dfSummary$instabilityTemp <- -(mean(dfClimateLevel$meanTemp,na.rm=T)/sd(dfClimateLevel$meanTemp,na.rm=T))
      dfSummary$instabilityPrec <- -(mean(dfClimateLevel$meanPrec,na.rm=T)/sd(dfClimateLevel$meanPrec,na.rm=T))
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


## save dataframe
names(dfAll)
dfAll <- dfAll[,c("Region","Country","timePeriod",
                  "stability","stabilityOverall","yield","yieldOverall","areaHarvested","areaHarvestedOverall",
                  "diversity",
                  "instabilityTemp","instabilityPrec")]

write.csv(dfAll, "datasetsDerived/dataFinal_europe.csv",row.names=F)


rm(list=ls())

