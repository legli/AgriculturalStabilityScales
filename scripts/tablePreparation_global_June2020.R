library(rgdal)
library(tidyr)
library(vegan)
library(countrycode)
library(rworldmap) 

######## DATA PREPARATION

#### currently exisiting regions  
levelMap <- getMap()
plot(levelMap)
vecLevel <- levelMap@data$ISO3 

#### cropland data
dfCropland <- read.csv("datasets/cropland_global.csv")
# adapt region names
dfCropland$Level <- countrycode(dfCropland$Area, 'country.name', 'iso3c') # Swaziland missing
levels(dfCropland$Area) <- c(levels(dfCropland$Area),"Swaziland")
dfCropland[which(grepl("Eswatini",dfCropland$Area)),"Area"] <- "Swaziland"
dfCropland$Level <- countrycode(dfCropland$Area, 'country.name', 'iso3c') # no important regions missing
dfCropland<-dfCropland[which(dfCropland$Area!="China, mainland"),]
sort(as.character(setdiff(vecLevel,dfCropland$Level)))
dfCropland <- dfCropland[which(dfCropland$Level%in%vecLevel),] # only keep current regions

# keep necessary columns
dfCropland <- dfCropland[,c("Level","Year","Value")]
dfCropland$croplandArea <- dfCropland$Value*1000 # convert to ha
dfCropland <- dfCropland[,c("Level","Year","croplandArea")]

# only keep the regions covering 99.9% of the total cropland area 
sum(is.na(dfCropland))
dfCroplandMean <- aggregate(croplandArea~Level,dfCropland,mean)
head(dfCroplandMean)
dfCroplandMean$propArea <- dfCroplandMean$croplandArea/sum(dfCroplandMean$croplandArea)
dfCroplandMean <- dfCroplandMean[order(dfCroplandMean$propArea,decreasing = T),]
dfCroplandMean$cumArea <- cumsum(dfCroplandMean$propArea)
ind <- which(dfCroplandMean$cumArea>=0.999)[1]
dfCropland <- dfCropland[which(dfCropland$Level%in%dfCroplandMean[1:ind,"Level"]),] # this are the target regions
vecLevel <- unique(dfCropland$Level)

nrow(unique(dfCropland[,c("Level","Year")])) == nrow(dfCropland) # check duplicates


#### agricultural production data
dfProduction <- read.csv("datasets/agriculturalProduction_global.csv")
names(dfProduction)

# adapt region names
dfProduction$Level <- countrycode(dfProduction$Area, 'country.name', 'iso3c') # Swaziland missing
levels(dfProduction$Area) <- c(levels(dfProduction$Area),"Swaziland")
dfProduction[which(grepl("Eswatini",dfProduction$Area)),"Area"] <- "Swaziland"
dfProduction$Level <- countrycode(dfProduction$Area, 'country.name', 'iso3c') # no important regions missing
dfProduction<-dfProduction[which(dfProduction$Area!="China, mainland"),]
sort(as.character(setdiff(vecLevel,dfProduction$Level)))
dfProduction <- dfProduction[which(dfProduction$Level%in%vecLevel),] # only keep current regions

# only keep area harvested and production, relevant columns and target years
names(dfProduction)
dfProduction <- dfProduction[which(dfProduction$Element=="Area harvested" | dfProduction$Element == "Production"),c(65,4,6,15:64)]
head(dfProduction)
names(dfProduction)[4:53] <- 1968:2017 # change colnames to years

# change dataset structure (frow wide to long)
dfProduction <- dfProduction %>% gather(Year, Value, "1968":"2017")
head(dfProduction)
dfProduction$Year <- as.numeric(dfProduction$Year)
# remove NA
dfProduction <- na.omit(dfProduction) # to ensure unique keys
# split area harvested and production in two columns (from long to wide)
dfProduction <- dfProduction %>% spread(Element, Value)
head(dfProduction)
names(dfProduction)[4] <- "AreaHarvested"

# set NA for harvested data for which there is no production data and vice versa 
dfProduction[is.na(dfProduction$Production),"AreaHarvested"] <- NA
dfProduction[is.na(dfProduction$AreaHarvested),"Production"] <- NA
dfProduction <- dfProduction[which(!is.na(dfProduction$AreaHarvested) & !is.na(dfProduction$Production)),]
# remove zero areas and production
dfProduction <- dfProduction[-which(dfProduction$AreaHarvested==0 |  dfProduction$Production==0),]

# add calories and make crops consistent with target crop file
dfCalories <- read.csv("datasets/targetCrops_global.csv")
head(dfCalories) 
dfCalories <- dfCalories[which(!is.na(dfCalories$Calories_kcal_t)),]
setdiff(dfProduction$Item,dfCalories$Item)

# subset crops without calories
dfProduction <- merge(dfProduction,dfCalories[,c("Item","Calories_kcal_t")],by="Item")
names(dfProduction)

# change production to calories
dfProduction$Production <- dfProduction$Production*dfProduction$Calories_kcal_t
# calculate individual yields and remove very unrealistic values
dfProduction$Yield <-dfProduction$Production / dfProduction$AreaHarvested
hist(dfProduction$Yield)
dfProduction<-dfProduction[dfProduction$Yield<1e+08,]

# keep necessary columns only 
dfProduction <- dfProduction[,c("Level","Item","Year","AreaHarvested","Production")]

# only keep crops with 10 entries per time period
dfProduction$timePeriod=0
dfProduction[dfProduction$Year%in%c(1968:1977),"timePeriod"] = 1968
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
length(unique(dfProduction$Item)) ## 130 crops

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


#### Fertilizer data
dfFertilizerArchive <- read.csv("datasets/fertilizerArchive_global.csv")
names(dfFertilizerArchive)[4] <- "Area"
dfFertilizerNew <- read.csv("datasets/fertilizerNew_global.csv")
dfFertilizer <- rbind(dfFertilizerArchive[,c("Area","Year","Item","Value")],dfFertilizerNew[,c("Area","Year","Item","Value")])
dfFertilizer <- dfFertilizer[which(dfFertilizer$Item=="Nitrogenous fertilizers"| dfFertilizer$Item=="Nutrient nitrogen N (total)"),]
dfFertilizer <- dfFertilizer[,c("Area","Year","Value")]
names(dfFertilizer)[3] <- "Nitrogen"
head(dfFertilizer)

# only keep target year
dfFertilizer <- dfFertilizer[which(dfFertilizer$Year%in%1968:2017),]

# adapt region names
dfFertilizer$Level <- countrycode(dfFertilizer$Area, 'country.name', 'iso3c') # Swaziland missing
levels(dfFertilizer$Area) <- c(levels(dfFertilizer$Area),"Swaziland")
dfFertilizer[which(grepl("Eswatini",dfFertilizer$Area)),"Area"] <- "Swaziland"
dfFertilizer$Level <- countrycode(dfFertilizer$Area, 'country.name', 'iso3c') # no important regions missing
dfFertilizer<-dfFertilizer[which(dfFertilizer$Area!="China, mainland"),]
sort(as.character(setdiff(vecLevel,dfFertilizer$Level)))
dfFertilizer <- dfFertilizer[which(dfFertilizer$Level%in%vecLevel),] # only keep current regions

# target columns
dfFertilizer <- dfFertilizer[,c("Level","Year","Nitrogen")]
nrow(unique(dfFertilizer[,c("Level","Year")])) == nrow(dfFertilizer) # check duplicates


## Irrigation
dfIrrigation <- read.csv("datasets/irrigationEquippedArea_global.csv")
head(dfIrrigation)

# only keep target year
dfIrrigation <- dfIrrigation[which(dfIrrigation$Year%in%1968:2017),]

# adapt region names
dfIrrigation$Level <- countrycode(dfIrrigation$Area, 'country.name', 'iso3c') # Swaziland missing
levels(dfIrrigation$Area) <- c(levels(dfIrrigation$Area),"Swaziland")
dfIrrigation[which(grepl("Eswatini",dfIrrigation$Area)),"Area"] <- "Swaziland"
dfIrrigation$Level <- countrycode(dfIrrigation$Area, 'country.name', 'iso3c') # no important regions missing
dfIrrigation<-dfIrrigation[which(dfIrrigation$Area!="China, mainland"),]
sort(as.character(setdiff(vecLevel,dfIrrigation$Level)))
dfIrrigation <- dfIrrigation[which(dfIrrigation$Level%in%vecLevel),] # only keep current regions
names(dfIrrigation)[12] <- "Irrigation"

# target columns
dfIrrigation <- dfIrrigation[,c("Level","Year","Irrigation")]
nrow(unique(dfIrrigation[,c("Level","Year")])) == nrow(dfIrrigation) # check duplicates


#### Climate
dfClimateFinal <- read.csv("datasetsDerived/climate_national.csv")
head(dfClimateFinal)

######## CALCULATE VARIABLES FOR THE 5 TIME PERIOS

# get regions across datasets
vecLevelFinal <- Reduce(intersect,list(dfCropland$Level,dfProduction$Level,dfYield$Level,dfShannon$Level,dfFertilizer$Level,dfIrrigation$Level,dfClimateFinal$Level))

# remove countries listed by Renard & Tilman 2019
vecLevelFinal <- vecLevelFinal[-which(vecLevelFinal%in%c("EGY","PRK", "GIN", "KEN","MOZ","ZMB","IRL","NLD","NZL"))] 

# adapt dfProduction
dfProduction <- dfProduction[which(dfProduction$Level%in%vecLevelFinal),]

## summarize per time frame 
lsAll <- lapply(vecLevelFinal,function(lev){
  # detrend yields
  show(as.character(lev))
  lsAggregate <- lapply(c(1968,1978,1988,1998,2008),function(yearStart){
    
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
      dfCroplandLevel <- dfCropland[which(dfCropland$Level==lev&dfCropland$Year>=yearStart&dfCropland$Year<=(yearStart+9)),]
      dfFertilizerLevel <- dfFertilizer[which(dfFertilizer$Level==lev&dfFertilizer$Year>=yearStart&dfFertilizer$Year<=(yearStart+9)),]
      dfIrrigationLevel <- dfIrrigation[which(dfIrrigation$Level==lev&dfIrrigation$Year>=yearStart&dfIrrigation$Year<=(yearStart+9)),]
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
      dfSummary$meanCropland <- mean(dfCroplandLevel$croplandArea,na.rm=T)
      dfSummary$meanNitrogen <- mean(dfFertilizerLevel$Nitrogen,na.rm=T)
      dfSummary$irrigation <- mean(dfIrrigationLevel$Irrigation,na.rm=T)
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


## calculate nitrogen per ha
dfAll$fertilizer <- dfAll$meanNitrogen/dfAll$meanCropland
# check NA
sum(is.na(dfAll))
## omit NA
dfAll <- na.omit(dfAll)
length(unique(dfAll$Level)) ## 137 countries

## add regions
dfRegions <- read.csv("datasets/regions_global.csv")
head(dfRegions)
dfRegions$Area <- as.character(dfRegions$Area)
dfRegions[which(dfRegions$Area=="Eswatini"),"Area"] <- "Swaziland"
dfRegions$Country  <- countrycode(dfRegions$Area, 'country.name', 'iso3c') 
names(dfRegions)[2:3] <- c("Continent","Level")
unique(dfAll$Level)%in%dfRegions$Level
dfAll <- merge(dfAll,unique(dfRegions[,c("Level","Continent")]))

## save dataframe
names(dfAll)[1] <- "Country"
dfAll <- dfAll[,c("Country","Continent","timePeriod",
                  "stability","stabilityOverall","stabilityReduced","yield","yieldOverall","yieldReduced","areaHarvested","areaHarvestedOverall","areaHarvestedReduced",
                  "diversity","fertilizer","irrigation",
                  "instabilityTemp","instabilityPrec","sdTemp","sdPrec")]
write.csv(dfAll, "datasetsDerived/dataFinal_national.csv",row.names=F)

rm(list=ls())
