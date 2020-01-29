library(rgdal)
library(tidyr)
library(vegan)
library(countrycode)
library(rworldmap) 

######## DATA PREPARATION

#### currently exisiting regions  
regionMap <- getMap()
plot(regionMap)
vecRegion <- regionMap@data$ISO3 

#### cropland data
dfCropland <- read.csv("datasets/cropland_global.csv")
# adapt region names
dfCropland$ISO3 <- countrycode(dfCropland$Area, 'country.name', 'iso3c') # Swaziland missing
levels(dfCropland$Area) <- c(levels(dfCropland$Area),"Swaziland")
dfCropland[which(grepl("Eswatini",dfCropland$Area)),"Area"] <- "Swaziland"
dfCropland$ISO3 <- countrycode(dfCropland$Area, 'country.name', 'iso3c') # no important regions missing
dfCropland<-dfCropland[which(dfCropland$Area!="China, mainland"),]
sort(as.character(setdiff(vecRegion,dfCropland$ISO3)))
dfCropland <- dfCropland[which(dfCropland$ISO3%in%vecRegion),] # only keep current regions

# keep necessary columns
dfCropland <- dfCropland[,c("ISO3","Year","Value")]
dfCropland$croplandArea <- dfCropland$Value*1000 # convert to ha
dfCropland <- dfCropland[,c("ISO3","Year","croplandArea")]

# only keep the regions covering 99.9% of the total cropland area 
sum(is.na(dfCropland))
dfCroplandMean <- aggregate(croplandArea~ISO3,dfCropland,mean)
head(dfCroplandMean)
dfCroplandMean$propArea <- dfCroplandMean$croplandArea/sum(dfCroplandMean$croplandArea)
dfCroplandMean <- dfCroplandMean[order(dfCroplandMean$propArea,decreasing = T),]
dfCroplandMean$cumArea <- cumsum(dfCroplandMean$propArea)
ind <- which(dfCroplandMean$cumArea>=0.999)[1]
dfCropland <- dfCropland[which(dfCropland$ISO3%in%dfCroplandMean[1:ind,"ISO3"]),] # this are the target regions
vecRegion <- unique(dfCropland$ISO3)

nrow(unique(dfCropland[,c("ISO3","Year")])) == nrow(dfCropland) # check duplicates


#### agricultural production data
dfProduction <- read.csv("datasets/agriculturalProduction_global.csv")
names(dfProduction)

# adapt region names
dfProduction$ISO3 <- countrycode(dfProduction$Area, 'country.name', 'iso3c') # Swaziland missing
levels(dfProduction$Area) <- c(levels(dfProduction$Area),"Swaziland")
dfProduction[which(grepl("Eswatini",dfProduction$Area)),"Area"] <- "Swaziland"
dfProduction$ISO3 <- countrycode(dfProduction$Area, 'country.name', 'iso3c') # no important regions missing
dfProduction<-dfProduction[which(dfProduction$Area!="China, mainland"),]
sort(as.character(setdiff(vecRegion,dfProduction$ISO3)))
dfProduction <- dfProduction[which(dfProduction$ISO3%in%vecRegion),] # only keep current regions

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
dfProduction[is.na(dfProduction$Area.Harvested),"Production"] <- NA
dfProduction <- dfProduction[which(!is.na(dfProduction$AreaHarvested) & !is.na(dfProduction$Production)),]
# remove zero areas and production
dfProduction <- dfProduction[-which(dfProduction$AreaHarvested==0 |  dfProduction$Production==0),]

# add calories and make crops consistent with target crop file
dfCalories <- read.csv("datasets/targetCrops_global.csv")
head(dfCalories) # Group2 is the target crop name
names(dfCalories)[2] <- "Item"
dfCalories <- dfCalories[which(!is.na(dfCalories$Group2)&!is.na(dfCalories$Calories)),]
setdiff(dfProduction$Item,dfCalories$Item)

# subset crops without calories
dfProduction <- merge(dfProduction,dfCalories[,c("Item","Calories")],by="Item")
names(dfProduction)

# change production to calories
dfProduction$Production <- dfProduction$Production*dfProduction$Calories
# calculate individual yields and remove very unrealistic values
dfProduction$Yield <-dfProduction$Production / dfProduction$AreaHarvested
hist(dfProduction$Yield)
dfProduction<-dfProduction[dfProduction$Yield<1e+08,]

# keep necessary columns only 
dfProduction <- dfProduction[,c("ISO3","Item","Year","AreaHarvested","Production")]

# Remove Spices, nes and onions dry 
dfProduction<-dfProduction[-which(dfProduction$Item%in%c("Spices, nes","Onions, dry")),]

# only keep crops with 10 entries per time period
dfProduction$timePeriod=0
dfProduction[dfProduction$Year%in%c(1968:1977),"timePeriod"] = 1968
dfProduction[dfProduction$Year%in%c(1978:1987),"timePeriod"] = 1978
dfProduction[dfProduction$Year%in%c(1988:1997),"timePeriod"] = 1988
dfProduction[dfProduction$Year%in%c(1998:2007),"timePeriod"] = 1998
dfProduction[dfProduction$Year%in%c(2008:2017),"timePeriod"] = 2008

sum(is.na(dfProduction))
dfProduction$sum <- 1
dfCount <- aggregate(sum~ISO3+timePeriod+Item,dfProduction,sum)
head(dfCount)

dfProduction <- merge(dfProduction[,c("ISO3","Item","Year","AreaHarvested","Production","timePeriod")],dfCount)
dfProduction <- dfProduction[which(dfProduction$sum==10),c("ISO3","Item","Year","AreaHarvested","Production")]
length(unique(dfProduction$Item)) ## 130 crops

nrow(unique(dfProduction[,c("ISO3","Year","Item")])) == nrow(dfProduction) # check duplicates

#### calculate yields
sum(is.na(dfProduction))
dfYield <- aggregate(cbind(Production,AreaHarvested)~ISO3+Year,dfProduction,sum)
head(dfYield)
dfYield$Yield <- dfYield$Production/dfYield$AreaHarvested
nrow(unique(dfYield[,c("ISO3","Year")])) == nrow(dfYield) # check duplicates


#### calculate effective diversity (exp of shannon)
dfShannon <- aggregate(AreaHarvested~ISO3+Year,dfProduction,function(x){exp(diversity(x,index="shannon"))})
head(dfShannon)
names(dfShannon)[3] <- "diversity"
nrow(dfShannon)==nrow(dfYield)
nrow(unique(dfShannon[,c("ISO3","Year")])) == nrow(dfShannon) # check duplicates


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
dfFertilizer$ISO3 <- countrycode(dfFertilizer$Area, 'country.name', 'iso3c') # Swaziland missing
levels(dfFertilizer$Area) <- c(levels(dfFertilizer$Area),"Swaziland")
dfFertilizer[which(grepl("Eswatini",dfFertilizer$Area)),"Area"] <- "Swaziland"
dfFertilizer$ISO3 <- countrycode(dfFertilizer$Area, 'country.name', 'iso3c') # no important regions missing
dfFertilizer<-dfFertilizer[which(dfFertilizer$Area!="China, mainland"),]
sort(as.character(setdiff(vecRegion,dfFertilizer$ISO3)))
dfFertilizer <- dfFertilizer[which(dfFertilizer$ISO3%in%vecRegion),] # only keep current regions

# target columns
dfFertilizer <- dfFertilizer[,c("ISO3","Year","Nitrogen")]

nrow(unique(dfFertilizer[,c("ISO3","Year")])) == nrow(dfFertilizer) # check duplicates


## Irrigation
dfIrrigation <- read.csv("datasets/irrigationEquippedArea_global.csv")
head(dfIrrigation)

# only keep target year
dfIrrigation <- dfIrrigation[which(dfIrrigation$Year%in%1968:2017),]

# adapt region names
dfIrrigation$ISO3 <- countrycode(dfIrrigation$Area, 'country.name', 'iso3c') # Swaziland missing
levels(dfIrrigation$Area) <- c(levels(dfIrrigation$Area),"Swaziland")
dfIrrigation[which(grepl("Eswatini",dfIrrigation$Area)),"Area"] <- "Swaziland"
dfIrrigation$ISO3 <- countrycode(dfIrrigation$Area, 'country.name', 'iso3c') # no important regions missing
dfIrrigation<-dfIrrigation[which(dfIrrigation$Area!="China, mainland"),]
sort(as.character(setdiff(vecRegion,dfIrrigation$ISO3)))
dfIrrigation <- dfIrrigation[which(dfIrrigation$ISO3%in%vecRegion),] # only keep current regions
names(dfIrrigation)[12] <- "Irrigation"

# target columns
dfIrrigation <- dfIrrigation[,c("ISO3","Year","Irrigation")]
nrow(unique(dfIrrigation[,c("ISO3","Year")])) == nrow(dfIrrigation) # check duplicates


#### Climate
# intersect file (intersection of country borders and climate pixels)
shpClimateID <- readOGR("spatial","countriesClimateID_global")
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
dfClimateFinalPrint <- dfClimateFinalPrint[,c(1,16:121)]

# merge
dfClimateFinalArea <- merge(dfClimateID,dfClimateFinalPrint,by="cellID")
head(dfClimateFinalArea)
length(unique(dfClimateFinalArea$cellID)) / nrow(dfClimateFinalArea)
names(dfClimateFinalArea)
# get cropland by region
dfCroplandTot <- aggregate(cbind(cropland1970AD,cropland1980AD,cropland1990AD,cropland2000AD,cropland2010AD)~Area,dfClimateFinalArea,sum)
head(dfCroplandTot)
names(dfCroplandTot)[2:6] <- paste0(names(dfCroplandTot)[2:6],"Tot")
dfClimateFinalArea <- merge(dfClimateFinalArea,dfCroplandTot,by="Area")
head(dfClimateFinalArea)
names(dfClimateFinalArea)
## weighted average: get area of cropland attributed to a cell segment (i.e. cropland area multiplied by area share of the segment), divide it by total cropland area across region
dfClimateFinalArea$weight1 <- (((dfClimateFinalArea$areaHA/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland1970AD)/dfClimateFinalArea$cropland1970ADTot)
dfClimateFinalArea$weight2 <- (((dfClimateFinalArea$areaHA/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland1980AD)/dfClimateFinalArea$cropland1980ADTot)
dfClimateFinalArea$weight3 <- (((dfClimateFinalArea$areaHA/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland1990AD)/dfClimateFinalArea$cropland1990ADTot)
dfClimateFinalArea$weight4 <- (((dfClimateFinalArea$areaHA/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland2000AD)/dfClimateFinalArea$cropland2000ADTot)
dfClimateFinalArea$weight5 <- (((dfClimateFinalArea$areaHA/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland2010AD)/dfClimateFinalArea$cropland2010ADTot)
hist(dfClimateFinalArea$weight5)

## multiply weight by climate values
names(dfClimateFinalArea)
dfClimateFinalArea[,7:26] <- dfClimateFinalArea[,7:26]*dfClimateFinalArea$weight1
dfClimateFinalArea[,27:46] <- dfClimateFinalArea[,27:46]*dfClimateFinalArea$weight2
dfClimateFinalArea[,47:66] <- dfClimateFinalArea[,47:66]*dfClimateFinalArea$weight3
dfClimateFinalArea[,67:86] <- dfClimateFinalArea[,67:86]*dfClimateFinalArea$weight4
dfClimateFinalArea[,87:106] <- dfClimateFinalArea[,87:106]*dfClimateFinalArea$weight5

# sum weighted values to get overall weighted average
names(dfClimateFinalArea)
dfClimateFinalAreaAgg <- aggregate(dfClimateFinalArea[,7:106],by=list(dfClimateFinalArea$Area),FUN=function(i){sum(i,na.rm=T)})
head(dfClimateFinalAreaAgg)
names(dfClimateFinalAreaAgg)[1] <- "Area"
min(dfClimateFinalAreaAgg[2:101]) # check for negative values -> would be problematic for instability calculation
sum(dfClimateFinalAreaAgg[2:101]<0) 
dfClimateFinalAreaAgg$neg <- apply(dfClimateFinalAreaAgg[,2:101],1,function(r){sum(r<0)})
dfClimateFinalAreaAgg <- dfClimateFinalAreaAgg[which(dfClimateFinalAreaAgg$neg==0),1:101]

# change structure
names(dfClimateFinalAreaAgg)
dfClimateFinal <- dfClimateFinalAreaAgg %>% gather(climYear, Value, names(dfClimateFinalAreaAgg)[2:101])
head(dfClimateFinal)
dfClimateFinal$Year <- as.numeric(substr(dfClimateFinal$climYear,9,12))
dfClimateFinal$Element <- substr(dfClimateFinal$climYear,1,8)
dfClimateFinal <- dfClimateFinal[,c("Area","Year","Element","Value")]
dfClimateFinal <- dfClimateFinal %>% spread(Element, Value)
head(dfClimateFinal)
nrow(dfClimateFinal)

# adapt region names
dfClimateFinal$ISO3 <- countrycode(dfClimateFinal$Area, 'country.name', 'iso3c') # no important regions missing
dfClimateFinal<-dfClimateFinal[which(dfClimateFinal$Area!="China, mainland"),]
sort(as.character(setdiff(vecRegion,dfClimateFinal$ISO3)))
dfClimateFinal <- dfClimateFinal[which(dfClimateFinal$ISO3%in%vecRegion),] # only keep current regions

# target columns
dfClimateFinal <- dfClimateFinal[,c("ISO3","Year","meanTemp","meanPrec")]
nrow(unique(dfClimateFinal[,c("ISO3","Year")])) == nrow(dfClimateFinal) # check duplicates




######## CALCULATE VARIABLE FOR THE 5 TIME PERIOS

# get regions across datasets
vecCRegionFinal <- Reduce(intersect,list(dfCropland$ISO3,dfProduction$ISO3,dfYield$ISO3,dfShannon$ISO3,dfFertilizer$ISO3,dfIrrigation$ISO3,dfClimateFinal$ISO3))

# remove countries listed by Renard & Tilman 2019
vecCRegionFinal <- vecCRegionFinal[-which(vecCRegionFinal%in%c("EGY","PRK", "GIN", "KEN","MOZ","ZMB","IRL","NLD","NZL"))] 

# remove regions in produciton file (needed for scale calculations)
dfProduction <- dfProduction[which(dfProduction$ISO3%in%vecCRegionFinal),]
save(dfProduction, file="datasetsDerived/dfProduction_global.RData")

## summarize per time frame 
lsAll <- lapply(vecCRegionFinal,function(reg){
  # detrend yields
  show(as.character(reg))
  lsAggregate <- lapply(c(1968,1978,1988,1998,2008),function(yearStart){
    
    sumReg <- sum(dfYield$ISO3==reg&dfYield$Year>=yearStart&dfYield$Year<=(yearStart+9))
    if(sumReg==10){
      # subset data for the target country
      dfProductionSumReg <- dfYield[which(dfYield$ISO3==reg&dfYield$Year>=yearStart&dfYield$Year<=(yearStart+9)),]
      dfProductionSumReg$YieldDet <- resid(lm(Yield ~ Year^2,data=dfProductionSumReg))
      dfShannonReg <- dfShannon[which(dfShannon$ISO3==reg&dfShannon$Year>=yearStart&dfShannon$Year<=(yearStart+9)),]
      dfCroplandReg <- dfCropland[which(dfCropland$ISO3==reg&dfCropland$Year>=yearStart&dfCropland$Year<=(yearStart+9)),]
      dfFertilizerReg <- dfFertilizer[which(dfFertilizer$ISO3==reg&dfFertilizer$Year>=yearStart&dfFertilizer$Year<=(yearStart+9)),]
      dfIrrigationReg <- dfIrrigation[which(dfIrrigation$ISO3==reg&dfIrrigation$Year>=yearStart&dfIrrigation$Year<=(yearStart+9)),]
      dfClimateReg <- dfClimateFinal[which(dfClimateFinal$ISO3==reg&dfClimateFinal$Year>=yearStart&dfClimateFinal$Year<=(yearStart+9)),]
    
      dfSummary <- data.frame(ISO3=reg, timePeriod= yearStart)
      dfSummary$stability <- mean(dfProductionSumReg$Yield,na.rm=T)/sd(dfProductionSumReg$YieldDet,na.rm=T)
      dfSummary$yield <- mean(dfProductionSumReg$Yield,na.rm=T)
      dfSummary$areaHarvested <- mean(dfProductionSumReg$AreaHarvested,na.rm=T)
      dfSummary$diversity <- mean(dfShannonReg$diversity,na.rm=T)
      dfSummary$meanCropland <- mean(dfCroplandReg$croplandArea,na.rm=T)
      dfSummary$meanNitrogen <- mean(dfFertilizerReg$Nitrogen,na.rm=T)
      dfSummary$meanIrrigation_share <- mean(dfIrrigationReg$Irrigation,na.rm=T)
      dfSummary$instabilityTemp <- -(mean(dfClimateReg$meanTemp,na.rm=T)/sd(dfClimateReg$meanTemp,na.rm=T))
      dfSummary$instabilityPrec <- -(mean(dfClimateReg$meanPrec,na.rm=T)/sd(dfClimateReg$meanPrec,na.rm=T))
      na.omit(dfSummary)
    }
  })
  do.call(rbind,lsAggregate)
})
dfAll <- do.call(rbind,lsAll)
head(dfAll)
nrow(unique(dfAll[,c("ISO3","timePeriod")])) == nrow(dfAll) # check duplicates

unique(dfAll$timePeriod)
head(dfAll)

## calculate nitrogen per ha
dfAll$meanFertilizer <- dfAll$meanNitrogen/dfAll$meanCropland
# check NA
sum(is.na(dfAll))
## omit NA
dfAll <- na.omit(dfAll)
length(unique(dfAll$ISO3)) ## 135 countries

## save dataframe
names(dfAll)
names(dfAll)[1] <- "Country"
dfAll <- dfAll[,c("Country","timePeriod",
                  "stability","yield","areaHarvested",
                  "diversity","meanNitrogen","meanIrrigation_share",
                  "instabilityTemp","instabilityPrec")]
write.csv(dfAll, "datasetsDerived/dataFinal_global.csv",row.names=F)



rm(list=ls())
