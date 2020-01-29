
# library(maptools)
# library(tidyr)


library(vegan)
library(data.table)
library(plyr)
library(dplyr)
library(countrycode)
library(rgdal)

################ DATA PREPARATION

######### Production data 
dfProduction <- read.csv("datasets/agriculturalProduction_global.csv") # raw production
dfCalories <- read.csv("datasets/cropCalories_global.csv") # calories
colnames(dfCalories)[1]<-"Item" # rename to match with production data

#### remove yield and mushrooms from the database
dfProduction <- droplevels(dfProduction[!dfProduction$Element == "Yield", ] ) 
dfProduction <- droplevels(dfProduction[!dfProduction$Item == "Mushrooms and truffles",]) 


#### restructure data to long format
dfProductionStr <- dfProduction[,-c(1,3,5,7)]
dfProductionStr  <- reshape(dfProductionStr[,c(1:length(dfProductionStr))], varying = names(dfProductionStr)[4:length(dfProductionStr)], v.names = "Value", timevar = "Year", 
                         times = names(dfProductionStr)[4:length(dfProductionStr)], direction = "long")
head(dfProductionStr)
row.names (dfProductionStr) = NULL
dfProductionStr <- dfProductionStr[,-(which(names(dfProductionStr)=="id"))]
dfProductionStr$Year <- gsub('Y',"",dfProductionStr$Year)

#### restructure data separate area harvested and production
dfProductionStr <- dcast(dfProductionStr, Area+Item+Year ~Element, value.var = "Value")
colnames(dfProductionStr)[which(colnames(dfProductionStr)=="Area harvested")] <-c("Area.Harvested") 

#### Setting NA for harvested data for which there is no production data and vice versa 
dfProductionStr$Area.Harvested[is.na(dfProductionStr$Production)] <- NA
dfProductionStr$Production[is.na(dfProductionStr$Area.Harvested)] <- NA
dfProductionStr <- dfProductionStr[!is.na(dfProductionStr$Area.Harvested) & !is.na(dfProductionStr$Production),]

#### remove zeroe areas and production
dfProductionStr <- dfProductionStr[-which(dfProductionStr$Area.Harvested==0 |  dfProductionStr$Production==0),]

#### change production to calories
## add calories
dfProductionStrCal <-dfProductionStr %>%left_join(dfCalories[,c("Item","kcal_100g")], by = 'Item')
dfProductionStrCal$Item = as.factor(dfProductionStrCal$Item)

## Removing crops with no caloires data
dfProductionStrCal <- dfProductionStrCal[!is.na(dfProductionStrCal$kcal_100g),]

#### Calculate production in kcal (original unit is tons)
dfProductionStrCal$production.Kcal <- dfProductionStrCal$Production* 1000 *dfProductionStrCal$kcal_100g * 10 
dfProductionStrCal$yield.Kcal <-dfProductionStrCal$production.Kcal / dfProductionStrCal$Area.Harvested

#### remove certain countries
# remove areas with no ISO3 (world regions)
dfProductionStrCal$ISO3 <- countrycode(dfProductionStrCal$Area, 'country.name', 'iso3c')
dfProductionStrCal[dfProductionStrCal$Area=="Eswatini","ISO3"]= "SWZ"
dfProductionStrCal<- droplevels(subset(dfProductionStrCal, !(dfProductionStrCal$ISO3 == "NA")))
unique(dfProductionStrCal$ISO3)

# countries with changing borders and the ones mentioned by Renard & Tilman 2019
dfProductionStrCal<- droplevels(subset(dfProductionStrCal, !(dfProductionStrCal$ISO3 %in% c("RUS", "SUN" ,"CSK", "CZE", "SVK", "SCG", "SRB", "MNE",
                                                "SSD", "SDN", "SFR", "PRK", "GIN,", "KEN","MOZ","ZMB","IRL","NLD", "NZL", "EGY","ETH"))))

# China : continental or country
summary(dfProductionStrCal[dfProductionStrCal$ISO3=="CHN",])
dfProductionStrCal<-droplevels(dfProductionStrCal[!dfProductionStrCal$Area=="China, mainland",])
length(unique(dfProductionStrCal$ISO3))#196 countries


#### Remove Spices, nes and onions dry and outliers in yield
dfProductionStrCal<-droplevels(dfProductionStrCal[!dfProductionStrCal$Item%in%c("Spices, nes","Onions, dry"),])
dfProductionStrCal<-droplevels(dfProductionStrCal[!dfProductionStrCal$yield.Kcal>1e+09,])

#### change character to factor
dfProductionStrCal$Year <- as.numeric(dfProductionStrCal$Year)
dfProductionStrCal$ISO3 <- as.factor(dfProductionStrCal$ISO3)
dfProductionStrCal$P <- as.factor(dfProductionStrCal$P)

##### Species by country and decade
dfCount<-dfProductionStrCal
dfCount$N<-NA
dfCount<-split(dfCount,list(droplevels(dfCount$ISO3), droplevels(dfCount$P), droplevels(dfCount$Item)), drop=T)

##### count number of year a species is cultivated 

for (i in 1:length(dfCount))
{
  print(i)
  dfCount[[i]]$N<-nrow(dfCount[[i]])
}

##### dataframe
dfProductionFinal<-ldply(dfCount)
dfProductionFinal<-dfProductionFinal[,-1]
summary(dfProductionFinal)

##### Remove species present during 10 years
dfProductionFinal<-droplevels(dfProductionFinal[dfProductionFinal$N==10,])
head(dfProductionFinal)

# #### add decades
# dfProductionStrCal$timePeriod=0
# dfProductionStrCal[dfProductionStrCal$Year%in%c(1968:1977),"timePeriod"] = 1968
# dfProductionStrCal[dfProductionStrCal$Year%in%c(1978:1987),"timePeriod"] = 1978
# dfProductionStrCal[dfProductionStrCal$Year%in%c(1988:1997),"timePeriod"] = 1988
# dfProductionStrCal[dfProductionStrCal$Year%in%c(1998:2007),"timePeriod"] = 1998
# dfProductionStrCal[dfProductionStrCal$Year%in%c(2008:2017),"timePeriod"] = 2008

######### Cropland data
dfCropland <- read.csv("datasets/cropland_global.csv")
dfCropland <- dfCropland[,c("Area","Year","Value")]
dfCropland$croplandArea <- dfCropland$Value*1000 # convert to ha
dfCropland <- dfCropland[,c("Area","Year","croplandArea")]

#### add iso3 codes and adapt some countries
dfCropland$ISO3 <- countrycode(dfCropland$Area, 'country.name', 'iso3c')
dfCropland[dfCropland$Area=="Eswatini","ISO3"] = "SWZ"
dfCropland<-droplevels(dfCropland[!dfCropland$Area=="China, mainland",])
dfCropland<-dfCropland[!is.na(dfCropland$ISO3),]
unique(dfCropland$ISO3)

######### Fertilizer
dfFertilizerArchive <- read.csv("datasets/fertilizerArchive_global.csv")
names(dfFertilizerArchive)[4] <- "Area"
dfFertilizerNew <- read.csv("datasets/fertilizerNew_global.csv")
dfFertilizer <- rbind(dfFertilizerArchive[,c("Area","Year","Item","Value")],dfFertilizerNew[,c("Area","Year","Item","Value")])
dfFertilizer <- dfFertilizer[which(dfFertilizer$Item=="Nitrogenous fertilizers"| dfFertilizer$Item=="Nutrient nitrogen N (total)"),]
dfFertilizer <- dfFertilizer[,c("Area","Year","Value")]
names(dfFertilizer)[3] <- "Nitrogen"
head(dfFertilizer)

#### add iso3 codes and adapt some countries
dfFertilizer$ISO3 <- countrycode(dfFertilizer$Area, 'country.name', 'iso3c')
dfFertilizer[dfFertilizer$Area=="Eswatini","ISO3"] = "SWZ"
dfFertilizer<-droplevels(dfFertilizer[!dfFertilizer$Area=="China, mainland",])
dfFertilizer<-dfFertilizer[!is.na(dfFertilizer$ISO3),]
unique(dfFertilizer$ISO3)

######### Irrigation
dfIrrigation <- read.csv("datasets/irrigationEquippedArea_global.csv")
dfIrrigation <- dfFertilizer[,c("Area","Year","Value")]
names(dfIrrigation)[3] <- "Irrigation"

#### add iso3 codes and adapt some countries
dfIrrigation$ISO3 <- countrycode(dfIrrigation$Area, 'country.name', 'iso3c')
dfIrrigation[dfIrrigation$Area=="Eswatini","ISO3"] = "SWZ"
dfIrrigation<-droplevels(dfIrrigation[!dfIrrigation$Area=="China, mainland",])
dfIrrigation<-dfIrrigation[!is.na(dfIrrigation$ISO3),]
unique(dfIrrigation$ISO3)


######### climate
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
dfClimateFinal <- dfClimateFinalPrint[,c(1,16:121)]

# merge
dfClimateFinalArea <- merge(dfClimateID,dfClimateFinal,by="cellID")
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
dfClimateFinalr <- reshape(dfClimateFinalAreaAgg[,1:length(dfClimateFinalAreaAgg)], varying = names(dfClimateFinalAreaAgg)[2:length(dfClimateFinalAreaAgg)], v.names = "Value", 
                         times = names(dfClimateFinalAreaAgg)[2:length(dfClimateFinalAreaAgg)], direction = "long")

row.names (dfClimateFinalr) = NULL
dfClimateFinalr <- dfClimateFinalr[,-(which(names(dfClimateFinalr)=="id"))]
dfClimateFinalr$Element <- substr(dfClimateFinalr$time,1,8)
dfClimateFinalr$Year <- as.numeric(substr(dfClimateFinalr$time,9,12))

#### restructure data separate mean Temp and mean prec
dfClimateFinals <- dcast(dfClimateFinalr, Area+Year ~Element, value.var = "Value")

#### add iso3 codes and adapt some countries
dfClimateFinals$ISO3 <- countrycode(dfClimateFinals$Area, 'country.name', 'iso3c')
dfClimateFinals<-dfClimateFinals[!is.na(dfClimateFinals$ISO3),]
unique(dfClimateFinals$ISO3)

#### calculate all variables for the 5 time periods
# vecCountryFinal <- Reduce(intersect,list(dfYieldCalories$Area,dfProductionCaloriesFinal$Area,dfProductionFullFinal$Area,dfShannon$Area,dfCropland$Area,dfFertilizer$Area,dfIrrigation$Area,dfClimateFinalr$Area,dfWarfare$Area))
vecISO <- Reduce(intersect,list(dfProductionStrCal$ISO3,dfCropland$ISO3,dfFertilizer$ISO3,dfIrrigation$ISO3,dfClimateFinals$ISO3))

# remove countries
dfProductionStrCal <- dfProductionStrCal[which(dfProductionStrCal$ISO3%in%vecISO),]
dfCropland <- dfCropland[which(dfCropland$ISO3%in%vecISO),]
dfFertilizer <- dfFertilizer[which(dfFertilizer$ISO3%in%vecISO),]
dfIrrigation <- dfIrrigation[which(dfIrrigation$ISO3%in%vecISO),]
dfClimateFinals <- dfClimateFinals[which(dfClimateFinals$ISO3%in%vecISO),]

# save(dfProductionFullFinal, file="datasetsDerived/dfProductionFullFinal_global.RData")
# save(dfYieldCalories, file="datasetsDerived/dfYieldCalories_global.RData")
# save(dfShannon, file="datasetsDerived/dfShannon_global.RData")
# save(dfCropland, file="datasetsDerived/dfCropland_global.RData")
# save(dfFertilizer, file="datasetsDerived/dfFertilizer_global.RData")
# save(dfIrrigation, file="datasetsDerived/dfIrrigation_global.RData")
# save(dfClimateFinalr, file="datasetsDerived/dfClimateFinalr_global.RData")

## summarize per time frame 
lsAll <- lapply(vecISO,function(ctry){
  # detrend yields
  show(as.character(ctry))
  lsAggregate <- lapply(c(1968,1978,1988,1998,2008),function(yearStart){
    N <- sum(dfProductionStrCal$ISO3==ctry&dfProductionStrCal$Year>=yearStart&dfProductionStrCal$Year<=(yearStart+9))
    if(sumCtry==10){
      # subset data for the target country
      dfProductionSumCtry <- dfYieldCalories[which(dfYieldCalories$Area==ctry&dfYieldCalories$Year>=yearStart&dfYieldCalories$Year<=(yearStart+9)),]
      dfProductionSumCtry$YieldDet <- resid(lm(Yield ~ Year^2,data=dfProductionSumCtry))
      dfShannonCtry <- dfShannon[which(dfShannon$Area==ctry&dfShannon$Year>=yearStart&dfShannon$Year<=(yearStart+9)),]
      dfCroplandCtry <- dfCropland[which(dfCropland$Area==ctry&dfCropland$Year>=yearStart&dfCropland$Year<=(yearStart+9)),]
      dfFertilizerCtry <- dfFertilizer[which(dfFertilizer$Area==ctry&dfFertilizer$Year>=yearStart&dfFertilizer$Year<=(yearStart+9)),]
      dfIrrigationCtry <- dfIrrigation[which(dfIrrigation$Area==ctry&dfIrrigation$Year>=yearStart&dfIrrigation$Year<=(yearStart+9)),]
      dfClimateCtry <- dfClimateFinalr[which(dfClimateFinalr$Area==ctry&dfClimateFinalr$Year>=yearStart&dfClimateFinalr$Year<=(yearStart+9)),]
    
      dfSummary <- data.frame(Area=ctry, timePeriod= yearStart)
      dfSummary$stability <- mean(dfProductionSumCtry$Yield,na.rm=T)/sd(dfProductionSumCtry$YieldDet,na.rm=T)
      dfSummary$yield <- mean(dfProductionSumCtry$Yield,na.rm=T)
      dfSummary$areaHarvested <- mean(dfProductionSumCtry$AreaHarvested,na.rm=T)
      dfSummary$diversity <- mean(dfShannonCtry$diversity,na.rm=T)
      dfSummary$meanCropland <- mean(dfCroplandCtry$croplandArea,na.rm=T)
      dfSummary$meanNitrogen <- mean(dfFertilizerCtry$Nitrogen,na.rm=T)
      dfSummary$meanIrrigation_share <- mean(dfIrrigationCtry$`Irrigation%`,na.rm=T)
      dfSummary$instabilityTemp <- -(mean(dfClimateCtry$meanTemp,na.rm=T)/sd(dfClimateCtry$meanTemp,na.rm=T))
      dfSummary$instabilityPrec <- -(mean(dfClimateCtry$meanPrec,na.rm=T)/sd(dfClimateCtry$meanPrec,na.rm=T))
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

## calculate nitrogen per ha
dfAll$meanNitrogen <- dfAll$meanNitrogen/dfAll$meanCropland
# check NA
sum(is.na(dfAll))
## omit NA
dfAll <- na.omit(dfAll)
length(unique(dfAll$Area)) ## 135 countries

## save dataframe
names(dfAll)
names(dfAll)[1] <- "Country"
dfAll <- dfAll[,c("Country","timePeriod",
                  "stability","yield","areaHarvested",
                  "diversity","meanNitrogen","meanIrrigation_share",
                  "instabilityTemp","instabilityPrec")]
write.csv(dfAll, "datasetsDerived/dataFinal_global.csv",row.names=F)



rm(list=ls())
