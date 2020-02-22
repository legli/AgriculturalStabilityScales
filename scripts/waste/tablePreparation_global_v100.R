library(rgdal)
library(maptools)
library(tidyr)
library(vegan)

################ DATA PREPARATION

#### cropland data
dfCropland <- read.csv("datasets/cropland_global.csv")
sort(as.character(unique(dfCropland$Area)))
head(dfCropland)

# harmonize cropland with spatial data 
ctryMap <- readOGR("spatial/countries_global.shp")
plot(ctryMap)
sort(as.character(setdiff(dfCropland$Area,ctryMap@data$Area)))

levels(dfCropland$Area) <- c(levels(dfCropland$Area),"Bolivia","Cote d'Ivoire","Czech Republic","Korea, Democratic People's Republic of",
                                                      "Swaziland","Ethiopia","Burma","Netherlands Antilles",
                                                      "Korea, Republic of","Reunion","Russia","Saint Helena","United States","Venezuela")

dfCropland[which(dfCropland$Area=="Bolivia (Plurinational State of)"),"Area"] <- "Bolivia"
dfCropland[which(dfCropland$Area=="CÃ´te d'Ivoire"|dfCropland$Area=="Côte d'Ivoire"|dfCropland$Area=="C?te d'Ivoire"),"Area"] <- "Cote d'Ivoire"
dfCropland[which(dfCropland$Area=="Czechia"),"Area"] <- "Czech Republic"
dfCropland[which(dfCropland$Area=="Democratic People's Republic of Korea"),"Area"] <- "Korea, Democratic People's Republic of"
dfCropland[which(dfCropland$Area=="Eswatini"),"Area"] <- "Swaziland"
dfCropland[which(dfCropland$Area=="Ethiopia PDR"),"Area"] <- "Ethiopia"
dfCropland[which(dfCropland$Area=="Myanmar"),"Area"] <- "Burma"
dfCropland[which(dfCropland$Area=="Netherlands Antilles (former)"),"Area"] <- "Netherlands Antilles"
dfCropland[which(dfCropland$Area=="Republic of Korea"),"Area"] <- "Korea, Republic of"
dfCropland[which(dfCropland$Area=="R?union"|dfCropland$Area=="Réunion"|dfCropland$Area=="RÃ©union"),"Area"] <- "Reunion"
dfCropland[which(dfCropland$Area=="Russian Federation"),"Area"] <- "Russia"
dfCropland[which(dfCropland$Area=="Saint Helena, Ascension and Tristan da Cunha"),"Area"] <- "Saint Helena"
dfCropland[which(dfCropland$Area=="United States of America"),"Area"] <- "United States"
dfCropland[which(dfCropland$Area=="Venezuela (Bolivarian Republic of)"),"Area"] <- "Venezuela"

sort(as.character(setdiff(dfCropland$Area,ctryMap@data$Area))) 

dfCropland <- dfCropland[which(dfCropland$Area%in%ctryMap@data$Area),] # only keep current countries
dfCropland <- dfCropland[,c("Area","Year","Value")]
dfCropland$croplandArea <- dfCropland$Value*1000 # convert to ha
dfCropland <- dfCropland[,c("Area","Year","croplandArea")]
nrow(unique(dfCropland[,c("Area","Year")])) == nrow(dfCropland) # check duplicates


# only keep highest 99.9%
sum(is.na(dfCropland))
dfCroplandMean <- aggregate(croplandArea~Area,dfCropland,mean)
head(dfCroplandMean)
dfCroplandMean$propArea <- dfCroplandMean$croplandArea/sum(dfCroplandMean$croplandArea)
dfCroplandMean <- dfCroplandMean[order(dfCroplandMean$propArea,decreasing = T),]
dfCroplandMean$cumArea <- cumsum(dfCroplandMean$propArea)
ind <- which(dfCroplandMean$cumArea>=0.999)[1]

dfCropland <- dfCropland[which(dfCropland$Area%in%dfCroplandMean[1:ind,"Area"]),]
## this are the target regions



#### agricultural production data

## read agricultural production file
dfProductionFull <- read.csv("datasets/agriculturalProduction_global.csv")
names(dfProductionFull)

## production data
names(dfProductionFull)
dfProductionFull <- dfProductionFull[which(dfProductionFull$Element=="Area harvested" | dfProductionFull$Element == "Production"),c(2,4,6,15:64)]
head(dfProductionFull)
names(dfProductionFull)[4:53] <- 1968:2017 # change colnames to years

## change dataset structure
dfProductionFullr <- dfProductionFull %>% gather(Year, Value, "1968":"2017")
head(dfProductionFullr)
dfProductionFullr$Year <- as.numeric(dfProductionFullr$Year)
dfProductionFullr <- dfProductionFullr %>% spread(Element, Value)
head(dfProductionFullr)
names(dfProductionFullr)[4] <- "AreaHarvested"


#### add calories and make crops consistent with target crop file
dfConsistency <- read.csv("datasets/targetCrops_global.csv")
head(dfConsistency) # Group2 is the target crop name
names(dfConsistency)[4] <- "Crop"
dfConsistency <- dfConsistency[which(!is.na(dfConsistency$Crop)&!is.na(dfConsistency$Calories)),]
setdiff(sort(unique(dfConsistency[which(!is.na(dfConsistency$harvestedAreaFAO)),"harvestedAreaFAO"])),sort(unique(dfProductionFull$Item)))

## subset crops without calories
dfProductionFullr <- merge(dfProductionFullr,dfConsistency[,c("harvestedAreaFAO","Crop","Calories")],by.x="Item",by.y="harvestedAreaFAO")
names(dfProductionFullr)
# change production to calories
dfProductionFullr$Production <- dfProductionFullr$Production*dfProductionFullr$Calories
# keep necessary columns only 
dfProductionFullr <- dfProductionFullr[,c("Area","Crop","Year","AreaHarvested","Production")]


## make area and production consistent
# set 0 areas to NA, set produciton in NA areas to NA
dfProductionFullr[which(dfProductionFullr$AreaHarvested==0),"AreaHarvested"] <- NA
dfProductionFullr[which(is.na(dfProductionFullr$AreaHarvested)),"Production"] <- NA
# set production to 0 where area is reported
dfProductionFullr[which(dfProductionFullr$AreaHarvested> 0 & is.na(dfProductionFullr$Production)),"Production"] <- 0

#### Setting NA for harvested data for which there is no production data and vice versa 
dfProductionStr$Area.Harvested[is.na(dfProductionStr$Production)] <- NA
dfProductionStr$Production[is.na(dfProductionStr$Area.Harvested)] <- NA
dfProductionStr <- dfProductionStr[!is.na(dfProductionStr$Area.Harvested) & !is.na(dfProductionStr$Production),]

#### remove zeroe areas and production
dfProductionStr <- dfProductionStr[-which(dfProductionStr$Area.Harvested==0 |  dfProductionStr$Production==0),]



# subset NA
dfProductionFullr <- na.omit(dfProductionFullr)

## harmonize country names
sort(as.character(setdiff(dfProductionFullr$Area,dfCropland$Area)))

levels(dfProductionFullr$Area) <- c(levels(dfProductionFullr$Area),"Bolivia","Cote d'Ivoire","Czech Republic","Korea, Democratic People's Republic of",
                             "Swaziland","Ethiopia","Burma",
                             "Korea, Republic of","Reunion","Russia","North Macedonia","United States","Venezuela")

dfProductionFullr[which(dfProductionFullr$Area=="Bolivia (Plurinational State of)"),"Area"] <- "Bolivia"
dfProductionFullr[which(dfProductionFullr$Area=="CÃ´te d'Ivoire"|dfProductionFullr$Area=="C?te d'Ivoire"|dfProductionFullr$Area=="Côte d'Ivoire"),"Area"] <- "Cote d'Ivoire"
dfProductionFullr[which(dfProductionFullr$Area=="Czechia"),"Area"] <- "Czech Republic"
dfProductionFullr[which(dfProductionFullr$Area=="Democratic People's Republic of Korea"),"Area"] <- "Korea, Democratic People's Republic of"
dfProductionFullr[which(dfProductionFullr$Area=="Eswatini"),"Area"] <- "Swaziland"
dfProductionFullr[which(dfProductionFullr$Area=="Ethiopia PDR"),"Area"] <- "Ethiopia"
dfProductionFullr[which(dfProductionFullr$Area=="Myanmar"),"Area"] <- "Burma"
dfProductionFullr[which(dfProductionFullr$Area=="Republic of Korea"),"Area"] <- "Korea, Republic of"
dfProductionFullr[which(dfProductionFullr$Area=="R?union"|dfProductionFullr$Area=="Réunion"|dfProductionFullr$Area=="RÃ©union"),"Area"] <- "Reunion"
dfProductionFullr[which(dfProductionFullr$Area=="Russian Federation"),"Area"] <- "Russia"
dfProductionFullr[which(dfProductionFullr$Area=="The former Yugoslav Republic of Macedonia"),"Area"] <- "North Macedonia"
dfProductionFullr[which(dfProductionFullr$Area=="United States of America"),"Area"] <- "United States"
dfProductionFullr[which(dfProductionFullr$Area=="Venezuela (Bolivarian Republic of)"),"Area"] <- "Venezuela"
sort(as.character(setdiff(dfProductionFullr$Area,dfCropland$Area)))

nrow(unique(dfProductionFullr[,c("Area","Year","Crop")])) == nrow(dfProductionFullr) # check duplicates
unique(dfProductionFullr[which(duplicated(dfProductionFullr[,c("Area","Year","Crop")])),"Area"])


## reduce dataset: only include crops with at least 15 entries per region, remove 0 at the beginning and end of time series!; maximize  number of crops and years per time window
# iterate through countries
vecCountry <- unique(dfProductionFullr$Area)
lsProductionFull <- lapply(vecCountry,function(ctry){
  show(as.character(ctry))
  ## subset country
  dfCountry <- dfProductionFullr[which(dfProductionFullr$Area==ctry),]
  dfCountrySum <- aggregate(Production~Year,dfCountry,sum)
  minYear <- min(dfCountrySum[which(dfCountrySum$Production>0),"Year"])
  maxYear <- max(dfCountrySum[which(dfCountrySum$Production>0),"Year"])
  dfFinal <- dfCountry[which(dfCountry$Year%in%minYear:maxYear),]

  # only keep entries with at least 15 years in total
  if (length(unique(dfFinal$Year))>=15)
    {dfFinal}
}) ## infinite if time period is not covered -> will be ignored
dfProductionFullFinal <- do.call(rbind,lsProductionFull)
head(dfProductionFullFinal)
sum(is.na(dfProductionFullFinal$AreaHarvested))
sum(is.na(dfProductionFullFinal$Production))

# check number of unique crops
length(unique(dfProductionFullFinal$Crop)) ## 131 crops

# #### calculate yields
sum(is.na(dfProductionFullFinal))
dfYieldCalories <- aggregate(cbind(Production,AreaHarvested)~Area+Year,dfProductionFullFinal,sum)
head(dfYieldCalories)
dfYieldCalories$Yield <- dfYieldCalories$Production/dfYieldCalories$AreaHarvested
dfYieldCalories <- dfYieldCalories[,c("Area","Year","Production","AreaHarvested","Yield")]
nrow(unique(dfYieldCalories[,c("Area","Year")])) == nrow(dfYieldCalories) # check duplicates

#### calculate effective diversity (exp of shannon)
dfShannon <- aggregate(AreaHarvested~Area+Year,dfProductionFullFinal,function(x){exp(diversity(x,index="shannon"))})
head(dfShannon)
names(dfShannon)[3] <- "diversity"
nrow(dfShannon)==nrow(dfYieldCalories)
nrow(unique(dfShannon[,c("Area","Year")])) == nrow(dfShannon) # check duplicates


################### Other Predictors

#### Fertilizer
dfFertilizerArchive <- read.csv("datasets/fertilizerArchive_global.csv")
names(dfFertilizerArchive)[4] <- "Area"
dfFertilizerNew <- read.csv("datasets/fertilizerNew_global.csv")
dfFertilizer <- rbind(dfFertilizerArchive[,c("Area","Year","Item","Value")],dfFertilizerNew[,c("Area","Year","Item","Value")])
dfFertilizer <- dfFertilizer[which(dfFertilizer$Item=="Nitrogenous fertilizers"| dfFertilizer$Item=="Nutrient nitrogen N (total)"),]
dfFertilizer <- dfFertilizer[,c("Area","Year","Value")]
names(dfFertilizer)[3] <- "Nitrogen"
head(dfFertilizer)


# harmonzie country names
sort(as.character(setdiff(dfFertilizer$Area,dfCropland$Area)))

levels(dfFertilizer$Area) <- c(levels(dfFertilizer$Area),"Bolivia","Cote d'Ivoire","Czech Republic","Korea, Democratic People's Republic of",
                             "Swaziland","Ethiopia","Burma","Netherlands Antilles",
                             "Reunion","Korea, Republic of","Russia","North Macedonia","United States","Venezuela")

dfFertilizer[which(dfFertilizer$Area=="Bolivia (Plurinational State of)"),"Area"] <- "Bolivia"
dfFertilizer[which(dfFertilizer$Area=="CÃ´te d'Ivoire"|dfFertilizer$Area=="C?te d'Ivoire"|dfFertilizer$Area=="Côte d'Ivoire"),"Area"] <- "Cote d'Ivoire"
dfFertilizer[which(dfFertilizer$Area=="Czechia"),"Area"] <- "Czech Republic"
dfFertilizer[which(dfFertilizer$Area=="Democratic People's Republic of Korea"),"Area"] <- "Korea, Democratic People's Republic of"
dfFertilizer[which(dfFertilizer$Area=="Eswatini"),"Area"] <- "Swaziland"
dfFertilizer[which(dfFertilizer$Area=="Ethiopia PDR"),"Area"] <- "Ethiopia"
dfFertilizer[which(dfFertilizer$Area=="Myanmar"),"Area"] <- "Burma"
dfFertilizer[which(dfFertilizer$Area=="Netherlands Antilles (former)"),"Area"] <- "Netherlands Antilles"
dfFertilizer[which(dfFertilizer$Area=="R?union"|dfFertilizer$Area=="Réunion"|dfFertilizer$Area=="RÃ©union"),"Area"] <- "Reunion"
dfFertilizer[which(dfFertilizer$Area=="Republic of Korea"),"Area"] <- "Korea, Republic of"
dfFertilizer[which(dfFertilizer$Area=="Russian Federation"),"Area"] <- "Russia"
dfFertilizer[which(dfFertilizer$Area=="The former Yugoslav Republic of Macedonia"),"Area"] <- "North Macedonia"
dfFertilizer[which(dfFertilizer$Area=="United States of America"),"Area"] <- "United States"
dfFertilizer[which(dfFertilizer$Area=="Venezuela (Bolivarian Republic of)"),"Area"] <- "Venezuela"

sort(as.character(setdiff(dfFertilizer$Area,dfCropland$Area)))

nrow(unique(dfFertilizer[,c("Area","Year")])) == nrow(dfFertilizer) # check duplicates


## Irrigation
dfIrrigation <- read.csv("datasets/irrigationEquippedArea_global.csv")

# harmonzie country names
sort(as.character(setdiff(dfIrrigation$Area,dfCropland$Area)))
levels(dfIrrigation$Area) <- c(levels(dfIrrigation$Area),"Bolivia","Cote d'Ivoire","Czech Republic","Korea, Democratic People's Republic of",
                               "Swaziland","Ethiopia","Burma","Reunion","Korea, Republic of","Russia","United States","Venezuela")

dfIrrigation[which(dfIrrigation$Area=="Bolivia (Plurinational State of)"),"Area"] <- "Bolivia"
dfIrrigation[which(dfIrrigation$Area=="CÃ´te d'Ivoire"|dfIrrigation$Area=="C?te d'Ivoire"|dfIrrigation$Area=="Côte d'Ivoire"),"Area"] <- "Cote d'Ivoire"
dfIrrigation[which(dfIrrigation$Area=="Czechia"),"Area"] <- "Czech Republic"
dfIrrigation[which(dfIrrigation$Area=="Democratic People's Republic of Korea"),"Area"] <- "Korea, Democratic People's Republic of"
dfIrrigation[which(dfIrrigation$Area=="Eswatini"),"Area"] <- "Swaziland"
dfIrrigation[which(dfIrrigation$Area=="Ethiopia PDR"),"Area"] <- "Ethiopia"
dfIrrigation[which(dfIrrigation$Area=="Myanmar"),"Area"] <- "Burma"
dfIrrigation[which(dfIrrigation$Area=="R?union"|dfIrrigation$Area=="Réunion"|dfIrrigation$Area=="RÃ©union"),"Area"] <- "Reunion"
dfIrrigation[which(dfIrrigation$Area=="Republic of Korea"),"Area"] <- "Korea, Republic of"
dfIrrigation[which(dfIrrigation$Area=="Russian Federation"),"Area"] <- "Russia"
dfIrrigation[which(dfIrrigation$Area=="United States of America"),"Area"] <- "United States"
dfIrrigation[which(dfIrrigation$Area=="Venezuela (Bolivarian Republic of)"),"Area"] <- "Venezuela"
sort(as.character(setdiff(dfIrrigation$Area,dfCropland$Area)))

dfIrrigation <- dfIrrigation[,c("Area","Year","Value")]
names(dfIrrigation)[3] <- "Irrigation%"
nrow(unique(dfIrrigation[,c("Area","Year")])) == nrow(dfIrrigation) # check duplicates


###### climate
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
dfClimateFinalr <- dfClimateFinalAreaAgg %>% gather(climYear, Value, names(dfClimateFinalAreaAgg)[2:101])
head(dfClimateFinalr)
dfClimateFinalr$Year <- as.numeric(substr(dfClimateFinalr$climYear,9,12))
dfClimateFinalr$Element <- substr(dfClimateFinalr$climYear,1,8)
dfClimateFinalr <- dfClimateFinalr[,c("Area","Year","Element","Value")]
dfClimateFinalr <- dfClimateFinalr %>% spread(Element, Value)
head(dfClimateFinalr)
nrow(dfClimateFinalr)
nrow(unique(dfClimateFinalr[,c("Area","Year")])) == nrow(dfClimateFinalr) # check duplicates



#### calculate all variables for the 5 time periods

# vecCountryFinal <- Reduce(intersect,list(dfYieldCalories$Area,dfProductionCaloriesFinal$Area,dfProductionFullFinal$Area,dfShannon$Area,dfCropland$Area,dfFertilizer$Area,dfIrrigation$Area,dfClimateFinalr$Area,dfWarfare$Area))
vecCountryFinal <- Reduce(intersect,list(dfYieldCalories$Area,dfProductionFullFinal$Area,dfShannon$Area,dfCropland$Area,dfFertilizer$Area,dfIrrigation$Area,dfClimateFinalr$Area))

# remove countries listed by Renard & Tilman 2019
vecCountryFinal <- vecCountryFinal[-which(vecCountryFinal%in%c("Egypt","Korea, Democratic People's Republic of", "Guinea", "Kenya","Mozambique",
                                               "Zambia","Ireland","New Zealand","Netherlands"))] # note that Ireland was not included anyway


# remove countries
dfProductionFullFinal <- dfProductionFullFinal[which(dfProductionFullFinal$Area%in%vecCountryFinal),]
dfYieldCalories <- dfYieldCalories[which(dfYieldCalories$Area%in%vecCountryFinal),]
dfShannon <- dfShannon[which(dfShannon$Area%in%vecCountryFinal),]
dfCropland <- dfCropland[which(dfCropland$Area%in%vecCountryFinal),]
dfFertilizer <- dfFertilizer[which(dfFertilizer$Area%in%vecCountryFinal),]
dfIrrigation <- dfIrrigation[which(dfIrrigation$Area%in%vecCountryFinal),]
dfClimateFinalr <- dfClimateFinalr[which(dfClimateFinalr$Area%in%vecCountryFinal),]

save(dfProductionFullFinal, file="datasetsDerived/dfProductionFullFinal_global.RData")
save(dfYieldCalories, file="datasetsDerived/dfYieldCalories_global.RData")
save(dfShannon, file="datasetsDerived/dfShannon_global.RData")
save(dfCropland, file="datasetsDerived/dfCropland_global.RData")
save(dfFertilizer, file="datasetsDerived/dfFertilizer_global.RData")
save(dfIrrigation, file="datasetsDerived/dfIrrigation_global.RData")
save(dfClimateFinalr, file="datasetsDerived/dfClimateFinalr_global.RData")

## summarize per time frame 
lsAll <- lapply(vecCountryFinal,function(ctry){
  # detrend yields
  show(as.character(ctry))
  lsAggregate <- lapply(c(1968,1978,1988,1998,2008),function(yearStart){
    
    sumCtry <- sum(dfYieldCalories$Area==ctry&dfYieldCalories$Year>=yearStart&dfYieldCalories$Year<=(yearStart+9))
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
