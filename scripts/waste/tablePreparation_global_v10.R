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
## this are the target regions



#### agricultural production data

## read agricultural production file
dfProductionFull <- read.csv("datasets/agriculturalProduction_global.csv")
names(dfProductionFull)

## production data
names(dfProductionFull)
dfProductionFull <- dfProductionFull[which(dfProductionFull$Element=="Area harvested" | dfProductionFull$Element == "Production"),c(2,4,6,8:63)]
head(dfProductionFull)
names(dfProductionFull)[4:59] <- 1961:2016 # change colnames to years

## change dataset structure
dfProductionFullr <- dfProductionFull %>% gather(Year, Value, "1961":"2016")
head(dfProductionFullr)
dfProductionFullr$Year <- as.numeric(dfProductionFullr$Year)
dfProductionFullr <- dfProductionFullr %>% spread(Element, Value)
head(dfProductionFullr)
names(dfProductionFullr)[4] <- "AreaHarvested"


#### add calories and make crops consistent with target crop file
dfConsistency <- read.csv("datasets/targetCrops_global.csv")
head(dfConsistency) # Group2 is the target crop name

dfConsistency <- dfConsistency[which(!is.na(dfConsistency$Group2)&!is.na(dfConsistency$Calories)),]
setdiff(sort(unique(dfConsistency[which(!is.na(dfConsistency$harvestedAreaFAO)),"harvestedAreaFAO"])),sort(unique(dfProductionFull$Item)))

## subset crops without calories
dfProductionFullr <- merge(dfProductionFullr,dfConsistency[,c("harvestedAreaFAO","Group2","Calories")],by.x="Item",by.y="harvestedAreaFAO")
names(dfProductionFullr)
# change production to calories
dfProductionFullr$Production <- dfProductionFullr$Production*dfProductionFullr$Calories
# keep necessary columns only 
dfProductionFullr <- dfProductionFullr[,c("Area","Group2","Year","AreaHarvested","Production")]


## make area and production consistent
# set 0 areas to NA, set produciton in NA areas to NA
dfProductionFullr[which(dfProductionFullr$AreaHarvested==0),"AreaHarvested"] <- NA
dfProductionFullr[which(is.na(dfProductionFullr$AreaHarvested)),"Production"] <- NA
# set production to 0 where area is reported
dfProductionFullr[which(dfProductionFullr$AreaHarvested> 0 & is.na(dfProductionFullr$Production)),"Production"] <- 0


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

nrow(unique(dfProductionFullr[,c("Area","Year","Group2")])) == nrow(dfProductionFullr) # check duplicates
unique(dfProductionFullr[which(duplicated(dfProductionFullr[,c("Area","Year","Group2")])),"Area"])



## add regions
dfRegions <- read.csv("datasets/regions_global.csv")
names(dfRegions)
unique(dfRegions$Region)

# harmonize country names
missing <- unique(dfProductionFullr[-which(dfProductionFullr$Area%in%dfRegions$Area),"Area"])
levels(dfRegions$Area) <- c(levels(dfRegions$Area),as.character(missing))
sort(as.character(dfRegions$Area))
dfRegions[which(dfRegions$Area=="Bolivia (Plurinational State of)"),"Area"] <- "Bolivia"
dfRegions[which(dfRegions$Area=="Myanmar"),"Area"] <- "Burma"
# dfRegions[which(dfRegions$Area=="China, mainland"),"Area"] <- "China"
dfRegions[which(dfRegions$Area=="Côte d'Ivoire"|dfRegions$Area=="CÃ´te d'Ivoire"|dfRegions$Area=="C?te d'Ivoire"),"Area"] <- "Cote d'Ivoire"
dfRegions[which(dfRegions$Area=="Czechia"),"Area"] <- "Czech Republic"
dfRegions[which(dfRegions$Area=="Democratic People's Republic of Korea"),"Area"] <- "Korea, Democratic People's Republic of"
dfRegions[which(dfRegions$Area=="Republic of Korea"),"Area"] <- "Korea, Republic of"
dfRegions[which(dfRegions$Area=="Réunion"|dfRegions$Area=="RÃ©union"),"Area"] <- "Reunion"
dfRegions[which(dfRegions$Area=="Russian Federation"),"Area"] <- "Russia"
dfRegions[which(dfRegions$Area=="Eswatini"),"Area"] <- "Swaziland"
dfRegions[which(dfRegions$Area=="United States of America"),"Area"] <- "United States"
dfRegions[which(dfRegions$Area=="Venezuela (Bolivarian Republic of)"),"Area"] <- "Venezuela"
unique(dfProductionFullr[-which(dfProductionFullr$Area%in%dfRegions$Area),"Area"])
dfProductionFullr <- merge(dfProductionFullr,dfRegions, by="Area")
names(dfProductionFullr)



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
  
  # only keep entries with at least 10 years in total
  if (length(unique(dfFinal$Year))>=15)
    {dfFinal}
}) ## infinite if time period is not covered -> will be ignored
dfProductionFull_Final <- do.call(rbind,lsProductionFull)
head(dfProductionFull_Final)
sum(is.na(dfProductionFull_Final$AreaHarvested))
sum(is.na(dfProductionFull_Final$Production))

# check number of unique crops
length(unique(dfProductionFull_Final$Group2)) ## 131 crops

#### calculate total production per country and year
# dfProductionCaloriesFinal <- aggregate(Production~Area+Region+Year,dfProductionFull_Final,sum)
# head(dfProductionCaloriesFinal)
# nrow(dfProductionCaloriesFinal)==nrow(unique(dfProductionFull_Final[,c("Area","Year")]))
# 
# #### calculate total area per country and year
# dfHarvestedAreaFinal <- aggregate(AreaHarvested~Area+Year,dfProductionFull_Final,sum)
# head(dfHarvestedAreaFinal)
# nrow(dfHarvestedAreaFinal)==nrow(dfProductionCaloriesFinal)
# 
# #### calculate yields
# dfYieldCalories <- merge(dfProductionCaloriesFinal,dfHarvestedAreaFinal,by=c("Area","Year"))
sum(is.na(dfProductionFull_Final))
dfYieldCalories <- aggregate(cbind(Production,AreaHarvested)~Area+Year,dfProductionFull_Final,sum)
head(dfYieldCalories)
dfYieldCalories$Yield <- dfYieldCalories$Production/dfYieldCalories$AreaHarvested
dfYieldCalories <- dfYieldCalories[,c("Area","Year","Production","AreaHarvested","Yield")]
nrow(unique(dfYieldCalories[,c("Area","Year")])) == nrow(dfYieldCalories) # check duplicates

#### calculate effective diversity (exp of shannon)
dfShannon <- aggregate(AreaHarvested~Area+Year,dfProductionFull_Final,function(x){exp(diversity(x,index="shannon"))})
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


###### Warfare
# dfWarfare <- read.csv("datasets/warfare_global.csv")
# names(dfWarfare)[3] <- "Area"
# 
# dfWarfare <- na.omit(dfWarfare) # remove NA
# # add former divided Germany: always 0
# dfWarfare[which(dfWarfare$Area=="Germany")[1],]
# dfG <- data.frame(SCODE="GMY",CCODE = 255, Area="Germany",YEAR=1961:1990,ACTOTAL=0)
# dfWarfare <- rbind(dfWarfare,dfG)
# 
# # combine north and south yemen
# dfWarfare[which(dfWarfare$Area=="Yemen")[1],]
# dfYemenN <- dfWarfare[which(dfWarfare$Area=="Yemen, North"),]
# dfYemenN$SCODE <- "YEM"
# dfYemenN$CCODE <- 679
# dfYemenN$Area <- "Yemen"
# dfYemenS <- dfWarfare[which(dfWarfare$Area=="Yemen, South"),]
# dfYemenS$SCODE <- "YEM"
# dfYemenS$CCODE <- 679
# dfYemenS$Area <- "Yemen"
# dfYemenT <- merge(dfYemenN,dfYemenS,by=c("SCODE","CCODE","Area","YEAR"),all=T)
# head(dfYemenT)
# dfYemenT$ACTOTAL <- rowSums(dfYemenT[,c("ACTOTAL.x","ACTOTAL.y")],na.rm=T)
# dfWarfare <- rbind(dfWarfare,dfYemenT[,c(1:4,7)])
# 
# # harmonize country names
# sort(as.character(setdiff(dfWarfare$Area,dfCropland$Area)))
# 
# levels(dfWarfare$Area) <- c(levels(dfWarfare$Area),"Iran (Islamic Republic of)","Korea, Democratic People's Republic of","Korea, Republic of",
#                             "Lao People's Democratic Republic","North Macedonia","Republic of Moldova","Burma","Russia","Syrian Arab Republic",
#                             "United Republic of Tanzania","Viet Nam")
# 
# dfWarfare[which(dfWarfare$Area=="Iran"),"Area"] <-  "Iran (Islamic Republic of)"
# dfWarfare[which(dfWarfare$Area=="Korea North"),"Area"] <- "Korea, Democratic People's Republic of"
# dfWarfare[which(dfWarfare$Area=="Korea South"),"Area"] <- "Korea, Republic of"
# dfWarfare[which(dfWarfare$Area=="Laos"),"Area"] <-  "Lao People's Democratic Republic"
# dfWarfare[which(dfWarfare$Area=="Macedonia"),"Area"] <-  "North Macedonia"
# dfWarfare[which(dfWarfare$Area=="Moldova"),"Area"] <- "Republic of Moldova"
# dfWarfare[which(dfWarfare$Area=="Myanmar (Burma)"),"Area"] <-  "Burma"
# dfWarfare[which(dfWarfare$Area=="RUSRia"),"Area"] <- "Russia"
# dfWarfare[which(dfWarfare$Area=="Syria"),"Area"] <- "Syrian Arab Republic"
# dfWarfare[which(dfWarfare$Area=="Tanzania"),"Area"] <-"United Republic of Tanzania"
# dfWarfare[which(dfWarfare$Area=="Vietnam"),"Area"] <- "Viet Nam"
# sort(as.character(setdiff(dfWarfare$Area,dfCropland$Area)))
# names(dfWarfare)[4:5] <- c("Year","warfare")
# dfWarfare <- dfWarfare[,c("Area","Year","warfare")]
# hist(dfWarfare$warfare)


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
dfClimateFinal <- dfClimateFinalPrint

# merge
dfClimateFinalArea <- merge(dfClimateID,dfClimateFinal,by="cellID")
head(dfClimateFinalArea)
length(unique(dfClimateFinalArea$cellID)) / nrow(dfClimateFinalArea)
names(dfClimateFinalArea)
# get cropland by region
dfCroplandTot <- aggregate(cbind(cropland1960AD,cropland1970AD,cropland1980AD,cropland1990AD,cropland2000AD,cropland2010AD)~Area,dfClimateFinalArea,sum)
head(dfCroplandTot)
names(dfCroplandTot)[2:7] <- paste0(names(dfCroplandTot)[2:7],"Tot")
dfClimateFinalArea <- merge(dfClimateFinalArea,dfCroplandTot,by="Area")
head(dfClimateFinalArea)
names(dfClimateFinalArea)
## weighted average: get area of cropland attributed to a cell segment (i.e. cropland area multiplied by area share of the segment), divide it by total cropland area across region
dfClimateFinalArea$weight1 <- (((dfClimateFinalArea$areaHA/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland1960AD)/dfClimateFinalArea$cropland1960ADTot)
dfClimateFinalArea$weight2 <- (((dfClimateFinalArea$areaHA/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland1970AD)/dfClimateFinalArea$cropland1970ADTot)
dfClimateFinalArea$weight3 <- (((dfClimateFinalArea$areaHA/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland1980AD)/dfClimateFinalArea$cropland1980ADTot)
dfClimateFinalArea$weight4 <- (((dfClimateFinalArea$areaHA/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland1990AD)/dfClimateFinalArea$cropland1990ADTot)
dfClimateFinalArea$weight5 <- (((dfClimateFinalArea$areaHA/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland1990AD)/dfClimateFinalArea$cropland1990ADTot)
dfClimateFinalArea$weight6 <- (((dfClimateFinalArea$areaHA/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland2000AD)/dfClimateFinalArea$cropland2000ADTot)
dfClimateFinalArea$weight7 <- (((dfClimateFinalArea$areaHA/dfClimateFinalArea$areaTot)*dfClimateFinalArea$cropland2010AD)/dfClimateFinalArea$cropland2010ADTot)
hist(dfClimateFinalArea$weight5)

## multiply weight by climate values
names(dfClimateFinalArea)
dfClimateFinalArea[,7:22] <- dfClimateFinalArea[,7:22]*dfClimateFinalArea$weight1
dfClimateFinalArea[,23:38] <- dfClimateFinalArea[,23:38]*dfClimateFinalArea$weight2
dfClimateFinalArea[,39:54] <- dfClimateFinalArea[,39:54]*dfClimateFinalArea$weight3
dfClimateFinalArea[,55:70] <- dfClimateFinalArea[,55:70]*dfClimateFinalArea$weight4
dfClimateFinalArea[,71:86] <- dfClimateFinalArea[,71:86]*dfClimateFinalArea$weight5
dfClimateFinalArea[,87:102] <- dfClimateFinalArea[,87:102]*dfClimateFinalArea$weight6
dfClimateFinalArea[,103:118] <- dfClimateFinalArea[,103:118]*dfClimateFinalArea$weight7

# sum weighted values to get overall weighted average
names(dfClimateFinalArea)
dfClimateFinalAreaAgg <- aggregate(dfClimateFinalArea[,7:118],by=list(dfClimateFinalArea$Area),FUN=function(i){sum(i,na.rm=T)})
head(dfClimateFinalAreaAgg)
names(dfClimateFinalAreaAgg)[1] <- "Area"
min(dfClimateFinalAreaAgg[2:113]) # check for negative values -> would be problematic for instability calculation
sum(dfClimateFinalAreaAgg[2:113]<0) 
# dfClimateFinalAreaAgg$neg <- apply(dfClimateFinalAreaAgg[,2:113],1,function(r){sum(r<0)})
# dfClimateFinalAreaAgg <- dfClimateFinalAreaAgg[which(dfClimateFinalAreaAgg$neg==0),1:113]


# change structure
names(dfClimateFinalAreaAgg)
dfClimateFinalr <- dfClimateFinalAreaAgg %>% gather(climYear, Value, names(dfClimateFinalAreaAgg)[2:113])
head(dfClimateFinalr)
dfClimateFinalr$Year <- as.numeric(substr(dfClimateFinalr$climYear,9,12))
dfClimateFinalr$Element <- substr(dfClimateFinalr$climYear,1,8)
dfClimateFinalr <- dfClimateFinalr[,c("Area","Year","Element","Value")]
dfClimateFinalr <- dfClimateFinalr %>% spread(Element, Value)
head(dfClimateFinalr)
nrow(dfClimateFinalr)
nrow(unique(dfClimateFinalr[,c("Area","Year")])) == nrow(dfClimateFinalr) # check duplicates



#### calculate all variables for the 5 time periods

# vecCountryFinal <- Reduce(intersect,list(dfYieldCalories$Area,dfProductionCaloriesFinal$Area,dfProductionFull_Final$Area,dfShannon$Area,dfCropland$Area,dfFertilizer$Area,dfIrrigation$Area,dfClimateFinalr$Area,dfWarfare$Area))
vecCountryFinal <- Reduce(intersect,list(dfYieldCalories$Area,dfShannon$Area,dfCropland$Area,dfFertilizer$Area,dfIrrigation$Area,dfClimateFinalr$Area))

# remove countries listed by Renard & Tilman 2019
vecCountryFinal <- vecCountryFinal[-which(vecCountryFinal%in%c("Egypt","Korea, Democratic People's Republic of", "Guinea", "Kenya","Mozambique",
                                               "Zambia","Ireland","New Zealand","Netherlands"))] # note that Ireland was not included anyway

# calculate global detrended production & Yield
head(dfYieldCalories)
sum(is.na(dfYieldCalories))
dfProductionWorld <- aggregate(cbind(Production,AreaHarvested)~Year,dfYieldCalories[which(dfYieldCalories$Area%in%vecCountryFinal),],sum)
dfProductionWorld$ProductionDet <- resid(loess(Production ~ Year,data=dfProductionWorld))
dfProductionWorld$Yield <- dfProductionWorld$Production/dfProductionWorld$AreaHarvested
dfProductionWorld$YieldDet <- resid(loess(Yield ~ Year,data=dfProductionWorld))

## summarize per time frame 
lsAll <- lapply(vecCountryFinal,function(ctry){
  # detrend yields
  show(as.character(ctry))

  # subset data for the target country
  dfProductionSumCtry <- dfYieldCalories[which(dfYieldCalories$Area==ctry),]
  dfProductionSumCtry$ProductionDet <- resid(loess(Production ~ Year,data=dfProductionSumCtry))
  dfProductionSumCtry$YieldDet <- resid(loess(Yield ~ Year,data=dfProductionSumCtry))
  # dfYieldCtry <- dfYieldCalories[which(dfYieldCalories$Area==ctry),c("Area","Year","Yield")]
  dfShannonCtry <- dfShannon[which(dfShannon$Area==ctry),]
  dfCroplandCtry <- dfCropland[which(dfCropland$Area==ctry),]
  dfFertilizerCtry <- dfFertilizer[which(dfFertilizer$Area==ctry),]
  dfIrrigationCtry <- dfIrrigation[which(dfIrrigation$Area==ctry),]
  dfClimateCtry <- dfClimateFinalr[which(dfClimateFinalr$Area==ctry),]

  lsAggregate <- lapply(c(1961,1969,1977,1985,1993,2001,2009),function(yearStart){
    # print(yearStart)
    # cv production
    dfSummary <- data.frame(Area=ctry, timePeriod= yearStart)
    dfSummary$cvPG <- sd(dfProductionWorld[which(dfProductionWorld$Year>=yearStart&dfProductionWorld$Year<=(yearStart+7)),"ProductionDet"],na.rm=T)/mean(dfProductionWorld[which(dfProductionWorld$Year>=yearStart&dfProductionWorld$Year<=(yearStart+7)),"Production"],na.rm=T)
    dfSummary$cvPL <- sd(dfProductionSumCtry[which(dfProductionSumCtry$Year>=yearStart&dfProductionSumCtry$Year<=(yearStart+7)),"ProductionDet"],na.rm=T)/mean(dfProductionSumCtry[which(dfProductionSumCtry$Year>=yearStart&dfProductionSumCtry$Year<=(yearStart+7)),"Production"],na.rm=T)
     # cv yield
    dfSummary$cvYG <- sd(dfProductionWorld[which(dfProductionWorld$Year>=yearStart&dfProductionWorld$Year<=(yearStart+7)),"YieldDet"],na.rm=T)/mean(dfProductionWorld[which(dfProductionWorld$Year>=yearStart&dfProductionWorld$Year<=(yearStart+7)),"Yield"],na.rm=T)
    dfSummary$cvYL <- sd(dfProductionSumCtry[which(dfProductionSumCtry$Year>=yearStart&dfProductionSumCtry$Year<=(yearStart+7)),"YieldDet"],na.rm=T)/mean(dfProductionSumCtry[which(dfProductionSumCtry$Year>=yearStart&dfProductionSumCtry$Year<=(yearStart+7)),"Yield"],na.rm=T)
    # mean yield
    dfSummary$yieldG <- mean(dfProductionWorld[which(dfProductionWorld$Year>=yearStart&dfProductionWorld$Year<=(yearStart+7)),"Yield"],na.rm=T)
    dfSummary$yieldL <- mean(dfProductionSumCtry[which(dfProductionSumCtry$Year>=yearStart&dfProductionSumCtry$Year<=(yearStart+7)),"Yield"],na.rm=T)
    
    dfSummary$diversity <- mean(dfShannonCtry[which(dfShannonCtry$Year>=yearStart&dfShannonCtry$Year<=(yearStart+7)),"diversity"],na.rm=T)
    

    dfSummary$meanCropland <- mean(dfCroplandCtry[which(dfCroplandCtry$Year>=yearStart&dfCroplandCtry$Year<=(yearStart+7)),"croplandArea"],na.rm=T)
    dfSummary$meanNitrogen <- mean(dfFertilizerCtry[which(dfFertilizerCtry$Year>=yearStart&dfFertilizerCtry$Year<=(yearStart+7)),"Nitrogen"],na.rm=T)
    dfSummary$meanIrrigation_share <- mean(dfIrrigationCtry[which(dfIrrigationCtry$Year>=yearStart&dfIrrigationCtry$Year<=(yearStart+7)),"Irrigation%"],na.rm=T)
    dfSummary$instabilityTemp <- -(mean(dfClimateCtry[which(dfClimateCtry$Year>=yearStart&dfClimateCtry$Year<=(yearStart+7)),"meanTemp"],na.rm=T)/sd(dfClimateCtry[which(dfClimateCtry$Year>=yearStart&dfClimateCtry$Year<=(yearStart+7)),"meanTemp"],na.rm=T))
    dfSummary$instabilityPrec <- -(mean(dfClimateCtry[which(dfClimateCtry$Year>=yearStart&dfClimateCtry$Year<=(yearStart+7)),"meanPrec"],na.rm=T)/sd(dfClimateCtry[which(dfClimateCtry$Year>=yearStart&dfClimateCtry$Year<=(yearStart+7)),"meanPrec"],na.rm=T))
    # dfSummary$meanWarfare <- mean(dfWarfare[which(dfWarfare$Year>=yearStart&dfWarfare$Year<=(yearStart+7)),"warfare"],na.rm=T)
    
    na.omit(dfSummary)
  })
  do.call(rbind,lsAggregate)
})
dfAll <- do.call(rbind,lsAll)
head(dfAll)
nrow(unique(dfAll[,c("Area","timePeriod")])) == nrow(dfAll) # check duplicates

unique(dfAll$timePeriod)
head(dfAll)

# # ratio CV
# dfAll$ratioStabilityG <- dfAll$cvG/dfAll$cvL
# dfAll$ratioStabilityYG <- dfAll$cvYG/dfAll$cvYL
# 
# 
# # ratio yield
# dfAll$ratioYieldG <- dfAll$yieldL/dfAll$yieldG

## calculate nitrogen per ha
dfAll$meanNitrogen_t_ha <- dfAll$meanNitrogen/dfAll$meanCropland



# check NA
sum(is.na(dfAll))
## omit NA
dfAll <- na.omit(dfAll)
length(unique(dfAll$Area)) ## 152 countries

# 
# dfAll$benefitStabilityG <- "winner"
# dfAll[which(dfAll$ratioStabilityG<1),"benefitStabilityG"] <- "loser"
# table(dfAll$benefitStabilityG)
# 
# dfAll$benefitYieldG <- "winner"
# dfAll[which(dfAll$ratioYieldG<1),"benefitYieldG"] <- "loser"
# table(dfAll$benefitYieldG)

## save dataframe
names(dfAll)
names(dfAll)[1] <- "Country"
dfAll <- dfAll[,c("Country","timePeriod",
                  "cvPG","cvPL","cvYG","cvYL",
                  "yieldG","yieldL",
                  "diversity","meanNitrogen_t_ha","meanIrrigation_share",
                  "instabilityTemp","instabilityPrec")]
write.csv(dfAll, "datasetsDerived/dataFinal_global.csv",row.names=F)



rm(list=ls())
