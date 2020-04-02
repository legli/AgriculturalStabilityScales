## import libraries
# library(haven)
library(vegan)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)

# library(rgeos)
# library(rgdal)

#### read Testbetriebsdaten
dfFull <- as.data.frame(read_sas("P:/egli20191219.sas7bdat"))
head(dfFull)
names(dfFull)
str(dfFull)


# remove columns not needed (fertilizer animals, separate labor)
dfFull <- dfFull[,-which(names(dfFull)%in%c("z0019s02","z0021s02","z1031s02","z2539s02","z2540s02","z2559s02","z5239s06","z5239s07","z5240s06","z5240s07","z7089s03","z7098s03","z7099s03","z8014s02","z8015s02","z8150s02","z8153s02","z8156s02"))]

# adapt names
names(dfFull)[which(names(dfFull)=="jahr")] <- "Jahr"
names(dfFull)[which(names(dfFull)=="z0003s02")] <- "Bundesland"
names(dfFull)[which(names(dfFull)=="z0004s02")] <- "Regierungsbezirk"
names(dfFull)[which(names(dfFull)=="z0023s02")] <- "Methode"
names(dfFull)[which(names(dfFull)=="z0024s02")] <- "Datum"
# names(dfFull)[which(names(dfFull)=="z2539s02")] <- "fertilizer_general_costs"
# names(dfFull)[which(names(dfFull)=="z2540s02")] <- "fertilizer_organic_costs"
names(dfFull)[which(names(dfFull)=="z6119s07")] <- "Landwirtschaftsflaeche"
# names(dfFull)[which(names(dfFull)=="z7099s03")] <- "labor_costs"
# names(dfFull)[which(names(dfFull)=="z2559s02")] <- "pesticides_costs"
# names(dfFull)[which(names(dfFull)=="z1031s02")] <- "machines_costs"

names(dfFull)


## remove farms with zero area
dfFull <- dfFull[which(dfFull$Landwirtschaftsflaeche>0),]

# only keep highest 99.9%
dfCropland <- na.omit(dfFull[,c("key","Jahr","Landwirtschaftsflaeche")])
dfCroplandMean <- aggregate(Landwirtschaftsflaeche~key,dfCropland,mean)
head(dfCroplandMean)
dfCroplandMean$propArea <- dfCroplandMean$Landwirtschaftsflaeche/sum(dfCroplandMean$Landwirtschaftsflaeche)
dfCroplandMean <- dfCroplandMean[order(dfCroplandMean$propArea,decreasing = T),]
dfCroplandMean$cumArea <- cumsum(dfCroplandMean$propArea)
ind <- which(dfCroplandMean$cumArea>=0.999)[1]

dfFull <- dfFull[which(dfFull$key%in%dfCroplandMean[1:ind,"key"]),]

## add spatial id
dfDistrict <- read.csv("datasets/districts_farmlevel.csv",sep=";")
head(dfDistrict)

dfFull <- merge(dfFull,dfDistrict[,c("state","district","districtName","REGION_ID")],by.x=c("Bundesland","Regierungsbezirk"),by.y=c("state","district"))
head(dfFull)
names(dfFull)
unique(dfFull$REGION_ID)

# only consider year from 1.7-30.6 (same time period for all farmers)
sum(is.na(dfFull$Datum))
unique(substr(dfFull$Datum,1,4))
dfFullRed <- dfFull[which(substr(dfFull$Datum,1,4)=="3006"|substr(dfFull$Datum,1,4)=="3106"|
                            substr(dfFull$Datum,1,3)=="306"|substr(dfFull$Datum,1,3)=="697"),]
sort(unique(dfFullRed$Datum))

## remove state, district and ,Datum (not needed anymore)
dfFullRed <- dfFullRed[,-which(names(dfFullRed)%in%c("Regierungsbezirk","Datum"))]
names(dfFullRed)

## only focus on organic vs. conventional
dfFullFocal <- dfFullRed[which(dfFullRed$Methode%in%c(1,3)),]


## resturcture
names(dfFullFocal)
dfFullFocal <- dfFullFocal[,c(2,1,97,96,3,4,5:94)]
names(dfFullFocal)
dfFullFocalR <-  dfFullFocal %>% gather(cropVar, values,names(dfFullFocal)[7:96])
head(dfFullFocalR)
dfFullFocalR$strucpro <- "Flaeche"
dfFullFocalR[which(grepl("s03",dfFullFocalR$cropVar)),"strucpro"] <- "Ertrag"
dfFullFocalR$crop <- gsub("s02|s03","",dfFullFocalR$crop)
dfFullFocalR <- dfFullFocalR[,-which(names(dfFullFocalR)=="cropVar")]
dfFullFocalS <- dfFullFocalR %>% spread(strucpro,values)
head(dfFullFocalS)
nrow(dfFullFocalS)/nrow(dfFullFocalR)==0.5



## read calorie data
dfTargetCrops <- read.csv("datasets/targetCrops_farmlevel.csv")
# # only include crops available across scales
dfTargetCrops <- unique(dfTargetCrops[,c("codeTestbetrieb","cropTestbetrieb","Calories")])
 
# ## only keep crops where calorie data is available
dfActual <- merge(dfFullFocalS,dfTargetCrops,by.x="crop",by.y="codeTestbetrieb")
head(dfActual)
dfActual <- na.omit(dfActual)
sort(as.character(unique(dfActual$cropTestbetrieb)))

# target crops from judith
# vecCrop <- c("Zuckerrüben","Winterweizen, Dinkel","Kartoffeln","Sojabohnen","Äpfel","Sommergerste","Hartweizen, Durum",
             # # "Winterraps","Sommerraps und Rübsen","Roggen","Sonnenblumen","Hafer","CCM","Energiemais","Körnermais",
             # "Erdbeeren","Energieölsaaten","Ackerbohnen","Süßkirschen","Pflaumen und Zwetschgen","Sauerkirschen, Schattenmorellen")  

# vecCrop <- c("Zuckerrüben","Winterweizen, Dinkel","Kartoffeln","Sommergerste","Hartweizen, Durum",
             # "Winterraps","Roggen","Hafer","CCM","Körnermais")  


# dfActual <- dfActual[which(dfActual$cropTestbetrieb%in%vecCrop),]
# length(unique(dfActual$cropTestbetrieb))==length(vecCrop)

table(dfActual$Methode)
dfActual$Anbaumethode <- "konventionell"
dfActual[which(dfActual$Methode==3),"Anbaumethode"] <- "oekologisch"


### check year availability
table(dfActual[which(dfActual$Bundesland==14),c("Jahr","Anbaumethode")])
table(dfActual[which(dfActual$districtName=="Leipzig"),c("Jahr","Anbaumethode")])

### remove 0 yield and area
sum(is.na(dfActual))
sum(is.na(dfActual$Ertrag))
sum(dfActual$Ertrag==0)
sum(dfActual$Flaeche==0)
dfActual <- dfActual[which(dfActual$Ertrag>0),]
dfActual <- dfActual[which(dfActual$Flaeche>0),]

## erträge Deutschland
# production in calories
dfGermany <- dfActual
dfGermany$productionCal <- dfGermany$Ertrag*dfGermany$Flaeche*dfGermany$Calories

vecYear <- unique(dfGermany$Jahr)
lsYear <- lapply(vecYear,function(y){
  dfYear <- dfGermany[which(dfGermany$Jahr==y),]
  vecCrop <- unique(dfYear$cropTestbetrieb)
  lsYield <- lapply(vecCrop,function(c){
    c <- as.character(c)
    dfCropK <- dfYear[which(dfYear$cropTestbetrieb==c&dfYear$Anbaumethode=="konventionell"),]
    dfCropO <- dfYear[which(dfYear$cropTestbetrieb==c&dfYear$Anbaumethode=="oekologisch"),]
    
    data.frame(Jahr=c(y,y),Kultur=c(c,c),Anbaumethode=c("konventionell","oekologisch"),
               nBetriebe=c(length(unique(dfCropK$key)),length(unique(dfCropO$key))),
               Mittelwert=c(mean(dfCropK$Ertrag),mean(dfCropO$Ertrag)),
               sd = c(sd(dfCropK$Ertrag),sd(dfCropO$Ertrag)),
               se = c(sd(dfCropK$Ertrag)/nrow(dfCropK),sd(dfCropO$Ertrag)/nrow(dfCropO)),
               Minimum=c(min(dfCropK$Ertrag),min(dfCropO$Ertrag)),
               Maximum=c(max(dfCropK$Ertrag),max(dfCropO$Ertrag)),
               sumCalories=c(sum(dfCropK$productionCal),sum(dfCropO$productionCal)),
               sumFlaeche=c(sum(dfCropK$Flaeche),sum(dfCropO$Flaeche)))
  })
  dfYield <- do.call(rbind,lsYield)
  dfYield
})
dfYieldGermanyAll <- do.call(rbind,lsYear)
head(dfYieldGermanyAll)

dfYieldGermanyAll[which(is.infinite(dfYieldGermanyAll$Minimum)),"Minimum"] <- NA
dfYieldGermanyAll[which(is.infinite(dfYieldGermanyAll$Maximum)),"Maximum"] <- NA
write.csv(dfYieldGermanyAll,"C:/Users/egli/Nextcloud/Cloud/Maasterarbeiten_Selbstversorgung/Testbetriebsnetz/ErtrageJahrGermany.csv",row.names = F)



## saxony
dfSaxonyAll <-dfActual[which(dfActual$Anbaumethode%in%c("konventionell","oekologisch")&dfActual$Bundesland==14),]
vecYear <- unique(dfSaxonyAll$Jahr)
names(dfSaxonyAll)

# production in calories
dfSaxonyAll$productionCal <- dfSaxonyAll$Ertrag*dfSaxonyAll$Flaeche*dfSaxonyAll$Calories

lsYear <- lapply(vecYear,function(y){
  dfYear <- dfSaxonyAll[which(dfSaxonyAll$Jahr==y),]
  vecCrop <- unique(dfYear$cropTestbetrieb)
  lsYield <- lapply(vecCrop,function(c){
    c <- as.character(c)
    dfCropK <- dfYear[which(dfYear$cropTestbetrieb==c&dfYear$Anbaumethode=="konventionell"),]
    dfCropO <- dfYear[which(dfYear$cropTestbetrieb==c&dfYear$Anbaumethode=="oekologisch"),]
    
    data.frame(Jahr=c(y,y),Kultur=c(c,c),Anbaumethode=c("konventionell","oekologisch"),
               nBetriebe=c(length(unique(dfCropK$key)),length(unique(dfCropO$key))),
               Mittelwert=c(mean(dfCropK$Ertrag),mean(dfCropO$Ertrag)),
               sd = c(sd(dfCropK$Ertrag),sd(dfCropO$Ertrag)),
               se = c(sd(dfCropK$Ertrag)/nrow(dfCropK),sd(dfCropO$Ertrag)/nrow(dfCropO)),
               Minimum=c(min(dfCropK$Ertrag),min(dfCropO$Ertrag)),
               Maximum=c(max(dfCropK$Ertrag),max(dfCropO$Ertrag)),
               sumCalories=c(sum(dfCropK$productionCal),sum(dfCropO$productionCal)),
               sumFlaeche=c(sum(dfCropK$Flaeche),sum(dfCropO$Flaeche)))
    })
  dfYieldSaxony <- do.call(rbind,lsYield)
  dfYieldSaxony
})
dfYieldSaxonyAll <- do.call(rbind,lsYear)
head(dfYieldSaxonyAll)

dfYieldSaxonyAll[which(is.infinite(dfYieldSaxonyAll$Minimum)),"Minimum"] <- NA
dfYieldSaxonyAll[which(is.infinite(dfYieldSaxonyAll$Maximum)),"Maximum"] <- NA
write.csv(dfYieldSaxonyAll,"C:/Users/egli/Nextcloud/Cloud/Maasterarbeiten_Selbstversorgung/Testbetriebsnetz/ErtrageJahrSachsen.csv",row.names = F)





### diversität pro betrieb und Jahr
names(dfSaxonyAll)
sum(is.na(dfSaxonyAll$Flaeche))
dfSaxonyDiversity <- aggregate(Flaeche~key+Jahr+Anbaumethode,dfSaxonyAll,diversity)
head(dfSaxonyDiversity)
# mittel pro jahr
dfSaxonyDiversityMean <- dfSaxonyDiversity %>%
  group_by(Anbaumethode,Jahr) %>%
  summarise(n = n(),
            mean = mean(Flaeche),
            median = median(Flaeche),
            sd = sd(Flaeche),
            min=min(Flaeche),
            max=max(Flaeche)) 

dfSaxonyDiversityMean <- as.data.frame(dfSaxonyDiversityMean)
names(dfSaxonyDiversityMean)
write.csv(dfSaxonyDiversityMean,"C:/Users/egli/Nextcloud/Cloud/Maasterarbeiten_Selbstversorgung/Testbetriebsnetz/ShannonJahrSachsen.csv",row.names = F)



### gesamtertrag pro betrieb und Jahr
names(dfSaxonyAll)
sum(is.na(dfSaxonyAll$Flaeche))
sum(is.na(dfSaxonyAll$productionCal))
dfSaxonyYieldTotal <- aggregate(cbind(productionCal,Flaeche)~key+Jahr+Anbaumethode,dfSaxonyAll,sum)
head(dfSaxonyYieldTotal)
dfSaxonyYieldTotal$ErtragCal <- dfSaxonyYieldTotal$productionCal/dfSaxonyYieldTotal$Flaeche

# only 2012-2018
dfSaxonyYieldTotal <- dfSaxonyYieldTotal[which(dfSaxonyYieldTotal$Jahr>=2012&dfSaxonyYieldTotal$Jahr<=2018),]
# cv per farm
sum(is.na(dfSaxonyYieldTotal$ErtragCal))
dfSaxonyYieldTotal <- na.omit(dfSaxonyYieldTotal)
dfSaxonyCVTOtal <- aggregate(ErtragCal~key+Anbaumethode,dfSaxonyYieldTotal,function(i){sd(i)/mean(i)})
head(dfSaxonyCVTOtal)  
dfSaxonyCVTOtal <- na.omit(dfSaxonyCVTOtal)

# mittel pro anbaumethode
dfSaxonyCVFinal <- dfSaxonyCVTOtal %>%
  group_by(Anbaumethode) %>%
  summarise(n = n(),
            mean = mean(ErtragCal),
            median = median(ErtragCal),
            sd = sd(ErtragCal),
            min=min(ErtragCal),
            max=max(ErtragCal)) 

dfSaxonyCVFinal <- as.data.frame(dfSaxonyCVFinal)
write.csv(dfSaxonyCVFinal,"C:/Users/egli/Nextcloud/Cloud/Maasterarbeiten_Selbstversorgung/Testbetriebsnetz/meanCVSachsen_2012_2018.csv",row.names = F)

### stability pro betrieb und Jahr

# only 2012-2018
dfSaxonyYieldCrop <- dfSaxonyAll[which(dfSaxonyAll$Jahr>=2012&dfSaxonyAll$Jahr<=2018),]
# cv per farm
sum(is.na(dfSaxonyYieldCrop$Ertrag))
dfSaxonyCVCrop <- aggregate(Ertrag~key+Anbaumethode+cropTestbetrieb,dfSaxonyYieldCrop,function(i){sd(i)/mean(i)})
head(dfSaxonyCVCrop)  
dfSaxonyCVCrop <- na.omit(dfSaxonyCVCrop)

# mittel pro anbaumethode
dfSaxonyCVCropFinal <- dfSaxonyCVCrop %>%
  group_by(Anbaumethode,cropTestbetrieb) %>%
  summarise(n = n(),
            mean = mean(Ertrag),
            median = median(Ertrag),
            sd = sd(Ertrag),
            min=min(Ertrag),
            max=max(Ertrag)) 

dfSaxonyCVCropFinal <- as.data.frame(dfSaxonyCVCropFinal)
write.csv(dfSaxonyCVCropFinal,"C:/Users/egli/Nextcloud/Cloud/Maasterarbeiten_Selbstversorgung/Testbetriebsnetz/cropsCVSachsen_2012_2018.csv",row.names = F)






## bio Sachsen  only consider focal time frame (2012-2018)
dfSaxony <-dfActual[which(dfActual$Jahr>=2012&dfActual$Jahr<=2018&dfActual$Bundesland==14&dfActual$Anbaumethode%in%c("konventionell","oekologisch")),]
vecCrop <- unique(dfSaxony$cropTestbetrieb)
names(dfSaxony)

lsYield <- lapply(vecCrop,function(c){
  dfCropK <- dfSaxony[which(dfSaxony$cropTestbetrieb==c&dfSaxony$Anbaumethode=="konventionell"),]
  dfCropO <- dfSaxony[which(dfSaxony$cropTestbetrieb==c&dfSaxony$Anbaumethode=="oekologisch"),]
  
  data.frame(crop=c,nK=nrow(dfCropK),nBetriebeK=length(unique(dfCropK$key)),MittelwertK=mean(dfCropK$Ertrag),MaximumK=max(dfCropK$Ertrag),
                    nO=nrow(dfCropO),nBetriebeO=length(unique(dfCropO$key)),MittelwertO=mean(dfCropO$Ertrag),MaximumO=max(dfCropO$Ertrag))
})
dfYieldSaxony <- do.call(rbind,lsYield)
head(dfYieldSaxony)

dfYieldSaxony <- na.omit(dfYieldSaxony)
write.csv(dfYieldSaxony,"C:/Users/egli/Nextcloud/Cloud/Maasterarbeiten_Selbstversorgung/Testbetriebsnetz/ErtrageMittel.csv",row.names = F)
          
## assume 75% of yield gap closing
dfYieldSaxonyOrgainic <- dfYieldSaxony[,c(1,6,8,9)]
dfYieldSaxonyOrgainic <- dfYieldSaxonyOrgainic[which(dfYieldSaxonyOrgainic$nO>=5),] ## only keep crops with at least 5 farms
dfYieldSaxonyOrgainic$ErtragOptimiert <- dfYieldSaxonyOrgainic$MaximumO*0.75
sum(dfYieldSaxonyOrgainic$ErtragOptimiert>dfYieldSaxonyOrgainic$MittelwertO)==nrow(dfYieldSaxonyOrgainic)
dfYieldSaxonyOrgainic$Ertragssteigerung <- (dfYieldSaxonyOrgainic$ErtragOptimiert/dfYieldSaxonyOrgainic$MittelwertO)
hist(dfYieldSaxonyOrgainic$Ertragssteigerung)
write.csv(dfYieldSaxonyOrgainic,"C:/Users/egli/Nextcloud/Cloud/Maasterarbeiten_Selbstversorgung/Testbetriebsnetz/ErtrageBioOptimiert.csv",row.names = F)

## variability per farmer
lsVariability <- lapply(vecCrop,function(c){
  dfCrop <- dfSaxony[which(dfSaxony$cropTestbetrieb==c),]
  vecKey <- unique(dfCrop$key)
  lsKey <- lapply(vecKey,function(k){
    dfKey <- dfCrop[which(dfCrop$key==k),]
    meth <- unique(dfKey$Anbaumethode)
    if (length(meth)==1)
    {
    data.frame(crop=c,key=k,n=nrow(dfKey),Mittelwert=mean(dfKey$Ertrag),sd=sd(dfKey$Ertrag),cv=sd(dfKey$Ertrag)/mean(dfKey$Ertrag),Anbaumethode=meth)
    }
  })
  dfKey <- do.call(rbind,lsKey)
  dfKey <- na.omit(dfKey)
  dfKey <- dfKey[which(dfKey$n>=3),]
  dfKeyK <- dfKey[which(dfKey$Anbaumethode=="konventionell"),]
  dfKeyO <- dfKey[which(dfKey$Anbaumethode=="oekologisch"),]
  
  data.frame(crop=c,nK=nrow(dfKeyK),MittelwertK=mean(dfKeyK$Mittelwert),sdK=mean(dfKeyK$sd),cvK=mean(dfKeyK$cv),
                    nO=nrow(dfKeyO),MittelwertO=mean(dfKeyO$Mittelwert),sdO=mean(dfKeyO$sd),cvO=mean(dfKeyO$cv))
})
dfVariabilitySaxony <- do.call(rbind,lsVariability)
dfVariabilitySaxony <- na.omit(dfVariabilitySaxony)
head(dfYieldSaxony)

#### calculate stability 
head(dfActual)
dfActualStability <- dfActual[which(dfActual$Jahr>=2012&dfActual$Jahr<=2018),]


dfActualStability$Produktion <- dfActualStability$Ertrag*dfActualStability$Flaeche
dfActualStability$ProduktionCal <- dfActualStability$Produktion*dfActualStability$Calories

# erträge für target regois
dfDeutschland <- aggregate(cbind(ProduktionCal,Flaeche)~Jahr,dfActualStability,sum)
dfDeutschland$Ertrag <- dfDeutschland$ProduktionCal/dfDeutschland$Flaeche
dfDeutschlandMethode <- aggregate(cbind(ProduktionCal,Flaeche)~Jahr+Anbaumethode,dfActualStability,sum)
dfDeutschlandMethode$Ertrag <- dfDeutschlandMethode$ProduktionCal/dfDeutschlandMethode$Flaeche

dfLeipzig <- aggregate(cbind(ProduktionCal,Flaeche)~Jahr,dfActualStability[which(dfActualStability$districtName=="Leipzig"),],sum)
dfLeipzig$Ertrag <- dfLeipzig$ProduktionCal/dfLeipzig$Flaeche

dfSachsenMethode <- aggregate(cbind(ProduktionCal,Flaeche)~Jahr+Anbaumethode,dfActualStability[which(dfActualStability$Bundesland==14),],sum)
dfSachsenMethode$Ertrag <- dfSachsenMethode$ProduktionCal/dfSachsenMethode$Flaeche

dfOhneLeipzig <- aggregate(cbind(ProduktionCal,Flaeche)~Jahr,dfActualStability[-which(dfActualStability$districtName=="Leipzig"),],sum)
dfOhneLeipzig$Ertrag <- dfOhneLeipzig$ProduktionCal/dfOhneLeipzig$Flaeche
dfOhneLeipzigMethode <- aggregate(cbind(ProduktionCal,Flaeche)~Jahr+Anbaumethode,dfActualStability[-which(dfActualStability$districtName=="Leipzig"),],sum)
dfOhneLeipzigMethode$Ertrag <- dfOhneLeipzigMethode$ProduktionCal/dfOhneLeipzigMethode$Flaeche

# stabilität für target regions
sd(dfDeutschland$Ertrag)/mean(dfDeutschland$Ertrag)
sd(dfDeutschlandMethode[which(dfDeutschlandMethode$Anbaumethode=="oekologisch"),"Ertrag"])/mean(dfDeutschlandMethode[which(dfDeutschlandMethode$Anbaumethode=="oekologisch"),"Ertrag"])
sd(dfDeutschlandMethode[which(dfDeutschlandMethode$Anbaumethode=="konventionell"),"Ertrag"])/mean(dfDeutschlandMethode[which(dfDeutschlandMethode$Anbaumethode=="konventionell"),"Ertrag"])

sd(dfLeipzig$Ertrag)/mean(dfLeipzig$Ertrag)
sd(dfSachsenMethode[which(dfSachsenMethode$Anbaumethode=="oekologisch"),"Ertrag"])/mean(dfSachsenMethode[which(dfSachsenMethode$Anbaumethode=="oekologisch"),"Ertrag"])
sd(dfSachsenMethode[which(dfSachsenMethode$Anbaumethode=="konventionell"),"Ertrag"])/mean(dfSachsenMethode[which(dfSachsenMethode$Anbaumethode=="konventionell"),"Ertrag"])

sd(dfOhneLeipzig$Ertrag)/mean(dfOhneLeipzig$Ertrag)
sd(dfOhneLeipzigMethode[which(dfOhneLeipzigMethode$Anbaumethode=="oekologisch"),"Ertrag"])/mean(dfOhneLeipzigMethode[which(dfOhneLeipzigMethode$Anbaumethode=="oekologisch"),"Ertrag"])
sd(dfOhneLeipzigMethode[which(dfOhneLeipzigMethode$Anbaumethode=="konventionell"),"Ertrag"])/mean(dfOhneLeipzigMethode[which(dfOhneLeipzigMethode$Anbaumethode=="konventionell"),"Ertrag"])


## diversität leipzig
dfShannon <- aggregate(cbind(ProduktionCal)~Jahr+Anbaumethode,dfActualStability[which(dfActualStability$Bundesland==14),],diversity)


rm(list=ls())

