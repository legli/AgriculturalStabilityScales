## import libraries
# library(haven)
# library(vegan)
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
dfFull <- dfFull[,-which(names(dfFull)%in%c("z0019s02","z0021s02","z1031s02","z5239s06","z5239s07","z5240s06","z5240s07","z7089s03","z7098s03","z8014s02","z8015s02","z8150s02","z8153s02","z8156s02"))]

# adapt names

names(dfFull)[which(names(dfFull)=="jahr")] <- "year"
names(dfFull)[which(names(dfFull)=="z0003s02")] <- "state"
names(dfFull)[which(names(dfFull)=="z0004s02")] <- "district"
names(dfFull)[which(names(dfFull)=="z0023s02")] <- "production"
names(dfFull)[which(names(dfFull)=="z0024s02")] <- "date"
names(dfFull)[which(names(dfFull)=="z2539s02")] <- "fertilizer_general_costs"
names(dfFull)[which(names(dfFull)=="z2540s02")] <- "fertilizer_organic_costs"
names(dfFull)[which(names(dfFull)=="z6119s07")] <- "agriculturalArea"
names(dfFull)[which(names(dfFull)=="z7099s03")] <- "labor_costs"
names(dfFull)[which(names(dfFull)=="z2559s02")] <- "pesticides_costs"
names(dfFull)[which(names(dfFull)=="z4040s02")] <- "ZuckerruebenFlaeche"
names(dfFull)[which(names(dfFull)=="z4040s03")] <- "ZuckerruebenErtrag"
names(dfFull)[which(names(dfFull)=="z4001s02")] <- "WinterweizenDinkelFlaeche"
names(dfFull)[which(names(dfFull)=="z4001s03")] <- "WinterweizenDinkelErtrag"
names(dfFull)[which(names(dfFull)=="z4039s02")] <- "KartoffelFlaeche"
names(dfFull)[which(names(dfFull)=="z4039s03")] <- "KartoffelErtrag"
names(dfFull)[which(names(dfFull)=="z4027s02")] <- "SojabohnenFlaeche"
names(dfFull)[which(names(dfFull)=="z4027s03")] <- "SojabohnenErtrag"
names(dfFull)[which(names(dfFull)=="z4200s02")] <- "AepfelFlaeche"
names(dfFull)[which(names(dfFull)=="z4200s03")] <- "AepfelErtrag"
names(dfFull)[which(names(dfFull)=="z4006s02")] <- "SommergersteFlaeche"
names(dfFull)[which(names(dfFull)=="z4006s03")] <- "SommergersteErtrag"
names(dfFull)[which(names(dfFull)=="z4003s02")] <- "HartweizenDurumFlaeche"
names(dfFull)[which(names(dfFull)=="z4003s03")] <- "HartweizenDurumErtrag"
names(dfFull)[which(names(dfFull)=="z4024s02")] <- "WinterrapsFlaeche"
names(dfFull)[which(names(dfFull)=="z4024s03")] <- "WinterrapsErtrag"
names(dfFull)[which(names(dfFull)=="z4025s02")] <- "SommerrapsundRuebsenFlaeche"
names(dfFull)[which(names(dfFull)=="z4025s03")] <- "SommerrapsundRuebsenErtrag"
names(dfFull)[which(names(dfFull)=="z4004s02")] <- "RoggenFlaeche"
names(dfFull)[which(names(dfFull)=="z4004s03")] <- "RoggenErtrag"
names(dfFull)[which(names(dfFull)=="z4026s02")] <- "SonnenblumenFlaeche"
names(dfFull)[which(names(dfFull)=="z4026s03")] <- "SonnenblumenErtrag"

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

# only consider year from 1.7-30.6 (same time period for all farmers)
sum(is.na(dfFull$date))
unique(substr(dfFull$date,1,4))
dfFullRed <- dfFull[which(substr(dfFull$date,1,4)=="3006"|substr(dfFull$date,1,4)=="3106"|
                            substr(dfFull$date,1,3)=="306"|substr(dfFull$date,1,3)=="697"),]
sort(unique(dfFullRed$date))

## remove state, district and date (not needed anymore)
dfFullRed <- dfFullRed[,-which(names(dfFullRed)%in%c("district","date"))]
names(dfFullRed)

## only consider focal time frame (2012-2018)
dfFullFocal <- dfFullRed[which(dfFullRed$year>=2012&dfFullRed$year<=2018),]

## only focus on organic vs. conventional
dfFullFocal <- dfFullFocal[which(dfFullFocal$production%in%c(1,3)),]



sum(is.na(dfFullFocal$fertilizer_general_costs)) # 17607
dfFullFocal[which(is.na(dfFullFocal$fertilizer_general_costs)),"fertilizer_general_costs"] <- 0
sum(is.na(dfFullFocal$fertilizer_organic_costs)) # 207956
dfFullFocal[which(is.na(dfFullFocal$fertilizer_organic_costs)),"fertilizer_organic_costs"] <- 0
sum(is.na(dfFullFocal$croplandArea))# 0
sum(is.na(dfFullFocal$pesticides_costs)) # 207956
dfFullFocal[which(is.na(dfFullFocal$pesticides_costs)),"pesticides_costs"] <- 0
sum(is.na(dfFullFocal$labor_costs)) # 207956
dfFullFocal[which(is.na(dfFullFocal$labor_costs)),"labor_costs"] <- 0

## recalcualte some variables
hist(dfFullFocal$fertilizer_general_costs)
hist(dfFullFocal$fertilizer_organic_costs)
hist(dfFullFocal$pesticides_costs)
hist(dfFullFocal$labor_costs)
dfFullFocal$fertilizer <- rowSums(dfFullFocal[,c("fertilizer_general_costs","fertilizer_organic_costs")],na.rm=T) # total costs for fertilizer

dfFullFocal <- dfFullFocal[,-which(names(dfFullFocal)%in%c("fertilizer_general_costs","fertilizer_organic_costs"))]
hist(dfFullFocal$fertilizer)
dfFullFocal$fertilizer <- dfFullFocal$fertilizer*(-1) # invert income-investment: investment should be postive
min(dfFullFocal$fertilizer)
dfFullFocal$pesticides <- dfFullFocal$pesticides_costs*(-1) # invert income-investment: investment should be postive

dfFullFocal$fertilizer <- dfFullFocal$fertilizer/dfFullFocal$agriculturalArea
dfFullFocal$pesticides <- dfFullFocal$pesticides/dfFullFocal$agriculturalArea
dfFullFocal$labor <- dfFullFocal$labor_costs/dfFullFocal$agriculturalArea

unique(dfFullFocal$districtName)



table(dfFullFocal[which(dfFullFocal$districtName=="Leipzig"),"production"])
table(dfFullFocal[which(dfFullFocal$districtName=="Chemnitz"),"production"])
table(dfFullFocal[which(dfFullFocal$districtName=="Dresden"),"production"])


## resturcture
names(dfFullFocal)
dfFullFocal <- dfFullFocal[,c(1:4,98:102,6,7,10:13,16,17,40:47,64:67,80,81)]
dfFullFocalR <-  dfFullFocal %>% gather(cropVar, values,names(dfFullFocal)[10:31])
head(dfFullFocalR)
dfFullFocalR$strucpro <- "AR"
dfFullFocalR[which(grepl("Flaeche",dfFullFocalR$cropVar)),"strucpro"] <- "YI"
dfFullFocalR$crop <- gsub("Flaeche|Ertrag","",dfFullFocalR$cropVar)
dfFullFocalR <- dfFullFocalR[,-which(names(dfFullFocalR)=="cropVar")]
dfFullFocalS <- dfFullFocalR %>% spread(strucpro,values)
head(dfFullFocalS)
nrow(dfFullFocalS)/nrow(dfFullFocalR)==0.5

## read calorie data
# dfTargetCrops <- read.csv("datasets/targetCrops_farmlevel.csv")
# # only include crops available across scales
# dfTargetCrops <- unique(dfTargetCrops[,c("codeTestbetrieb","cropTestbetrieb","calories")])
# 
# ## only keep crops where calorie data is available
# dfActual <- merge(dfFullFocalS,dfTargetCrops,by.x="crop",by.y="codeTestbetrieb")
# head(dfActual)

dfActual <- na.omit(dfFullFocalS)

dfActual$mode <- "konventionell"
dfActual[which(dfActual$production==3),"mode"] <- "oekologisch"


dfSaxony <-dfActual[which(dfActual$state==14),]

dfKartoffel <- dfSaxony[which(dfSaxony$crop=="Kartoffel"),]
mod <- lm(YI~(fertilizer+pesticides+labor+mode)^2,dfKartoffel)
summary(mod)
# dfKartoffel <- aggregate(YI~year+mode,dfKartoffel,mean)
head(dfKartoffel)
table(dfKartoffel$mode)

ggplot(dfKartoffel, aes(x=log(YI), color=mode)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")

ggplot(data=dfKartoffel, aes(x=key, y=YI, colour=factor(mode))) +
  geom_point()

hist(log(dfKartoffel$YI))

dfAggkonv <- dfActual %>%
  filter(!is.na(YI)) %>%
  group_by(state,mode,crop) %>%
  summarise(n = n(),
            mean = mean(YI),
            median = median(YI),
            sd = sd(YI),
            min=min(YI),
            max=max(YI)) 
head(dfAggkonv)


dfEast <- as.data.frame(dfAggkonv[which(dfAggkonv$state%in%c(12,14,15,16)),])


a1 <- ggplot(data=dfEast[which(dfEast$state==14),], aes(x=cropTestbetrieb, y=mean, fill=mode)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1,
                position=position_dodge(.9)) +
  theme_classic() +
  ggtitle("Sachsen")+
  xlab("") +
  ylim(0,850)+
  ylab("Ertrag (dt/ha)") +
  theme(axis.title.y=element_text(size=8)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=6))+
  theme(axis.text.y = element_text(size=8))+
  theme(plot.margin = unit(c(0.2,0.3,-0.5,0.2), "cm"))+
  theme(legend.position = "none")

b1 <- ggplot(data=dfEast[which(dfEast$state==15),], aes(x=cropTestbetrieb, y=mean, fill=mode)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1,
                position=position_dodge(.9)) +
  theme_classic() +
  ggtitle("Sachsen-Anhalt")+
  xlab("") +
  ylim(0,850)+
  ylab("Ertrag (dt/ha)") +
  theme(axis.title.y=element_text(size=8)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=6))+
  theme(axis.text.y = element_text(size=8))+
  theme(plot.margin = unit(c(0.2,0.3,-0.5,0.2), "cm"))+
  theme(legend.position = "none")


 
c1 <- ggplot(data=dfEast[which(dfEast$state==16),], aes(x=cropTestbetrieb, y=mean, fill=mode)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1,
                position=position_dodge(.9)) +
  theme_classic() +
  ggtitle("Thueringen")+
  xlab("") +
  ylim(0,850)+
  ylab("Ertrag (dt/ha)") +
  theme(axis.title.y=element_text(size=8)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=6))+
  theme(axis.text.y = element_text(size=8))+
  theme(plot.margin = unit(c(0.2,0.3,-0.5,0.2), "cm"))+
  theme(legend.position = "none")

d1 <- ggplot(data=dfEast[which(dfEast$state==12),], aes(x=cropTestbetrieb, y=mean, fill=mode)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1,
                position=position_dodge(.9)) +
  theme_classic() +
  ggtitle("Brandenburg")+
  xlab("") +
  ylim(0,850)+
  ylab("Ertrag (dt/ha)") +
  theme(axis.title.y=element_text(size=8)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=6))+
  theme(axis.text.y = element_text(size=8))+
  theme(plot.margin = unit(c(0.2,0.3,-0.5,0.2), "cm"))+
  theme(legend.position = "none")

jpeg("results/Fig_Ertraege.jpeg", width = 16.9, height = 16.9, units = 'cm', res = 600)

ggarrange(a1,b1,c1,d1,
          labels = letters[1:4],font.label=list(size=6),
          ncol = 2, nrow = 2)

dev.off()


rm(list=ls())
