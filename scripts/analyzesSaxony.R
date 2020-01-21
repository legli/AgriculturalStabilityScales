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
dfFull <- dfFull[,-which(names(dfFull)%in%c("z0019s02","z0021s02","z1031s02","z2539s02","z2540s02","z2559s02","z5239s06","z5239s07","z5240s06","z5240s07","z7099s03","z7089s03","z7098s03","z8014s02","z8015s02","z8150s02","z8153s02","z8156s02"))]

# adapt names

names(dfFull)[which(names(dfFull)=="jahr")] <- "year"
names(dfFull)[which(names(dfFull)=="z0003s02")] <- "state"
names(dfFull)[which(names(dfFull)=="z0004s02")] <- "district"
names(dfFull)[which(names(dfFull)=="z0023s02")] <- "production"
names(dfFull)[which(names(dfFull)=="z0024s02")] <- "date"
names(dfFull)[which(names(dfFull)=="z6119s07")] <- "agriculturalArea"
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

# count occurences of variables per year
dfCount <- aggregate(dfFull,by=list(dfFull$year),function(i){sum(!is.na(i))})
colSums(dfCount)

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


unique(dfFullFocal$districtName)



table(dfFullFocal[which(dfFullFocal$districtName=="Leipzig"),"production"])
table(dfFullFocal[which(dfFullFocal$districtName=="Chemnitz"),"production"])
table(dfFullFocal[which(dfFullFocal$districtName=="Dresden"),"production"])


## resturcture
names(dfFullFocal)
dfFullFocalR <-  dfFullFocal %>% gather(cropVar, values,names(dfFullFocal)[5:94])
head(dfFullFocalR)
dfFullFocalR$strucpro <- "AR"
dfFullFocalR[which(substr(dfFullFocalR$cropVar,7,8)=="03"),"strucpro"] <- "YI"
dfFullFocalR$crop <- substr(dfFullFocalR$cropVar,1,5)
dfFullFocalS <- dfFullFocalR[,-which(names(dfFullFocalR)=="cropVar")] %>% spread(strucpro,values)
head(dfFullFocalS)
nrow(dfFullFocalS)/nrow(dfFullFocalR)==0.5

## read calorie data
dfTargetCrops <- read.csv("datasets/targetCrops_farmlevel.csv")
# only include crops available across scales
dfTargetCrops <- unique(dfTargetCrops[,c("codeTestbetrieb","cropTestbetrieb","calories")])

## only keep crops where calorie data is available
dfActual <- merge(dfFullFocalS,dfTargetCrops,by.x="crop",by.y="codeTestbetrieb")
head(dfActual)

dfActual$mode <- "konventionell"
dfActual[which(dfActual$production==3),"mode"] <- "oekologisch"

dfAggkonv <- dfActual %>%
  filter(!is.na(YI)) %>%
  group_by(state,mode,cropTestbetrieb) %>%
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
