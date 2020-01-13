library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(sf)
library(rgdal)

## globals

vecColors <- brewer.pal(5,"PuBu")
vecColors2 <- c(brewer.pal(11,"PuOr")[c(1,4,10)],"darkgreen")

lev <- c("National","Subnational","Farm")
myColors <- c("#4daf4a",vecColors[5], "#ff7f00")
names(myColors) <- factor(lev,levels=lev)



###### National level
dfNational <- read.csv("datasetsDerived/dataFinal_global.csv")
sum(dfNational$benefitStabilityG=="winner")/nrow(dfNational)
sum(dfNational$benefitYieldG=="winner")/nrow(dfNational)


## trade-off between yield benefit and stability benefit
cor(dfNational$ratioStabilityG,dfNational$ratioYieldG)
plot(dfNational$ratioStabilityG,dfNational$ratioYieldG,xlim=c(0,5),ylim=c(0,5))

dfLogNational=with(dfNational,data.frame(Country,
                                  ratioStabilityG = log(ratioStabilityG),
                                  ratioYieldG = log(ratioYieldG),
                                  asynchrony,diversity, 
                                  irrigation=sqrt(meanIrrigation_share),
                                  fertilizer=sqrt(meanNitrogen_t_ha),
                                  instabilityTemp,instabilityPrec,
                                  timePeriod
))
names(dfLogNational)
head(dfLogNational)

## scale predictors for standardized regression
dfPredictorsNational=sapply(dfLogNational[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterNational=data.frame(Country=dfLogNational[,1],ratioStabilityG=dfLogNational[,2],ratioYieldG=dfLogNational[,3],dfPredictorsNational)
head(dfCenterNational)

## regression models
modAsynchronyStabilityNational <- lm(ratioStabilityG~asynchrony+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterNational)
summary(modAsynchronyStabilityNational)
modDiversityStabilityNational <- lm(ratioStabilityG~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterNational)
summary(modDiversityStabilityNational)

modAsynchronyYieldNational <- lm(ratioYieldG~asynchrony+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterNational)
summary(modAsynchronyYieldNational)
modDiversityYieldNational <- lm(ratioYieldG~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterNational)
summary(modDiversityYieldNational)

# modStabilityG0 <- lm(ratioStabilityG~1,data=dfCenterNational)
# 
# modStabilityG2 <- lm(ratioStabilityG~asynchrony+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterNational)
# 
# modStabilityGFull <- lm(ratioStabilityG~(asynchrony+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod)^2,data=dfCenterNational)
# 
# modCV_reduced=stepAIC(modStabilityGFull,k=log(nrow(dfCenterNational)))
# summary(modCV_reduced)
# summary(modStabilityG)
# 
# 
# plot(allEffects(modCV_reduced),multiline=T)
# 
# AIC(modStabilityG0)
# 
# AIC(modStabilityG2)




######## EUROPE
dfSubnational <- read.csv("datasetsDerived/dataFinal_europe.csv")
dfSubnational <- merge(dfSubnational,dfNational[,c("Country","timePeriod","meanNitrogen_t_ha","meanIrrigation_share")],by=c("Country","timePeriod"))

sum(dfSubnational$benefitStabilityG=="winner")/nrow(dfSubnational)
sum(dfSubnational$benefitYieldG=="winner")/nrow(dfSubnational)


dfLogSubnational=with(dfSubnational,data.frame(RegionCode,
                                  ratioStabilityG = log(ratioStabilityG),
                                  ratioYieldG = log(ratioYieldG),
                                  asynchrony,diversity, 
                                  irrigation=sqrt(meanIrrigation_share),
                                  fertilizer=sqrt(meanNitrogen_t_ha),                                  
                                  instabilityTemp,instabilityPrec,
                                  timePeriod
))
names(dfLogSubnational)
head(dfLogSubnational)

## scale predictors for standardized regression
dfPredictorsSubnational=sapply(dfLogSubnational[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterSubnational=data.frame(Area=dfLogSubnational[,1],ratioStabilityG=dfLogSubnational[,2],ratioYieldG=dfLogSubnational[,3],dfPredictorsSubnational)
head(dfCenterSubnational)



## regression models
modAsynchronyStabilitySubnational <- lm(ratioStabilityG~asynchrony+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterSubnational)
summary(modAsynchronyStabilitySubnational)
modDiversityStabilitySubnational <- lm(ratioStabilityG~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterSubnational)
summary(modDiversityStabilitySubnational)

modAsynchronyYieldSubnational <- lm(ratioYieldG~asynchrony+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterSubnational)
summary(modAsynchronyYieldSubnational)
modDiversityYieldSubnational <- lm(ratioYieldG~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterSubnational)
summary(modDiversityYieldSubnational)


####### FARMLEVEL

dfFarm <- read.csv("C:/Users/egli/Nextcloud/Cloud/PhD_Leipzig/Publications/Globalization/datasetsDerived/dataFinal_farmlevel.csv")
sum(dfFarm$benefitStabilityG=="winner")/nrow(dfFarm)
sum(dfFarm$benefitYieldG=="winner")/nrow(dfFarm)

dfLogFarmer=with(dfFarm,data.frame(id,
                                  ratioStabilityG = log(ratioStabilityG),
                                  ratioYieldG = log(ratioYieldG),
                                  asynchrony,diversity,
                                  irrigation=sqrt(meanIrrigation),
                                  fertilizer=sqrt(meanFertilizer_ha),
                                  instabilityTemp,instabilityPrec,
                                  timePeriod
))
names(dfLogFarmer)
head(dfLogFarmer)

## scale predictors for standardized regression
dfPredictorsFarmer=sapply(dfLogFarmer[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterFarmer=data.frame(Area=dfLogFarmer[,1],ratioStabilityG=dfLogFarmer[,2],ratioYieldG=dfLogFarmer[,3],dfPredictorsFarmer)
head(dfCenterFarmer)


## regression models
modAsynchronyStabilityFarmer <- lm(ratioStabilityG~asynchrony+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterFarmer)
summary(modAsynchronyStabilityFarmer)
modDiversityStabilityFarmer <- lm(ratioStabilityG~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterFarmer)
summary(modDiversityStabilityFarmer)

modAsynchronyYieldFarmer <- lm(ratioYieldG~asynchrony+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterFarmer)
summary(modAsynchronyYieldFarmer)
modDiversityYieldFarmer <- lm(ratioYieldG~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterFarmer)
summary(modDiversityYieldFarmer)

plot(dfFarm$ratioStabilityG,dfFarm$ratioYieldG)



###### Figures

# Fig 1. 

dfScaleEffectsMain <- data.frame(ratioMean=c(
                                mean(dfNational$ratioStabilityG),
                                mean(dfSubnational$ratioStabilityG),
                                mean(dfFarm$ratioStabilityG),
                                mean(dfNational$ratioYieldG),
                                mean(dfSubnational$ratioYieldG),
                                mean(dfFarm$ratioYieldG)),
                                ratioSD=c(
                                sd(dfNational$ratioStabilityG)/length(dfNational$ratioStabilityG),
                                sd(dfSubnational$ratioStabilityG)/length(dfSubnational$ratioStabilityG),
                                sd(dfFarm$ratioStabilityG)/length(dfFarm$ratioStabilityG),
                                sd(dfNational$ratioYieldG)/length(dfNational$ratioYieldG),
                                sd(dfSubnational$ratioYieldG)/length(dfSubnational$ratioYieldG),
                                sd(dfFarm$ratioYieldG)/length(dfFarm$ratioYieldG)),
                                propBenefit=c(
                                sum(dfNational$benefitStabilityG=="winner")/nrow(dfNational),
                                sum(dfSubnational$benefitStabilityG=="winner")/nrow(dfSubnational),
                                sum(dfFarm$benefitStabilityG=="winner")/nrow(dfFarm),
                                sum(dfNational$benefitYieldG=="winner")/nrow(dfNational),
                                sum(dfSubnational$benefitYieldG=="winner")/nrow(dfSubnational),
                                sum(dfFarm$benefitYieldG=="winner")/nrow(dfFarm)),
                                nam=c("Stability","Stability","Stability","Yield","Yield","Yield"),
                                Level=rep(c("National","Subnational","Farm"),2)
                                )

dfScaleEffectsMain$Level <- factor(dfScaleEffectsMain$Level, levels = c("National","Subnational","Farm"))


a1 <- ggplot(data=dfScaleEffectsMain, aes(x=nam, y=ratioMean, fill=Level)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=ratioMean-ratioSD, ymax=ratioMean+ratioSD), width=.1,
                position=position_dodge(.9)) +
  theme_classic() +  
  xlab("") +
  ylab("Mean ratio") +
  theme(axis.title.y=element_text(size=8)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8))+
  theme(axis.text.y = element_text(size=8))+
  scale_fill_manual(name = "Level",values = myColors)+
  theme(legend.position = c(0.9, 0.8))+
  theme(legend.position = "none") +
  theme(plot.margin = unit(c(0.2,0.3,-0.5,0.2), "cm")) 

b1 <- ggplot(data=dfScaleEffectsMain, aes(x=nam, y=propBenefit, fill=Level)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_classic() +  
  xlab("") +
  ylab("Prop. winner") +
  theme(axis.title.y=element_text(size=8)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8))+
  theme(axis.text.y = element_text(size=8))+
  scale_fill_manual(name = "Level",values = myColors)+
  theme(legend.position = c(0.9, 0.8))+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))+
  theme(legend.key.size = unit(0.2,"cm")) +
  theme(plot.margin = unit(c(0.2,0.3,-0.5,0.2), "cm")) 


# plot
jpeg("results/Fig1.jpeg", width = 16.9, height = 16.9*0.5, units = 'cm', res = 600)

ggarrange(a1,b1,
          labels = letters[1:2],font.label=list(size=8),
          ncol = 2, nrow = 1)

dev.off()



## Fig 2

# barplot of effects: combine coefficents of both modesl
dfRegNational <- data.frame(summary(modAsynchronyStabilityNational)$coefficients[2:7,c(1,2,4)])
names(dfRegNational) <- c("Effect","SE","pVal")
dfRegNational$nam <- c("Asynchrony","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time")
dfRegNational$Level <- "National"

dfRegSubnational <- data.frame(summary(modAsynchronyStabilitySubnational)$coefficients)[2:7,c(1,2,4)]
names(dfRegSubnational) <- c("Effect","SE","pVal")
dfRegSubnational$nam <- c("Asynchrony","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time")
dfRegSubnational$Level <-  "Subnational"

dfRegFarm <- data.frame(summary(modAsynchronyStabilityFarmer)$coefficients)[2:7,c(1,2,4)]
names(dfRegFarm) <- c("Effect","SE","pVal")
dfRegFarm$nam <- c("Asynchrony","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time")
dfRegFarm$Level <-  "Farm"

dfCombinedStabiliy <- rbind(dfRegNational,dfRegSubnational,dfRegFarm)
dfCombinedStabiliy$Level <- factor(dfCombinedStabiliy$Level, levels = c("National","Subnational","Farm"))
dfCombinedStabiliy$nam <- factor(dfCombinedStabiliy$nam, levels = unique(dfCombinedStabiliy$nam))
dfCombinedStabiliy$labHeight <- dfCombinedStabiliy$Effect + dfCombinedStabiliy$SE + 0.03
dfCombinedStabiliy[which(dfCombinedStabiliy$Effect<0),"labHeight"] <- dfCombinedStabiliy[which(dfCombinedStabiliy$Effect<0),"Effect"]- dfCombinedStabiliy[which(dfCombinedStabiliy$Effect<0),"SE"] - 0.03
dfCombinedStabiliy$lab <- ""
dfCombinedStabiliy[which(dfCombinedStabiliy$pVal<0.05),"lab"] <- "*"
dfCombinedStabiliy[which(dfCombinedStabiliy$pVal<0.01),"lab"] <- "**"
dfCombinedStabiliy[which(dfCombinedStabiliy$pVal<0.001),"lab"] <- "***"
dfCombinedStabiliy[which(dfCombinedStabiliy$pVal>=0.05),"lab"] <- "NS"
dfCombinedStabiliy$lab <- factor(dfCombinedStabiliy$lab, levels = unique(dfCombinedStabiliy$lab))

dfCombinedStabiliy <- dfCombinedStabiliy[unlist(lapply(1:6,function(i)seq(i,18,6))),]
dfText <- data.frame(xpos=sort(c(1:6-0.3,1:6,1:6+0.3)),ypos=dfCombinedStabiliy$labHeight,lab=dfCombinedStabiliy$lab,Level=dfCombinedStabiliy$Level)


a2 <- ggplot(data=dfCombinedStabiliy, aes(x=nam, y=Effect, fill=Level)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=Effect-SE, ymax=Effect+SE), width=.1,
                position=position_dodge(.9)) +
  geom_text(data=dfText,aes(x=xpos,y=ypos,label=lab),size=2)+  
  theme_classic() +  
  xlab("") +
  scale_y_continuous(breaks = round(seq(-0.6,0.6, by = 0.1),1),limits=c(-0.6,0.6)) +
  # scale_y_discrete("Standardized regression coefficient", seq(-0.3,0.3,0.1))+
  ylab("Standardized regression coefficient") +
  theme(axis.title.y=element_text(size=8)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8))+
  theme(axis.text.y = element_text(size=8))+
  scale_fill_manual(name = "Level",values = myColors)+
  geom_hline(yintercept=0)+
  theme(legend.position = "none") +
  theme(plot.margin = unit(c(0.2,0.3,-0.5,0.2), "cm")) 


# b
# barplot of effects: combine coefficents of both modesl
dfRegNational <- data.frame(summary(modAsynchronyYieldNational)$coefficients[2:7,c(1,2,4)])
names(dfRegNational) <- c("Effect","SE","pVal")
dfRegNational$nam <- c("Asynchrony","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time")
dfRegNational$Level <- "National"

dfRegSubnational <- data.frame(summary(modAsynchronyYieldSubnational)$coefficients)[2:7,c(1,2,4)]
names(dfRegSubnational) <- c("Effect","SE","pVal")
dfRegSubnational$nam <- c("Asynchrony","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time")
dfRegSubnational$Level <-  "Subnational"

dfRegFarm <- data.frame(summary(modAsynchronyYieldFarmer)$coefficients)[2:7,c(1,2,4)]
names(dfRegFarm) <- c("Effect","SE","pVal")
dfRegFarm$nam <- c("Asynchrony","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time")
dfRegFarm$Level <-  "Farm"

dfCombinedYield <- rbind(dfRegNational,dfRegSubnational,dfRegFarm)
dfCombinedYield$Level <- factor(dfCombinedYield$Level, levels = c("National","Subnational","Farm"))
dfCombinedYield$nam <- factor(dfCombinedYield$nam, levels = unique(dfCombinedYield$nam))
dfCombinedYield$labHeight <- dfCombinedYield$Effect + dfCombinedYield$SE + 0.03
dfCombinedYield[which(dfCombinedYield$Effect<0),"labHeight"] <- dfCombinedYield[which(dfCombinedYield$Effect<0),"Effect"]- dfCombinedYield[which(dfCombinedYield$Effect<0),"SE"] - 0.03
dfCombinedYield$lab <- ""
dfCombinedYield[which(dfCombinedYield$pVal<0.05),"lab"] <- "*"
dfCombinedYield[which(dfCombinedYield$pVal<0.01),"lab"] <- "**"
dfCombinedYield[which(dfCombinedYield$pVal<0.001),"lab"] <- "***"
dfCombinedYield[which(dfCombinedYield$pVal>=0.05),"lab"] <- "NS"
dfCombinedYield$lab <- factor(dfCombinedYield$lab, levels = unique(dfCombinedYield$lab))

dfCombinedYield <- dfCombinedYield[unlist(lapply(1:6,function(i)seq(i,18,6))),]
dfText <- data.frame(xpos=sort(c(1:6-0.3,1:6,1:6+0.3)),ypos=dfCombinedYield$labHeight,lab=dfCombinedYield$lab,Level=dfCombinedYield$Level)


b2 <- ggplot(data=dfCombinedYield, aes(x=nam, y=Effect, fill=Level)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=Effect-SE, ymax=Effect+SE), width=.1,
                position=position_dodge(.9)) +
  geom_text(data=dfText,aes(x=xpos,y=ypos,label=lab),size=2)+  
  theme_classic() +  
  xlab("") +
  scale_y_continuous(breaks = round(seq(-0.6,0.6, by = 0.1),1),limits=c(-0.6,0.6)) +
  # scale_y_discrete("Standardized regression coefficient", seq(-0.3,0.3,0.1))+
  ylab("") +
  theme(axis.title.y=element_text(size=8)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8))+
  theme(axis.text.y = element_text(size=8))+
  scale_fill_manual(name = "Level",values = myColors)+
  geom_hline(yintercept=0)+
  theme(legend.position = c(0.9, 0.8))+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))+
  theme(legend.key.size = unit(0.2,"cm")) +
  theme(plot.margin = unit(c(0.2,0.3,-0.5,0.2), "cm")) 



# plot
jpeg("results/Fig2.jpeg", width = 16.9, height = 16.9*0.5, units = 'cm', res = 600)

ggarrange(a2,b2,
          labels = letters[1:2],font.label=list(size=8),
          ncol = 2, nrow = 1)

dev.off()


## Fig 3: bivariate maps
grd <- rbind(data.frame(dim2=3,dim1=3,color="#3F2949"),
             data.frame(dim2=2,dim1=3,color="#435786"),
             data.frame(dim2=1,dim1=3,color="#4885C1"),
             data.frame(dim2=3,dim1=2,color="#77324C"),
             data.frame(dim2=2,dim1=2,color="#806A8A"),
             data.frame(dim2=1,dim1=2,color="#89A1C8"),
             data.frame(dim2=3,dim1=1,color="#AE3A4E"),
             data.frame(dim2=2,dim1=1,color="#BC7C8F"),
             data.frame(dim2=1,dim1=1,color="#CABED0"))

grd$color <- as.character(grd$color)


# global
mapNational <- readOGR("spatial/countries_global.shp")
plot(mapNational)
mapNational$Area <- as.character(mapNational$Area)

# aggregate ratios
sum(is.na(dfNational))
dfNationalMean <- aggregate(cbind(ratioStabilityG,ratioYieldG)~Country,dfNational,mean)
head(dfNationalMean)
dfNationalState <- aggregate(dfNational[,c("benefitStabilityG","benefitYieldG")],list(dfNational$Country),function(i){sum(as.character(i)=="winner")/length(i)})
dfNationalMap <- merge(dfNationalMean,dfNationalState,by.x="Country",by.y="Group.1")
dfNationalMap$id <- as.character(dfNationalMap$Country)
head(dfNationalMap)

trintStabilityNational <- as.numeric(quantile(dfNationalMap$ratioStabilityG,probs=seq(0,1,length.out = 4)))
trintYieldNational <- as.numeric(quantile(dfNationalMap$ratioYieldG,probs=seq(0,1,length.out = 4)))

dfNationalMap$dim1 <-car::recode(dfNationalMap$ratioStabilityG,"trintStabilityNational[1]:trintStabilityNational[2]=1; trintStabilityNational[2]:trintStabilityNational[3]=2; trintStabilityNational[3]:trintStabilityNational[4]=3;")
dfNationalMap$dim2 <-car::recode(dfNationalMap$ratioYieldG,"trintYieldNational[1]:trintYieldNational[2]=1; trintYieldNational[2]:trintYieldNational[3]=2; trintYieldNational[3]:trintYieldNational[4]=3;")
dfNationalMap <- merge(dfNationalMap[,c("id","benefitStabilityG","benefitYieldG","dim1","dim2")],grd,by=c("dim1","dim2"))
dfNationalMap$dim1 <-car::recode(dfNationalMap$benefitStabilityG,"0:(1/3)=1; (1/3):(2/3)=2; (2/3):1=3;")
dfNationalMap$dim2 <-car::recode(dfNationalMap$benefitYieldG,"0:(1/3)=1; (1/3):(2/3)=2; (2/3):1=3;")
dfNationalMap <- merge(dfNationalMap[,c("id","color","dim1","dim2")],grd,by=c("dim1","dim2"))
names(dfNationalMap)[4:5] <- c("colorMean","colorState")

mapsBivariateNational <- fortify(mapNational,region="Area")
mapsBivariateNational = join(mapsBivariateNational, dfNationalMap[,c("id","colorMean","colorState")], by="id")

a3 <- ggplot() +
  geom_map(data = mapsBivariateNational, map = mapsBivariateNational,
           aes(x = long, y = lat,  map_id=id, fill=factor(colorMean)),
           colour = "#7f7f7f", size=0.05) +
  scale_fill_identity()+
  theme_void()+
  theme(legend.position="none")

d3 <- ggplot() +
  geom_map(data = mapsBivariateNational, map = mapsBivariateNational,
           aes(x = long, y = lat,  map_id=id, fill=factor(colorState)),
           colour = "#7f7f7f", size=0.05) +
  scale_fill_identity()+
  theme_void()+
  theme(legend.position="none")

# europe
mapSubnational <- readOGR("spatial/regions_europe.shp")
plot(mapSubnational)
mapSubnational$NUTS_ID <- as.character(mapSubnational$NUTS_ID)

# aggregate ratios
sum(is.na(dfSubnational))
dfSubnationalMean <- aggregate(cbind(ratioStabilityG,ratioYieldG)~RegionCode,dfSubnational,mean)
head(dfSubnationalMean)
dfSubnationalState <- aggregate(dfSubnational[,c("benefitStabilityG","benefitYieldG")],list(dfSubnational$RegionCode),function(i){sum(as.character(i)=="winner")/length(i)})
dfSubnationalMap <- merge(dfSubnationalMean,dfSubnationalState,by.x="RegionCode",by.y="Group.1")
dfSubnationalMap$id <- as.character(dfSubnationalMap$RegionCode)
head(dfSubnationalMap)

trintStabilitySubnational <- as.numeric(quantile(dfSubnationalMap$ratioStabilityG,probs=seq(0,1,length.out = 4)))
trintYieldSubnational <- as.numeric(quantile(dfSubnationalMap$ratioYieldG,probs=seq(0,1,length.out = 4)))

dfSubnationalMap$dim1 <-car::recode(dfSubnationalMap$ratioStabilityG,"trintStabilitySubnational[1]:trintStabilitySubnational[2]=1; trintStabilitySubnational[2]:trintStabilitySubnational[3]=2; trintStabilitySubnational[3]:trintStabilitySubnational[4]=3;")
dfSubnationalMap$dim2 <-car::recode(dfSubnationalMap$ratioYieldG,"trintYieldSubnational[1]:trintYieldSubnational[2]=1; trintYieldSubnational[2]:trintYieldSubnational[3]=2; trintYieldSubnational[3]:trintYieldSubnational[4]=3;")
dfSubnationalMap <- merge(dfSubnationalMap[,c("id","benefitStabilityG","benefitYieldG","dim1","dim2")],grd,by=c("dim1","dim2"))
dfSubnationalMap$dim1 <-car::recode(dfSubnationalMap$benefitStabilityG,"0:(1/3)=1; (1/3):(2/3)=2; (2/3):1=3;")
dfSubnationalMap$dim2 <-car::recode(dfSubnationalMap$benefitYieldG,"0:(1/3)=1; (1/3):(2/3)=2; (2/3):1=3;")
dfSubnationalMap <- merge(dfSubnationalMap[,c("id","color","dim1","dim2")],grd,by=c("dim1","dim2"))
names(dfSubnationalMap)[4:5] <- c("colorMean","colorState")

mapsBivariateSubnational <- fortify(mapSubnational,region="NUTS_ID")
mapsBivariateSubnational = join(mapsBivariateSubnational, dfSubnationalMap[,c("id","colorMean","colorState")], by="id")

b3 <- ggplot() +
  geom_map(data = mapsBivariateSubnational, map = mapsBivariateSubnational,
           aes(x = long, y = lat,  map_id=id, fill=factor(colorMean)),
           colour = "#7f7f7f", size=0.05) +
  scale_fill_identity()+
  theme_void()+
  theme(legend.position="none")

e3 <- ggplot() +
  geom_map(data = mapsBivariateSubnational, map = mapsBivariateSubnational,
           aes(x = long, y = lat,  map_id=id, fill=factor(colorState)),
           colour = "#7f7f7f", size=0.05) +
  scale_fill_identity()+
  theme_void()+
  theme(legend.position="none")


# farm
mapFarm <- readOGR("spatial/vg2500_rbz_bld.shp")
plot(mapFarm)
mapFarm$district <- as.character(mapFarm$district)

# aggregate ratios
sum(is.na(dfFarm))
dfFarmMean <- aggregate(cbind(ratioStabilityG,ratioYieldG)~districtCode,dfFarm,mean)
head(dfFarmMean)
dfFarmState <- aggregate(dfFarm[,c("benefitStabilityG","benefitYieldG")],list(dfFarm$districtCode),function(i){sum(as.character(i)=="winner")/length(i)})
dfFarmMap <- merge(dfFarmMean,dfFarmState,by.x="districtCode",by.y="Group.1")
dfFarmMap$id <- as.character(dfFarmMap$districtCode)
head(dfFarmMap)

trintStabilityFarm <- as.numeric(quantile(dfFarmMap$ratioStabilityG,probs=seq(0,1,length.out = 4)))
trintYieldFarm <- as.numeric(quantile(dfFarmMap$ratioYieldG,probs=seq(0,1,length.out = 4)))

dfFarmMap$dim1 <-car::recode(dfFarmMap$ratioStabilityG,"trintStabilityFarm[1]:trintStabilityFarm[2]=1; trintStabilityFarm[2]:trintStabilityFarm[3]=2; trintStabilityFarm[3]:trintStabilityFarm[4]=3;")
dfFarmMap$dim2 <-car::recode(dfFarmMap$ratioYieldG,"trintYieldFarm[1]:trintYieldFarm[2]=1; trintYieldFarm[2]:trintYieldFarm[3]=2; trintYieldFarm[3]:trintYieldFarm[4]=3;")
dfFarmMap <- merge(dfFarmMap[,c("id","benefitStabilityG","benefitYieldG","dim1","dim2")],grd,by=c("dim1","dim2"))
dfFarmMap$dim1 <-car::recode(dfFarmMap$benefitStabilityG,"0:(1/3)=1; (1/3):(2/3)=2; (2/3):1=3;")
dfFarmMap$dim2 <-car::recode(dfFarmMap$benefitYieldG,"0:(1/3)=1; (1/3):(2/3)=2; (2/3):1=3;")
dfFarmMap <- merge(dfFarmMap[,c("id","color","dim1","dim2")],grd,by=c("dim1","dim2"))
names(dfFarmMap)[4:5] <- c("colorMean","colorState")

mapsBivariateFarm <- fortify(mapFarm,region="district")
mapsBivariateFarm = join(mapsBivariateFarm, dfFarmMap[,c("id","colorMean","colorState")], by="id")

c3 <- ggplot() +
  geom_map(data = mapsBivariateFarm, map = mapsBivariateFarm,
           aes(x = long, y = lat,  map_id=id, fill=factor(colorMean)),
           colour = "#7f7f7f", size=0.05) +
  scale_fill_identity()+
  theme_void()+
  theme(legend.position="none")

f3 <- ggplot() +
  geom_map(data = mapsBivariateFarm, map = mapsBivariateFarm,
           aes(x = long, y = lat,  map_id=id, fill=factor(colorState)),
           colour = "#7f7f7f", size=0.05) +
  scale_fill_identity()+
  theme_void()+
  theme(legend.position="none")

# legend
g.legend <- ggplot(grd, aes(dim1,dim2,fill=factor(1:9)))+
  geom_tile()+
  scale_fill_manual(values=grd$color)+
  theme_void()+
  theme(legend.position="none",axis.title=element_text(size=5),
        panel.background=element_blank(),plot.margin=margin(t=10,b=10,l=10))+
  theme(axis.title=element_text(color="black",size=8),
        axis.title.y = element_text(angle = 90))+
  labs(x="Stability benefit",y="Yield benefit") 
# theme(axis.title=element_text(size=8))
library(grid)
vp<-viewport(width=0.15,height=0.3,x=0.35,y=0.1)



jpeg("results/Fig3.jpeg", width = 16.9, height = 16.9*0.5, units = 'cm', res = 600)

ggarrange(a3, b3, c3, d3, e3,f3,
          labels = c(letters[1:6]),font.label=list(size=8),
          ncol = 3, nrow = 2,heights = c(1,1))
print(g.legend,vp=vp)

dev.off()






ggplot(ctryMapMean) +
  geom_sf(aes(fill = color), color = "black",size=0.1)

ggplot() +
  geom_map(data = mapsBivariate, map = mapsBivariate,
           aes(x = long, y = lat,  map_id=id, fill=factor(color)),
           colour = "#7f7f7f", size=0.05) +
  scale_fill_identity()+
  theme_void()+
  theme(legend.position="none")

funMap <- function(shape,df,predictor,title,legend=F){
  target <- shape
  quantiles <- as.numeric(quantile(df[,predictor],na.rm=T,probs=seq(0,1,0.2)))
  target$brks <- cut(target[[predictor]], 
                     breaks=c((min(quantiles)-1), quantiles[2], quantiles[3], quantiles[4], quantiles[5], quantiles[6]), 
                     labels=c(round(quantiles[2],2), round(quantiles[3],2),round(quantiles[4],2),round(quantiles[5],2),round(quantiles[6],2)))
  
  p <- ggplot(target) +
    geom_sf(aes(fill = brks), color = "black",size=0.1)+
    scale_fill_viridis(name=title, discrete=TRUE,drop=F)+
    theme_classic()+
    theme(
      text = element_text(size = 6),
      legend.key.height = unit(0.2, "cm"), legend.key.width = unit(0.2, "cm"),
      axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background=element_blank())
  
  if (!legend)
  {
    p <- p + theme(legend.position = "none")
  }
  
  return(p)
  
}




























## read DF


length(unique(dfGlobal$Area))
names(dfGlobal)
table(dfGlobal$IncomeGroup)

#### 1: explore realtionship between diversity and asynchrony
dfDiversityAsynchronyGlobal <- dfGlobal[,c("Area","timePeriod","IncomeGroup","asynchrony","diversity")]
dfDiversityAsynchronyGlobal$timePeriod <- factor(dfDiversityAsynchronyGlobal$timePeriod,levels = c(1968,1978,1988,1998,2008))
dfDiversityAsynchronyGlobal$IncomeGroup <- factor(dfDiversityAsynchronyGlobal$IncomeGroup,levels = c("High income","Upper middle income","Lower middle income","Low income"))

cor.test(dfDiversityAsynchronyGlobal$diversity,dfDiversityAsynchronyGlobal$asynchrony,method='s')

# test if correlation between diversity and asynchrony decreases over time
modDiversityTimeGlobal <- lmer(asynchrony ~ diversity + (1+diversity|timePeriod), data = dfDiversityAsynchronyGlobal)
modDiversityLMGlobal <- lm(asynchrony ~ diversity,data = dfDiversityAsynchronyGlobal)
summary(modDiversityLMGlobal)
anova(modDiversityTimeGlobal,modDiversityLMGlobal) 

modDiversityTimeFixedGlobal=fixef(modDiversityTimeGlobal)
r.squaredGLMM(modDiversityTimeGlobal) 
modDiversityTimeGroupGlobal <-   coef(modDiversityTimeGlobal)$timePeriod

## plot
a1 <- ggplot(dfDiversityAsynchronyGlobal, aes(x=diversity, y=asynchrony, color = timePeriod)) +
  geom_point(size=0.7) +
  scale_colour_manual(name="Time interval",values = vecColors, labels = c("1961-1970","1971-1980","1981-1990","1991-2000","2001-2010")) +
  scale_radius(range = c(2,12)) +
  geom_abline(intercept = modDiversityTimeGroupGlobal[,1],slope = modDiversityTimeGroupGlobal[,2],color=vecColors)+
  geom_abline(intercept = summary(modDiversityLMGlobal)$coefficients[1,1],slope = summary(modDiversityLMGlobal)$coefficients[2,1],color="black",linetype=3)+
  theme_classic() +  
  xlab("") +
  ylab("Asynchrony") + 
  theme(axis.title.x = element_text(size=8)) +  
  theme(axis.text.x = element_text(size=8)) +
  theme(axis.title.y = element_text(size=8)) +    
  theme(axis.text.y = element_text(size=8)) +
  theme(legend.position = c(0.8, 0.17))+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  theme(legend.key.size = unit(0.2,"cm")) + 
  theme(plot.margin = unit(c(0.2,0.3,0.2,0.2), "cm")) 


## test if correlation between diversity and asynchrony differs between income group
modDiversityIncomeGlobal <- lmer(asynchrony ~ diversity + (1+diversity|IncomeGroup), data = dfDiversityAsynchronyGlobal)

anova(modDiversityIncomeGlobal,modDiversityLMGlobal) 

modDiversityIncomeFixedGlobal=fixef(modDiversityIncomeGlobal)
r.squaredGLMM(modDiversityIncomeGlobal) 
modDiversityIncomeGroupGlobal <-   coef(modDiversityIncomeGlobal)$IncomeGroup

## plot
c1 <- ggplot(dfDiversityAsynchronyGlobal, aes(x=diversity, y=asynchrony, color = IncomeGroup)) +
  geom_point(size=0.7) +
  scale_colour_manual(name="Income group",values = vecColors2, labels = c("High income","Upper middle income","Lower middle income","Low income")) +
  scale_radius(range = c(2,12)) +
  geom_abline(intercept = modDiversityIncomeGroupGlobal[,1],slope = modDiversityIncomeGroupGlobal[,2],color=vecColors2)+
  geom_abline(intercept = summary(modDiversityLMGlobal)$coefficients[1,1],slope = summary(modDiversityLMGlobal)$coefficients[2,1],color="black",linetype=3)+
  theme_classic() +  
  xlab("Diversity") +
  ylab("Asynchrony") + 
  theme(axis.title.x = element_text(size=8)) +  
  theme(axis.text.x = element_text(size=8)) +
  theme(axis.title.y = element_text(size=8)) +    
  theme(axis.text.y = element_text(size=8)) +
  theme(legend.position = c(0.8, 0.17))+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  theme(legend.key.size = unit(0.2,"cm")) +
  theme(plot.margin = unit(c(0.2,0.3,0.2,0.2), "cm")) 



#### 2: regression analyses
# transform variables
dfLogGlobal=with(dfGlobal,data.frame(Area,
                                     stability = log(stability),
                                     diversity,
                                     asynchrony, 
                                     meanIrrigation_share=sqrt(meanIrrigation_share),
                                     meanNitrogen_t_ha=sqrt(meanNitrogen_t_ha),
                                     instabilityTemp,instabilityPrec,
                                     meanWarfare,
                                     timePeriod
))
names(dfLogGlobal)
head(dfLogGlobal)

## scale predictors for standardized regression
dfPredictorsGlobal=sapply(dfLogGlobal[,-c(1:2)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterGlobal=data.frame(Area=dfLogGlobal[,1],stability=dfLogGlobal[,2],dfPredictorsGlobal)
head(dfCenterGlobal)

cor(dfCenterGlobal$diversity,dfCenterGlobal$asynchrony)

# models
modAsynchronyGlobal <- lm(stability~asynchrony+meanIrrigation_share+meanNitrogen_t_ha+meanWarfare+timePeriod+instabilityTemp+instabilityPrec,data=dfCenterGlobal)
summary(modAsynchronyGlobal)

modDiversityGlobal <- lm(stability~diversity+meanIrrigation_share+meanNitrogen_t_ha+meanWarfare+timePeriod+instabilityTemp+instabilityPrec,data=dfCenterGlobal)
summary(modDiversityGlobal)

modTotalGlobal <- lm(stability~diversity+asynchrony+meanIrrigation_share+meanNitrogen_t_ha+meanWarfare+timePeriod+instabilityTemp+instabilityPrec,data=dfCenterGlobal)
summary(modTotalGlobal)

# barplot of effects: combine coefficents of both modesl
dfDiversity <- data.frame(summary(modDiversityGlobal)$coefficients)[2:8,c(1,2,4)]
names(dfDiversity) <- c("Effect","SE","pVal")
colnames(dfDiversity)
dfDiversity$nam <- c("Diversity","sqrt(Irigation)","sqrt(N use intensity)","Warfare","Time","Temperature instability","Precipitation instability")
dfDiversity <- rbind(dfDiversity[1,],data.frame(Effect=0,SE=0,pVal=NA,nam="Asynchrony"),dfDiversity[2:7,])
dfDiversity$Model <- "Diversity"

dfAsynchrony <- data.frame(summary(modAsynchronyGlobal)$coefficients)[2:8,c(1,2,4)]
names(dfAsynchrony) <- c("Effect","SE","pVal")
colnames(dfAsynchrony)
dfAsynchrony$nam <- c("Asynchrony","sqrt(Irigation)","sqrt(N use intensity)","Warfare","Time","Temperature instability","Precipitation instability")
dfAsynchrony <- rbind(data.frame(Effect=0,SE=0,pVal=NA,nam="Diversity"),dfAsynchrony)
dfAsynchrony$Model <-  "Asynchrony"

dfTotal <- data.frame(summary(modTotalGlobal)$coefficients)[2:9,c(1,2,4)]
names(dfTotal) <- c("Effect","SE","pVal")
colnames(dfTotal)
dfTotal$nam <- c("Diversity","Asynchrony","sqrt(Irigation)","sqrt(N use intensity)","Warfare","Time","Temperature instability","Precipitation instability")
dfTotal$Model <- "Combined"

dfCombined <- rbind(dfDiversity,dfAsynchrony,dfTotal)
dfCombined$Model <- factor(dfCombined$Model, levels = unique(dfCombined$Model))
dfCombined$nam <- factor(dfCombined$nam, levels = unique(dfCombined$nam))
dfCombined$labHeight <- dfCombined$Effect + 0.05
dfCombined[which(dfCombined$Effect<0),"labHeight"] <- dfCombined[which(dfCombined$Effect<0),"Effect"] - 0.05
dfCombined$lab <- ""
dfCombined[which(dfCombined$pVal<0.05),"lab"] <- "*"
dfCombined[which(dfCombined$pVal<0.01),"lab"] <- "**"
dfCombined[which(dfCombined$pVal<0.001),"lab"] <- "***"
dfCombined[which(dfCombined$pVal>=0.05),"lab"] <- "NS"
dfCombined$lab <- factor(dfCombined$lab, levels = unique(dfCombined$lab))

dfCombined <- dfCombined[unlist(lapply(1:8,function(i)seq(i,24,8))),]

dfText <- data.frame(xpos=sort(c(1:8-0.3,1:8,1:8+0.3)),ypos=dfCombined$labHeight,lab=dfCombined$lab,Model=dfCombined$Model)

a2 <- ggplot(data=dfCombined, aes(x=nam, y=Effect, fill=Model)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=Effect-SE, ymax=Effect+SE), width=.1,
                position=position_dodge(.9)) +
  geom_text(data=dfText,aes(x=xpos,y=ypos,label=lab),size=2)+  
  theme_classic() +  
  xlab("") +
  scale_y_continuous(breaks = round(seq(-0.6,0.6, by = 0.1),1),limits=c(-0.6,0.6)) +
  # scale_y_discrete("Standardized regression coefficient", seq(-0.3,0.3,0.1))+
  ylab("Standardized regression coefficient") +
  theme(axis.title.y=element_text(size=8)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8))+
  theme(axis.text.y = element_text(size=8))+
  scale_fill_manual(name = "Model",values = myColors)+
  geom_hline(yintercept=0)+
  theme(legend.position = c(0.9, 0.8))+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))+
  theme(legend.key.size = unit(0.2,"cm")) +
  theme(plot.margin = unit(c(0.2,0.3,-0.5,0.2), "cm")) 


##### Europe

## read DF
dfEurope <- read.csv("datasetsDerived/dataFinal_europe.csv")
head(dfEurope)



#### 1: explore relationship between diversity and asynchrony
dfDiversityAsynchronyEurope <- dfEurope[,c("NUTS_ID","timePeriod","Member","asynchrony","diversity")]
dfDiversityAsynchronyEurope$timePeriod <- factor(dfDiversityAsynchronyEurope$timePeriod,levels = c(1978,1988,1998,2008))
dfDiversityAsynchronyEurope$Member <- factor(dfDiversityAsynchronyEurope$Member,levels = c("old","new","not"))
cor.test(dfDiversityAsynchronyEurope$diversity,dfDiversityAsynchronyEurope$asynchrony,method='s')

## test if correlation between diversity and asynchrony decreases over time
modDiversityTimeEurope <- lmer(asynchrony ~ diversity + (1+diversity|timePeriod), data = dfDiversityAsynchronyEurope)
modDiversityLMEurope <- lm(asynchrony ~ diversity,data = dfDiversityAsynchronyEurope)
summary(modDiversityLMEurope)
anova(modDiversityTimeEurope,modDiversityLMEurope) 

modDiversityTimeFixedEurope=fixef(modDiversityTimeEurope)
r.squaredGLMM(modDiversityTimeEurope) 
modDiversityTimeGroupEurope <-   coef(modDiversityTimeEurope)$timePeriod

## plot
b1 <- ggplot(dfDiversityAsynchronyEurope, aes(x=diversity, y=asynchrony, color = timePeriod)) +
  geom_point(size=0.7) +
  scale_colour_manual(name="Time interval",values = vecColors[2:5], labels = c("1978-1987","1988-1997","1998-2007","2008-2017")) +
  scale_radius(range = c(2,12)) +
  geom_abline(intercept = modDiversityTimeGroupEurope[,1],slope = modDiversityTimeGroupEurope[,2],color=vecColors[2:5])+
  geom_abline(intercept = summary(modDiversityLMEurope)$coefficients[1,1],slope = summary(modDiversityLMEurope)$coefficients[2,1],color="black",linetype=3)+
  theme_classic() +  
  xlab("") +
  ylab("") + 
  theme(axis.title.x = element_text(size=8)) +  
  theme(axis.text.x = element_text(size=8)) +
  theme(axis.title.y = element_text(size=8)) +    
  theme(axis.text.y = element_text(size=8)) +
  # theme(legend.position = c(0.8, 0.17))+
  # theme(legend.title = element_text(size = 8),
  #       legend.text = element_text(size = 8)) +
  # theme(legend.key.size = unit(0.2,"cm")) +
  theme(legend.position = "none") +
  theme(plot.margin = unit(c(0.2,0.3,0.2,0.2), "cm")) 

## test if correlation between diversity and asynchrony differs between duration of EU membership
modDiversityMemberEurope <- lmer(asynchrony ~ diversity + (1+diversity|Member), data = dfDiversityAsynchronyEurope)
anova(modDiversityMemberEurope,modDiversityLMEurope) 

modDiversityMemberFixedEurope=fixef(modDiversityMemberEurope)
r.squaredGLMM(modDiversityMemberEurope) 
modDiversityMemberGroupEurope <-   coef(modDiversityMemberEurope)$Member

## plot
d1 <- ggplot(dfDiversityAsynchronyEurope, aes(x=diversity, y=asynchrony, color = Member)) +
  geom_point(size=0.7) +
  scale_colour_manual(name="Membership EU",values = vecColors2[1:3], labels = c("Old","New","NA")) +
  scale_radius(range = c(2,12)) +
  geom_abline(intercept = modDiversityMemberGroupEurope[,1],slope = modDiversityMemberGroupEurope[,2],color=vecColors2[1:3])+
  geom_abline(intercept = summary(modDiversityLMEurope)$coefficients[1,1],slope = summary(modDiversityLMEurope)$coefficients[2,1],color="black",linetype=3)+
  theme_classic() +  
  xlab("Diversity") +
  ylab("") + 
  theme(axis.title.x = element_text(size=8)) +  
  theme(axis.text.x = element_text(size=8)) +
  theme(axis.title.y = element_text(size=8)) +    
  theme(axis.text.y = element_text(size=8)) +
  theme(legend.position = c(0.8, 0.17))+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  theme(legend.key.size = unit(0.2,"cm")) +
  theme(plot.margin = unit(c(0.2,0.3,0.2,0.2), "cm")) 

#### 2: regression analyses

# transform variables
dfLogEurope=with(dfEurope,data.frame(NUTS_ID,
                                     stability = log(stability),
                                     diversity,asynchrony, 
                                     instabilityTemp,instabilityPrec,
                                     timePeriod
))
names(dfLogEurope)
head(dfLogEurope)

## scale predictors for standardized regression
dfPredictorsEurope=sapply(dfLogEurope[,-c(1:2)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterEurope=data.frame(Area=dfLogEurope[,1],stability=dfLogEurope[,2],dfPredictorsEurope)
head(dfCenterEurope)

cor(dfCenterEurope$diversity,dfCenterEurope$asynchrony)
cor(dfCenterEurope$stability,dfCenterEurope$asynchrony)

# models
modAsynchronyEurope <- lm(stability~asynchrony+timePeriod+instabilityTemp+instabilityPrec,data=dfCenterEurope)
summary(modAsynchronyEurope)

modDiversityEurope <- lm(stability~diversity+timePeriod+instabilityTemp+instabilityPrec,data=dfCenterEurope)
summary(modDiversityEurope)

modTotalEurope <- lm(stability~diversity+asynchrony+timePeriod+instabilityTemp+instabilityPrec,data=dfCenterEurope)
summary(modTotalEurope)

# barplot of effects: combine coefficents of both modesl
dfDiversityEurope <- data.frame(summary(modDiversityEurope)$coefficients)[2:5,c(1,2,4)]
names(dfDiversityEurope) <- c("Effect","SE","pVal")
colnames(dfDiversityEurope)
dfDiversityEurope$nam <- c("Diversity","Time","Temperature instability","Precipitation instability")
dfDiversityEurope <- rbind(dfDiversityEurope[1,],data.frame(Effect=0,SE=0,pVal=NA,nam="Asynchrony"),dfDiversityEurope[2:4,])
dfDiversityEurope$Model <- "Diversity"

dfAsynchronyEurope <- data.frame(summary(modAsynchronyEurope)$coefficients)[2:5,c(1,2,4)]
names(dfAsynchronyEurope) <- c("Effect","SE","pVal")
colnames(dfAsynchronyEurope)
dfAsynchronyEurope$nam <- c("Asynchrony","Time","Temperature instability","Precipitation instability")
dfAsynchronyEurope <- rbind(data.frame(Effect=0,SE=0,pVal=NA,nam="Diversity"),dfAsynchronyEurope)
dfAsynchronyEurope$Model <-  "Asynchrony"

dfTotalEurope <- data.frame(summary(modTotalEurope)$coefficients)[2:6,c(1,2,4)]
names(dfTotalEurope) <- c("Effect","SE","pVal")
colnames(dfTotalEurope)
dfTotalEurope$nam <- c("Diversity","Asynchrony","Time","Temperature instability","Precipitation instability")
dfTotalEurope$Model <- "Combined"


dfCombinedEurope <- rbind(dfDiversityEurope,dfAsynchronyEurope,dfTotalEurope)
dfCombinedEurope$Model <- factor(dfCombinedEurope$Model, levels = unique(dfCombinedEurope$Model))
dfCombinedEurope$nam <- factor(dfCombinedEurope$nam, levels = unique(dfCombinedEurope$nam))
dfCombinedEurope$labHeight <- dfCombinedEurope$Effect + 0.05
dfCombinedEurope[which(dfCombinedEurope$Effect<0),"labHeight"] <- dfCombinedEurope[which(dfCombinedEurope$Effect<0),"Effect"] - 0.05
dfCombinedEurope$lab <- ""
dfCombinedEurope[which(dfCombinedEurope$pVal<0.05),"lab"] <- "*"
dfCombinedEurope[which(dfCombinedEurope$pVal<0.01),"lab"] <- "**"
dfCombinedEurope[which(dfCombinedEurope$pVal<0.001),"lab"] <- "***"
dfCombinedEurope[which(dfCombinedEurope$pVal>=0.05),"lab"] <- "NS"
dfCombinedEurope$lab <- factor(dfCombinedEurope$lab, levels = unique(dfCombinedEurope$lab))

dfCombinedEurope <- dfCombinedEurope[unlist(lapply(1:5,function(i)seq(i,15,5))),]

dfTextEurope <- data.frame(xpos=sort(c(1:5-0.3,1:5,1:5+0.3)),ypos=dfCombinedEurope$labHeight,lab=dfCombinedEurope$lab,Model=dfCombinedEurope$Model)

b2 <- ggplot(data=dfCombinedEurope, aes(x=nam, y=Effect, fill=Model)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=Effect-SE, ymax=Effect+SE), width=.1,
                position=position_dodge(.9)) +
  geom_text(data=dfTextEurope,aes(x=xpos,y=ypos,label=lab),size=2)+  
  theme_classic() +  
  xlab("") +
  scale_y_continuous(breaks = round(seq(-0.6,0.6, by = 0.1),1),limits=c(-0.6,0.6)) +
  # scale_y_discrete("Standardized regression coefficient", seq(-0.3,0.3,0.1))+
  ylab("Standardized regression coefficient") +
  theme(axis.title.y=element_text(size=8)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8))+
  theme(axis.text.y = element_text(size=8))+
  scale_fill_manual(name = "Model",values = myColors)+
  geom_hline(yintercept=0)+
  # theme(legend.position = c(0.9, 0.8))+
  # theme(legend.title = element_text(size = 8),
  #       legend.text = element_text(size = 8))+
  # theme(legend.key.size = unit(0.2,"cm")) +
  theme(legend.position = "none") +
  theme(plot.margin = unit(c(0.2,0.3,-0.5,0.2), "cm")) 




###### SAVE FIGURES

## Fig 1
jpeg("results/Fig1.jpeg", width = 16.9, height = 16.9, units = 'cm', res = 600)

ggarrange(a1,b1,c1,d1,
          labels = letters[1:4],font.label=list(size=8),
          ncol = 2, nrow = 2,widths =  c(1,1))

dev.off()

## Fig2
jpeg("results/Fig2.jpeg", width = 16.9, height = 16.9*2/3, units = 'cm', res = 600)

ggarrange(a2,b2,
          labels = letters[1:2],font.label=list(size=8),
          ncol = 2, nrow = 1,widths =  c(1.5,1))

dev.off()


rm(list=ls())

