library(ggplot2)
library(ggpubr)
library(sf)
library(rgdal)
library(raster)
library(plyr)
library(RColorBrewer)
library(fitdistrplus)
library(countrycode)
library(grid)
library(gridExtra)
library(car)
library(scales)


############################################################################################
###################           GLOBALS               ########################################
############################################################################################

lev <- c("Country","Region","Farm")
myColors <- c("#4daf4a","#045A8D", "#ff7f00")
names(myColors) <- factor(lev,levels=lev)

myColorsModel <- c("#4daf4a","#045A8D")
names(myColorsModel) <- factor(c("Stability","Yield"),levels=c("Stability","Yield"))

vecColMap <- brewer.pal(9,"PuOr")
vecColMap[5] <- "gray75"

source("scripts/functionsAnalyses_Apr2020.R")


############################################################################################
###################              DATA               ########################################
############################################################################################

###### Country level
dfCountry <- read.csv("datasetsDerived/dataFinal_global.csv")
hist(dfCountry$stability) 
hist(dfCountry$stabilityOverall)
hist(dfCountry$stabilityReduced)
dfCountry$ratioS <- dfCountry$stability/dfCountry$stabilityOverall
dfCountry$ratioL <- dfCountry$stabilityOverall/dfCountry$stabilityReduced
nrow(dfCountry)
length(unique(dfCountry$Country))
length(unique(dfCountry[which(dfCountry$timePeriod==1968),"Country"]))
length(unique(dfCountry[which(dfCountry$timePeriod==1978),"Country"]))
length(unique(dfCountry[which(dfCountry$timePeriod==1988),"Country"]))
length(unique(dfCountry[which(dfCountry$timePeriod==1998),"Country"]))
length(unique(dfCountry[which(dfCountry$timePeriod==2008),"Country"]))
sum(table(dfCountry$Country)>1)/length(unique(dfCountry$Country))


# harvested area to Mio ha
dfCountry$areaHarvested <- dfCountry$areaHarvested/1000000

###### Regional level
dfRegion <- read.csv("datasetsDerived/dataFinal_europe.csv")
hist(dfRegion$stability)
hist(dfRegion$stabilityOverall)
hist(dfRegion$stabilityReduced)
dfRegion$ratioS <- dfRegion$stability/dfRegion$stabilityOverall
dfRegion$ratioL <- dfRegion$stabilityOverall/dfRegion$stabilityReduced
nrow(dfRegion)
length(unique(dfRegion$Region))
length(unique(dfRegion[which(dfRegion$timePeriod==1978),"Region"]))
length(unique(dfRegion[which(dfRegion$timePeriod==1988),"Region"]))
length(unique(dfRegion[which(dfRegion$timePeriod==1998),"Region"]))
length(unique(dfRegion[which(dfRegion$timePeriod==2008),"Region"]))
sum(table(dfRegion$Region)>1)/length(unique(dfRegion$Region))

###### Farm level
dfFarm <- read.csv("P:/dataFinal_farmlevel.csv")
hist(dfFarm$stability)
hist(dfFarm$stabilityOverall)
hist(dfFarm$stabilityReduced)
dfFarm$ratioS <- dfFarm$stability/dfFarm$stabilityOverall
dfFarm$ratioL <- dfFarm$stabilityOverall/dfFarm$stabilityReduced
nrow(dfFarm)
length(unique(dfFarm$Farm))
length(unique(dfFarm[which(dfFarm$timePeriod==1998),"Farm"]))
length(unique(dfFarm[which(dfFarm$timePeriod==2008),"Farm"]))
sum(table(dfFarm$Farm)>1)/length(unique(dfFarm$Farm))


############################################################################################
###################          ANALYSES               ########################################
############################################################################################

##################### Regression local 
###### Country level

# check distribution of response variables
hist(dfCountry$stability)
hist(dfCountry$ratioS)
hist(dfCountry$ratioL)

fitdist(dfCountry$stability,"norm")$aic - fitdist(dfCountry$stability,"lnorm")$aic # log normally distributed
fitdist(dfCountry$ratioS,"norm")$aic - fitdist(dfCountry$ratioS,"lnorm")$aic # log normally distributed
fitdist(dfCountry$ratioL,"lnorm")$aic - fitdist(dfCountry$ratioL,"norm")$aic #normally distributed

### regression analyses
## transformations
dfTransCountry=with(dfCountry,data.frame(Country,
                                         stability = log(stability),
                                         ratioS = log(ratioS),
                                         ratioL = ratioL,
                                         diversity, 
                                         irrigation=sqrt(irrigation),
                                         fertilizer=sqrt(fertilizer),
                                         instabilityTemp,instabilityPrec,
                                         areaHarvested = log(areaHarvested),
                                         timePeriod
))


# scale predictors for standardized regression
dfPredictorsCountry=sapply(dfTransCountry[,-c(1:4)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterCountry=data.frame(Country=dfTransCountry[,1],stability=dfTransCountry[,2],ratioS=dfTransCountry[,3],ratioL=dfTransCountry[,4],dfPredictorsCountry)
head(dfCenterCountry)

## check colinearity
cor(dfCenterCountry[,2:11],method='s') 

## regression models
# stability
modStabilityCountryFull <- lm(stability~diversity+fertilizer+irrigation+areaHarvested+instabilityTemp+instabilityPrec+timePeriod
                              ,data=dfCenterCountry)
summary(modStabilityCountryFull)

modStabilityCountry=stepAIC(modStabilityCountryFull,k=log(nrow(dfCenterCountry)))
summary(modStabilityCountry) 
vif(modStabilityCountry)

# ratio small
modRatioSmallCountryFull <- lm(ratioS~diversity+fertilizer+irrigation+areaHarvested+instabilityTemp+instabilityPrec+timePeriod,
                                     data=dfCenterCountry)
summary(modRatioSmallCountryFull)

modRatioSmallCountry=stepAIC(modRatioSmallCountryFull,k=log(nrow(dfCenterCountry)))
summary(modRatioSmallCountry) 
vif(modRatioSmallCountry)

# ratio large
modRatioLargeCountryFull <- lm(ratioL~diversity+fertilizer+irrigation+areaHarvested+instabilityTemp+instabilityPrec+timePeriod
                               ,data=dfCenterCountry)
summary(modRatioLargeCountryFull)

modRatioLargeCountry=stepAIC(modRatioLargeCountryFull,k=log(nrow(dfCenterCountry)))
summary(modRatioLargeCountry) 
vif(modRatioLargeCountry)


###### European level

# check distribution of response variables
hist(dfRegion$stability)
hist(dfRegion$ratioS)
hist(dfRegion$ratioL)
fitdist(dfRegion$stability,"norm")$aic - fitdist(dfRegion$stability,"lnorm")$aic #log normally distributed
fitdist(dfRegion$ratioS,"norm")$aic - fitdist(dfRegion$ratioS,"lnorm")$aic # log normally distributed
fitdist(dfRegion$ratioL,"lnorm")$aic - fitdist(dfRegion$ratioL,"norm")$aic # normally distributed

### regression analyses
## transformations
dfTransRegion=with(dfRegion,data.frame(Region,
                                         stability = log(stability),
                                         ratioS = log(ratioS),
                                         ratioL = ratioL,
                                         diversity, 
                                         irrigation=sqrt(irrigation),
                                         fertilizer=sqrt(fertilizer),
                                         instabilityTemp,instabilityPrec,
                                         areaHarvested = log(areaHarvested),
                                         timePeriod
))

# scale predictors for standardized regression
dfPredictorsRegion=sapply(dfTransRegion[,-c(1:4)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterRegion=data.frame(Region=dfTransRegion[,1],stability=dfTransRegion[,2],ratioS=dfTransRegion[,3],ratioL=dfTransRegion[,4],dfPredictorsRegion)
head(dfCenterRegion)

## check colinearity
cor(dfCenterRegion[,2:11],method='s') 

## regression models
# stability
modStabilityRegionFull <- lm(stability~diversity+fertilizer+irrigation+areaHarvested+instabilityTemp+instabilityPrec+timePeriod
                                        ,data=dfCenterRegion)
summary(modStabilityRegionFull)

modStabilityRegion=stepAIC(modStabilityRegionFull,k=log(nrow(dfCenterRegion)))
summary(modStabilityRegion) 
vif(modStabilityRegion)

# ratio small
modRatioSmallRegionFull <- lm(ratioS~diversity+fertilizer+irrigation+areaHarvested+instabilityTemp+instabilityPrec+timePeriod
                              ,data=dfCenterRegion)
summary(modRatioSmallRegionFull)

modRatioSmallRegion=stepAIC(modRatioSmallRegionFull,k=log(nrow(dfCenterRegion)))
summary(modRatioSmallRegion) 
vif(modRatioSmallRegion)

# ratio large
modRatioLargeRegionFull <- lm(ratioL~diversity+fertilizer+irrigation+areaHarvested+instabilityTemp+instabilityPrec+timePeriod
                                 ,data=dfCenterRegion)
summary(modRatioLargeRegionFull)

modRatioLargeRegion=stepAIC(modRatioLargeRegionFull,k=log(nrow(dfCenterRegion)))
summary(modRatioLargeRegion) 
vif(modRatioLargeRegion)


###### Farm level
# check distribution of response variables
hist(dfFarm$stability)
hist(dfFarm$ratioS)
hist(dfFarm$ratioL)

fitdist(dfFarm$stability,"norm")$aic-fitdist(dfFarm$stability,"lnorm")$aic # log normally distributed
fitdist(dfFarm$ratioS,"norm")$aic-fitdist(dfFarm$ratioS,"lnorm")$aic # log normally distributed
fitdist(dfFarm$ratioL,"lnorm")$aic-fitdist(dfFarm$ratioL,"norm")$aic # normalky distributed

### regression analyses
## transformations
dfTransFarm=with(dfFarm,data.frame(Farm,
                                       stability = log(stability),
                                       ratioS = log(ratioS),
                                       ratioL = ratioL,
                                       diversity, 
                                       irrigation=sqrt(irrigation),
                                       fertilizer=sqrt(fertilizer),
                                       instabilityTemp,instabilityPrec,
                                       areaHarvested = log(areaHarvested),
                                       timePeriod
))

# scale predictors for standardized regression
dfPredictorsFarm=sapply(dfTransFarm[,-c(1:4)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterFarm=data.frame(Farm=dfTransFarm[,1],stability=dfTransFarm[,2],ratioS=dfTransFarm[,3],ratioL=dfTransFarm[,4],dfPredictorsFarm)
head(dfCenterFarm)

## check colinearity
cor(dfCenterFarm[,2:11],method='s') # instabilityTime, instabilityPrec and time very correlated (but no need to exclude due to stepwise model selection)

## regression models
# stability
modStabilityFarmFull <- lm(stability~diversity+fertilizer+irrigation+areaHarvested+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterFarm)

modStabilityFarm=stepAIC(modStabilityFarmFull,k=log(nrow(dfCenterFarm)))
summary(modStabilityFarm) 
vif(modStabilityFarm)

# ratio small
modRatioSmallFarmFull <- lm(ratioS~diversity+fertilizer+irrigation+areaHarvested+instabilityTemp+instabilityPrec+timePeriod
                                 ,data=dfCenterFarm)
summary(modRatioSmallFarmFull)

modRatioSmallFarm=stepAIC(modRatioSmallFarmFull,k=log(nrow(dfCenterFarm)))
summary(modRatioSmallFarm) 
vif(modRatioSmallFarm)

# ratio large
modRatioLargeFarmFull <- lm(ratioL~diversity+fertilizer+irrigation+areaHarvested+instabilityTemp+instabilityPrec+timePeriod
                                 ,data=dfCenterFarm)
summary(modRatioLargeFarmFull)

modRatioLargeFarm=stepAIC(modRatioLargeFarmFull,k=log(nrow(dfCenterFarm)))
summary(modRatioLargeFarm) 
vif(modRatioLargeFarm)




############################################################################################
###################           Manuscript calclations########################################
############################################################################################

# get proportion of high stability units
sum(dfCountry$ratioS>1)/nrow(dfCountry)*100
sum(dfRegion$ratioS>1)/nrow(dfRegion)*100
sum(dfFarm$ratioS>1)/nrow(dfFarm)*100

# get proportion of units stabilizing large sacle variability 
sum(dfCountry$ratioL>1)/nrow(dfCountry)*100
sum(dfRegion$ratioL>1)/nrow(dfRegion)*100
sum(dfFarm$ratioL>1)/nrow(dfFarm)*100

# get average difference betten first and last period
1-mean(dfCountry[which(dfCountry$timePeriod==2008),"stability"])/mean(dfCountry[which(dfCountry$timePeriod==1968),"stability"])
1-mean(dfRegion[which(dfRegion$timePeriod==2008),"stability"])/mean(dfRegion[which(dfRegion$timePeriod==1978),"stability"])
1-mean(dfFarm[which(dfFarm$timePeriod==2008),"stability"])/mean(dfFarm[which(dfFarm$timePeriod==1998),"stability"])


# get differences between first and last time period
dfDifferentce <- merge(dfCountry[which(dfCountry$timePeriod==1968),c("Country","stability")],dfCountry[which(dfCountry$timePeriod==2008),c("Country","stability")],by="Country")
dfDifferentce$ratio <- dfDifferentce$stability.y/dfDifferentce$stability.x
range(dfDifferentce$ratio)

dfDifferentce <- merge(dfRegion[which(dfRegion$timePeriod==1978),c("Region","stability")],dfRegion[which(dfRegion$timePeriod==2008),c("Region","stability")],by="Region")
dfDifferentce$ratio <- dfDifferentce$stability.y/dfDifferentce$stability.x
range(dfDifferentce$ratio)

dfDifferentce <- merge(dfFarm[which(dfFarm$timePeriod==1998),c("Farm","stability")],dfFarm[which(dfFarm$timePeriod==2008),c("Farm","stability")],by="Farm")
dfDifferentce$ratio <- dfDifferentce$stability.y/dfDifferentce$stability.x
range(dfDifferentce$ratio)

# compare between scales
mean(dfCountry$stability)/mean(dfRegion$stability)-1
mean(dfCountry$stability)/mean(dfFarm$stability)-1

mean(dfCountry$stabilityOverall)/mean(dfCountry$stability)
mean(dfRegion$stabilityOverall)/mean(dfRegion$stability)
mean(dfFarm$stabilityOverall)/mean(dfFarm$stability)

# maximum contribution to large scale stability
dfCountry[order(dfCountry$ratioL,decreasing = T),c("Country","timePeriod","ratioL")]
dfRegion[order(dfRegion$ratioL,decreasing = T),c("Region","timePeriod","ratioL")]


############################################################################################
###################           FIGURES               ########################################
############################################################################################

##### Fig 1,3,5: maps of stability, small and large scale ratios

# national level
mapCountry <- readOGR("spatial/countries_global.shp")
mapCountry$Country <-  countrycode(mapCountry$Area, 'country.name', 'iso3c')
dfCountryAgg <- aggregate(cbind(stability,ratioS,ratioL)~Country,dfCountry,mean)

# regional level
mapRegion <- readOGR("spatial/regions_europe.shp")
mapRegion$Region <- mapRegion$NUTS_ID
dfRegionAgg <- aggregate(cbind(stability,ratioS,ratioL)~Region,dfRegion,mean)

# farm level
mapFarm <- readOGR("spatial/vg2500_rbz_bld.shp")
mapFarm$REGION_ID <- mapFarm$district
dfFarmAgg <- aggregate(cbind(stability,ratioS,ratioL)~Farm,dfFarm,mean)
dfFarmAggRegion <- aggregate(cbind(stability,ratioS,ratioL)~REGION_ID,dfFarm,mean)
dfFarmAggRegionTime <- aggregate(cbind(stability,ratioS,ratioL)~REGION_ID+timePeriod,dfFarm,mean)

## Fig 1: stability
a1 <- funMaps(dfCountryAgg,variable="stability",st_as_sf(mapCountry),"Country",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Stability","a",T)
b1 <- funMaps(dfRegionAgg,variable="stability",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"","b",F)
c1 <- funMaps(dfFarmAggRegion,variable="stability",st_as_sf(mapFarm),"REGION_ID",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"","c",F)

jpeg("results/Fig1.jpeg", width = 16.9,height = 16.9, units = 'cm',res = 600)
  grid.arrange(a1,b1,c1,
             layout_matrix = rbind(c(1, 1),
                                   c(2, 3)))
dev.off()

## Fig 3: ratio small scale
a3 <- funMaps(dfCountryAgg,variable="ratioS",st_as_sf(mapCountry),"Country",seq(0,2,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Ratio","a",T)
b3 <- funMaps(dfRegionAgg,variable="ratioS",st_as_sf(mapRegion),"Region",seq(0,2,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"","b",F)
c3 <- funMaps(dfFarmAggRegion,variable="ratioS",st_as_sf(mapFarm),"REGION_ID",seq(0,2,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"","c",F)

jpeg("results/Fig3.jpeg", width = 16.9,height = 16.9, units = 'cm',res = 300)
  grid.arrange(a3,b3,c3,
             layout_matrix = rbind(c(1, 1),
                                   c(2, 3)))
dev.off()

## Fig 5: ratio large scale
a5 <- funMaps(dfCountryAgg,variable="ratioL",st_as_sf(mapCountry),"Country",seq(0.85,1.15,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Ratio","a",T)
b5 <- funMaps(dfRegionAgg,variable="ratioL",st_as_sf(mapRegion),"Region",seq(0.85,1.15,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"","b",F)
c5 <- funMaps(dfFarmAggRegion,variable="ratioL",st_as_sf(mapFarm),"REGION_ID",seq(0.85,1.15,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"","c",F)

jpeg("results/Fig5.jpeg", width = 16.9,height = 16.9, units = 'cm',res = 300)
  grid.arrange(a5,b5,c5,
             layout_matrix = rbind(c(1, 1),
                                   c(2, 3)))
dev.off()



##### Part 2: regression results

## Fig 2: barplots of  stability model
a2 <- funEffect(modStabilityCountry,6,c("Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability"),"National",myColors[1],0.4,0.1,1,"Standardized regression coefficient",0.02,0.03)
b2 <- funEffect(modStabilityRegion,4,c("Diversity","sqrt(Fertilizer)","Precipitation instability"),"Regional",myColors[2],0.4,0.1,1,"",0.02,0.03)
c2 <- funEffect(modStabilityFarm,8,c("Diversity","sqrt(Fertilizer)","sqrt(Irigation)","log(Area harvested)","Temperature instability","Precipitation instability","Time"),"Farm",myColors[3],0.4,0.1,1,"",0.02,0.03)

# plot
jpeg("results/Fig2.jpeg", width = 16.9, height = 8, units = 'cm', res = 600)
  ggarrange(a2,b2,c2,
            labels = letters[1:3],font.label=list(size=8),
            ncol = 3, nrow = 1,align='h')
dev.off()

## Fig 4: barplots of  ratio small scale
a4 <- funEffect(modRatioSmallCountry,6,c("Diversity","sqrt(Fertilizer)","Temperature instability","Precipitation instability","Time"),"National",myColors[1],0.4,0.1,1,"Standardized regression coefficient",0.02,0.03)
b4 <- funEffect(modRatioSmallRegion,5,c("Diversity","sqrt(Fertilizer)","Precipitation instability","Time"),"Regional",myColors[2],0.4,0.1,1,"",0.02,0.03)
c4 <- funEffect(modRatioSmallFarm,7,c("Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time"),"Farm",myColors[3],0.4,0.1,1,"",0.02,0.03)

# plot
jpeg("results/Fig4.jpeg", width = 16.9, height = 8, units = 'cm', res = 600)
  ggarrange(a4,b4,c4,
          labels = letters[1:3],font.label=list(size=8),
          ncol = 3, nrow = 1,align='h')
dev.off()

## Fig 6: barplots of  ratio large scale
a6 <- funEffect(modRatioLargeCountry,5,c("Diversity","sqrt(Fertilizer)","log(Area harvested)","Temperature instability"),"National",myColors[1],0.01,0.01,2,"Standardized regression coefficient",0.0005,0.0006)
b6 <- funEffect(modRatioLargeRegion,2,c("sqrt(Fertilizer)"),"Regional",myColors[2],0.01,0.01,2,"",0.0005,0.0006)

# plot
jpeg("results/Fig6.jpeg", width = 8, height = 8, units = 'cm', res = 600)
ggarrange(a6,b6,
          labels = letters[1:3],font.label=list(size=8),
          ncol = 2, nrow = 1,align='h')
dev.off()


## Fig 7: summary barplots 

dfStability <- data.frame(mean=c(mean(dfCountry$stability),mean(dfRegion$stability),mean(dfFarm$stability)),
                          sd=c(sd(dfCountry$stability),sd(dfRegion$stability),sd(dfFarm$stability)),
                          level=c("National","Regional","Farm"))
dfStability$level <- factor(dfStability$level,levels=c("National","Regional","Farm"))
dfStability$lwr <- dfStability$mean-dfStability$sd
dfStability[which(dfStability$lwr<0),"lwr"] <- 0


dfRelativeStability <- data.frame(mean=c(mean(dfCountry$stabilityOverall),mean(dfRegion$stabilityOverall),mean(dfFarm$stabilityOverall)),
                          sd=c(sd(dfCountry$stabilityOverall),sd(dfRegion$stabilityOverall),sd(dfFarm$stabilityOverall)),
                          level=c("National","Regional","Farm"))
dfRelativeStability$level <- factor(dfRelativeStability$level,levels=c("National","Regional","Farm"))


a7 <- ggplot(data=dfStability, aes(x=level, y=mean)) +
  geom_bar(stat="identity", position=position_dodge(), fill=myColors)+
  geom_errorbar(aes(ymin=lwr, ymax=mean+sd), width=.1,
                position=position_dodge(.9)) +
  theme_classic() +  
  ylim(c(0,65))+
  xlab("") +
  ylab("Stability") +
  theme(axis.title.y=element_text(size=8)) +
  theme(axis.text.y = element_text(size=8))+
  geom_hline(yintercept=0,size=0)+
  theme(plot.margin = unit(c(0.2,0.3,-0.5,0.2), "cm"))+
  theme(legend.position = "none")+
  theme(plot.title = element_text(size=8))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=6))


b7 <- ggplot(data=dfRelativeStability, aes(x=level, y=mean)) +
  geom_bar(stat="identity", position=position_dodge(), fill=myColors)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1,
                position=position_dodge(.9)) +
  theme_classic() +  
  ylim(c(0,65))+
  xlab("") +
  ylab("Larger-scale stability") +
  theme(axis.title.y=element_text(size=8)) +
  theme(axis.text.y = element_text(size=8))+
  geom_hline(yintercept=0,size=0)+
  theme(plot.margin = unit(c(0.2,0.3,-0.5,0.2), "cm"))+
  theme(legend.position = "none")+
  theme(plot.title = element_text(size=8))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=6))

jpeg("results/Fig7.jpeg", width = 8, height = 8, units = 'cm', res = 600)
ggarrange(a7,b7,
          labels = letters[1:2],font.label=list(size=8),
          ncol = 2, nrow = 1,align='h')
dev.off()

# table contribution
dfCountryC <- dfCountry[order(dfCountry$ratioL,decreasing = T),c("Country","timePeriod","ratioL")]
dfCountryC <- dfCountryC[c(1:10,(nrow(dfCountryC)-10):(nrow(dfCountryC))),]
mean(dfCountry$ratioL)
sd(dfCountry$ratioL)

dfRegionC <- dfRegion[order(dfRegion$ratioL,decreasing = T),c("Region","timePeriod","ratioL")]
dfRegionC <- dfRegionC[c(1:10,(nrow(dfRegionC)-10):(nrow(dfRegionC))),]
mean(dfRegion$ratioL)
sd(dfRegion$ratioL)

dfTable2 <- cbind(dfCountryC,dfRegionC)
names(dfTable2) <- c("Country","Time","Yield stability contribution","Region","Time","Yield stability contribution")
dfTable2[,c(2,3,5,6)] <- round(dfTable2[,c(2,3,5,6)],2)
write.csv(dfTable2,"results/Table2.csv",row.names = F)

dfCountry[order(dfCountry$ratioS),c("Country","timePeriod","ratioS")][1:5,]


fig7c <- ggplot(data=dfStabilityContribution, aes(x=level, y=mean)) +
  geom_boxplot(stat="identity", position=position_dodge(), fill=myColors)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1,
                position=position_dodge(.9)) +
  theme_classic() +  
  xlab("") +
  ylab("Relative stability") +
  theme(axis.title.y=element_text(size=8)) +
  theme(axis.text.y = element_text(size=8))+
  geom_hline(yintercept=0,size=0)+
  theme(plot.margin = unit(c(0.2,0.3,-0.5,0.2), "cm"))+
  theme(legend.position = "none")+
  theme(plot.title = element_text(size=8))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=6))

mean(dfCountry$stabilityOverall)/mean(dfCountry$stability)
mean(dfCountry$ratioL)
mean(dfRegion$stabilityOverall)/mean(dfRegion$stability)
mean(dfFarm$stabilityOverall)/mean(dfFarm$stability)


####### supplementary figures

#### Fgs. S1-S3
# national level

a1s <- funMaps(dfCountry[which(dfCountry$timePeriod==1968),],variable="stability",st_as_sf(mapCountry),"Country",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Stability","",T)
b1s <- funMaps(dfCountry[which(dfCountry$timePeriod==1968),],variable="ratioS",st_as_sf(mapCountry),"Country",seq(0,2,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Ratio","",T)
c1s <- funMaps(dfCountry[which(dfCountry$timePeriod==1968),],variable="ratioL",st_as_sf(mapCountry),"Country",seq(0.85,1.15,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Ratio","",T)

d1s <- funMaps(dfCountry[which(dfCountry$timePeriod==1978),],variable="stability",st_as_sf(mapCountry),"Country",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Stability","",T)
e1s <- funMaps(dfCountry[which(dfCountry$timePeriod==1978),],variable="ratioS",st_as_sf(mapCountry),"Country",seq(0,2,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Ratio","",T)
f1s <- funMaps(dfCountry[which(dfCountry$timePeriod==1978),],variable="ratioL",st_as_sf(mapCountry),"Country",seq(0.85,1.15,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Ratio","",T)

g1s <- funMaps(dfCountry[which(dfCountry$timePeriod==1988),],variable="stability",st_as_sf(mapCountry),"Country",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Stability","",T)
h1s <- funMaps(dfCountry[which(dfCountry$timePeriod==1988),],variable="ratioS",st_as_sf(mapCountry),"Country",seq(0,2,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Ratio","",T)
i1s <- funMaps(dfCountry[which(dfCountry$timePeriod==1988),],variable="ratioL",st_as_sf(mapCountry),"Country",seq(0.85,1.15,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Ratio","",T)

j1s <- funMaps(dfCountry[which(dfCountry$timePeriod==1998),],variable="stability",st_as_sf(mapCountry),"Country",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Stability","",T)
k1s <- funMaps(dfCountry[which(dfCountry$timePeriod==1998),],variable="ratioS",st_as_sf(mapCountry),"Country",seq(0,2,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Ratio","",T)
l1s <- funMaps(dfCountry[which(dfCountry$timePeriod==1998),],variable="ratioL",st_as_sf(mapCountry),"Country",seq(0.85,1.15,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Ratio","",T)

m1s <- funMaps(dfCountry[which(dfCountry$timePeriod==2008),],variable="stability",st_as_sf(mapCountry),"Country",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Stability","",T)
n1s <- funMaps(dfCountry[which(dfCountry$timePeriod==2008),],variable="ratioS",st_as_sf(mapCountry),"Country",seq(0,2,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Ratio","",T)
o1s <- funMaps(dfCountry[which(dfCountry$timePeriod==2008),],variable="ratioL",st_as_sf(mapCountry),"Country",seq(0.85,1.15,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Ratio","",T)

jpeg("results/FigS1.jpeg", width = 30,height = 20, units = 'cm',res = 600)
  ggarrange(a1s,b1s,c1s,d1s,e1s,f1s,g1s,h1s,i1s,j1s,k1s,l1s,m1s,n1s,o1s,
          labels = letters[1:15],font.label=list(size=8),
          ncol = 3, nrow = 5)
dev.off()


# regional level
a2s <- funMaps(dfRegion[which(dfRegion$timePeriod==1978),],variable="stability",st_as_sf(mapRegion),"Region",seq(0,40,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Stability","",T)
b2s <- funMaps(dfRegion[which(dfRegion$timePeriod==1978),],variable="ratioS",st_as_sf(mapRegion),"Region",seq(0,2,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Ratio","",T)
c2s <- funMaps(dfRegion[which(dfRegion$timePeriod==1978),],variable="ratioL",st_as_sf(mapRegion),"Region",seq(0.85,1.15,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Ratio","",T)

d2s <- funMaps(dfRegion[which(dfRegion$timePeriod==1988),],variable="stability",st_as_sf(mapRegion),"Region",seq(0,40,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Stability","",T)
e2s <- funMaps(dfRegion[which(dfRegion$timePeriod==1988),],variable="ratioS",st_as_sf(mapRegion),"Region",seq(0,2,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Ratio","",T)
f2s <- funMaps(dfRegion[which(dfRegion$timePeriod==1988),],variable="ratioL",st_as_sf(mapRegion),"Region",seq(0.85,1.15,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Ratio","",T)

g2s <- funMaps(dfRegion[which(dfRegion$timePeriod==1998),],variable="stability",st_as_sf(mapRegion),"Region",seq(0,40,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Stability","",T)
h2s <- funMaps(dfRegion[which(dfRegion$timePeriod==1998),],variable="ratioS",st_as_sf(mapRegion),"Region",seq(0,2,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Ratio","",T)
i2s <- funMaps(dfRegion[which(dfRegion$timePeriod==1998),],variable="ratioL",st_as_sf(mapRegion),"Region",seq(0.85,1.15,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Ratio","",T)

j2s <- funMaps(dfRegion[which(dfRegion$timePeriod==2008),],variable="stability",st_as_sf(mapRegion),"Region",seq(0,40,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Stability","",T)
k2s <- funMaps(dfRegion[which(dfRegion$timePeriod==2008),],variable="ratioS",st_as_sf(mapRegion),"Region",seq(0,2,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Ratio","",T)
l2s <- funMaps(dfRegion[which(dfRegion$timePeriod==2008),],variable="ratioL",st_as_sf(mapRegion),"Region",seq(0.85,1.15,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Ratio","",T)

jpeg("results/FigS2.jpeg", width = 16.9,height = 20, units = 'cm',res = 600)

ggarrange(a2s,b2s,c2s,d2s,e2s,f2s,g2s,h2s,i2s,j2s,k2s,l2s,
          labels = letters[1:12],font.label=list(size=8),
          ncol = 3, nrow = 4)

dev.off()

# farm level
a3s <- funMaps(dfFarmAggRegionTime[which(dfFarmAggRegionTime$timePeriod==1998),],variable="stability",st_as_sf(mapFarm),"REGION_ID",seq(0,20,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Stability","",T)
b3s <- funMaps(dfFarmAggRegionTime[which(dfFarmAggRegionTime$timePeriod==1998),],variable="ratioS",st_as_sf(mapFarm),"REGION_ID",seq(0.2,1.2,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Ratio","",T)
c3s <- funMaps(dfFarmAggRegionTime[which(dfFarmAggRegionTime$timePeriod==1998),],variable="ratioL",st_as_sf(mapFarm),"REGION_ID",seq(0.99,1.01,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Ratio","",T)

d3s <- funMaps(dfFarmAggRegionTime[which(dfFarmAggRegionTime$timePeriod==2008),],variable="stability",st_as_sf(mapFarm),"REGION_ID",seq(0,20,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Stability","",T)
e3s <- funMaps(dfFarmAggRegionTime[which(dfFarmAggRegionTime$timePeriod==2008),],variable="ratioS",st_as_sf(mapFarm),"REGION_ID",seq(0.2,1.2,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Ratio","",T)
f3s <- funMaps(dfFarmAggRegionTime[which(dfFarmAggRegionTime$timePeriod==2008),],variable="ratioL",st_as_sf(mapFarm),"REGION_ID",seq(0.99,1.01,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Ratio","",T)

jpeg("results/FigS3.jpeg", width = 16.9,height = 10, units = 'cm',res = 600)

  ggarrange(a3s,b3s,c3s,d3s,e3s,f3s,
          labels = letters[1:6],font.label=list(size=8),
          ncol = 3, nrow = 2)

dev.off()




#### Fgs. S4-S6: # response curves (one figure for each level of organization)
# national level

empty <- ggplot() + theme_void()

dfPredictCountry <- data.frame(diversity=rep(0,1000),irrigation=0,fertilizer=0,instabilityTemp=0,instabilityPrec=0,timePeriod=0,areaHarvested=0)
dfPredictRegion <- data.frame(diversity=rep(0,1000),irrigation=0,fertilizer=0,instabilityTemp=0,instabilityPrec=0,timePeriod=0,areHarvested=0)
dfPredictFarm <- data.frame(diversity=rep(0,1000),irrigation=0,fertilizer=0,instabilityTemp=0,instabilityPrec=0,timePeriod=0,areaHarvested=0)

a4s <- funPredRange(predictor="diversity",      dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,dfOriginal=dfCountry,modS=modStabilityCountry,transResponse = "log",trans="",xlabel="Diversity",ylabel=expression(paste("Yield stability (",mu,"/",sigma,")")),0,100,"gray30")
b4s <- funPredRange(predictor="fertilizer",     dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,dfOriginal=dfCountry,modS=modStabilityCountry,transResponse = "log",trans="sqrt",xlabel="Fertilizer (t/ha)",ylabel="",0,100,"gray30")
c4s <- funPredRange(predictor="irrigation",     dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,dfOriginal=dfCountry,modS=modStabilityCountry,transResponse = "log",trans="sqrt",xlabel="Irrigation (%)",ylabel="",0,100,"gray30")
d4s <- funPredRange(predictor="instabilityTemp",dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,dfOriginal=dfCountry,modS=modStabilityCountry,transResponse = "log",trans="",xlabel=expression(paste("Temperature instability (-(",mu,"/",sigma,"))")),ylabel="",0,100,"gray30")
e4s <- funPredRange(predictor="instabilityPrec",dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,dfOriginal=dfCountry,modS=modStabilityCountry,transResponse = "log",trans="",xlabel=expression(paste("Precipitation instability (-(",mu,"/",sigma,"))")),ylabel="",0,100,"gray30")

f4s <- funPredRange(predictor="diversity",      dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,dfOriginal=dfCountry,modS=modRatioSmallCountry,transResponse = "log",trans="",xlabel="Diversity",ylabel=expression(paste("Relative yield stability")),0,2,"gray30")
g4s <- funPredRange(predictor="fertilizer",     dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,dfOriginal=dfCountry,modS=modRatioSmallCountry,transResponse = "log",trans="sqrt",xlabel="Fertilizer (t/ha)",ylabel="",0,2,"gray30")
h4s <- funPredRange(predictor="instabilityTemp",dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,dfOriginal=dfCountry,modS=modRatioSmallCountry,transResponse = "log",trans="",xlabel=expression(paste("Temperature instability (-(",mu,"/",sigma,"))")),ylabel="",0,2,"gray30")
i4s <- funPredRange(predictor="instabilityPrec",dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,dfOriginal=dfCountry,modS=modRatioSmallCountry,transResponse = "log",trans="",xlabel=expression(paste("Precipitation instability (-(",mu,"/",sigma,"))")),ylabel="",0,2,"gray30")
j4s <- funPredRange(predictor="timePeriod",dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,dfOriginal=dfCountry,modS=modRatioSmallCountry,transResponse = "log",trans="",xlabel="Time",ylabel="",0,2,"gray30")

k4s <- funPredRange(predictor="diversity",      dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,dfOriginal=dfCountry,modS=modRatioLargeCountry,transResponse = "",trans="",xlabel="Diversity",ylabel=expression(paste("Yield stability contribution (",mu,"/",sigma,")")),0.9,1.1,"gray30")
l4s <- funPredRange(predictor="fertilizer",      dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,dfOriginal=dfCountry,modS=modRatioLargeCountry,transResponse = "",trans="log",xlabel="Fertilizer (t/ha)",ylabel="",0.9,1.1,"gray30")
m4s <- funPredRange(predictor="areaHarvested",     dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,dfOriginal=dfCountry,modS=modRatioLargeCountry,transResponse = "",trans="log",xlabel="Area harvested (Mio ha.)",ylabel="",0.9,1.1,"gray30")
n4s <- funPredRange(predictor="instabilityTemp",dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,dfOriginal=dfCountry,modS=modRatioLargeCountry,transResponse = "",trans="",xlabel=expression(paste("Temperature instability (-(",mu,"/",sigma,"))")),ylabel="",0.9,1.1,"gray30")

jpeg("results/FigS4.jpeg", width = 30*(5/7),height = (3/5)*30*(5/7), units = 'cm',res = 600)

  ggarrange(a4s,b4s,c4s,d4s,e4s,
          f4s,g4s,h4s,i4s,j4s,
          k4s,l4s,m4s,n4s,empty,
          # labels = c(letters[1:14],"",letters[10:12],"",""),font.label=list(size=8),
          labels = letters[1:14],font.label=list(size=8),
          ncol = 5, nrow = 3)

dev.off()


# regional level
a5s <- funPredRange(predictor="diversity",      dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,dfOriginal=dfRegion,modS=modStabilityRegion,transResponse = "log",trans="",xlabel="Diversity",ylabel=expression(paste("Yield stability (",mu,"/",sigma,")")),0,25,"gray30")
b5s <- funPredRange(predictor="fertilizer",     dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,dfOriginal=dfRegion,modS=modStabilityRegion,transResponse = "log",trans="sqrt",xlabel="Fertilizer (t/ha)",ylabel="",0,25,"gray30")
c5s <- funPredRange(predictor="instabilityPrec",dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,dfOriginal=dfRegion,modS=modStabilityRegion,transResponse = "log",trans="",xlabel=expression(paste("Precipitation instability (-(",mu,"/",sigma,"))")),ylabel="",0,25,"gray30")

d5s <- funPredRange(predictor="diversity",      dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,dfOriginal=dfRegion,modS=modRatioSmallRegion,transResponse = "log",trans="",xlabel="Diversity",ylabel=expression(paste("Relative yield stability")),0,2,"gray30")
e5s <- funPredRange(predictor="fertilizer",     dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,dfOriginal=dfRegion,modS=modRatioSmallRegion,transResponse = "log",trans="sqrt",xlabel="Fertilizer (t/ha)",ylabel="",0,2,"gray30")
f5s <- funPredRange(predictor="instabilityPrec",dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,dfOriginal=dfRegion,modS=modRatioSmallRegion,transResponse = "log",trans="",xlabel=expression(paste("Precipitation instability (-(",mu,"/",sigma,"))")),ylabel="",0,2,"gray30")
g5s <- funPredRange(predictor="timePeriod",dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,dfOriginal=dfRegion,modS=modRatioSmallRegion,transResponse = "log",trans="",xlabel="Time",ylabel="",0,2,"gray30")

h5s <- funPredRange(predictor="fertilizer",      dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,dfOriginal=dfRegion,modS=modRatioLargeRegion,transResponse = "",trans="log",xlabel="Fertilizer (t/ha)",ylabel="Yield stability contribution",0.9,1.1,"gray30")

jpeg("results/FigS5.jpeg", width = 30*(4/7),height = (3/4)*30*(4/7), units = 'cm',res = 600)
  ggarrange(a5s,b5s,c5s,empty,
            d5s,e5s,f5s,g5s,
            h5s,
            labels = c(letters[1:3],"",letters[4:8]),font.label=list(size=8),
          ncol = 4, nrow = 3)
dev.off()

# farm level
a6s <- funPredRange(predictor="diversity",      dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfTransFarm,dfOriginal=dfFarm,modS=modStabilityFarm,transResponse = "log",trans="",xlabel="Diversity",ylabel=expression(paste("Yield stability (",mu,"/",sigma,")")),0,25,"gray30")
b6s <- funPredRange(predictor="fertilizer",     dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfTransFarm,dfOriginal=dfFarm,modS=modStabilityFarm,transResponse = "log",trans="sqrt",xlabel="Fertilizer (t/ha)",ylabel="",0,25,"gray30")
c6s <- funPredRange(predictor="irrigation",     dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfTransFarm,dfOriginal=dfFarm,modS=modStabilityFarm,transResponse = "log",trans="sqrt",xlabel="Irrigation (%)",ylabel="",0,25,"gray30")
d6s <- funPredRange(predictor="areaHarvested",     dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfTransFarm,dfOriginal=dfFarm,modS=modStabilityFarm,transResponse = "log",trans="log",xlabel="Area harvested (ha)",ylabel="",0,25,"gray30")
e6s <- funPredRange(predictor="instabilityTemp",dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfTransFarm,dfOriginal=dfFarm,modS=modStabilityFarm,transResponse = "log",trans="",xlabel=expression(paste("Temperature instability (-(",mu,"/",sigma,"))")),ylabel="",0,25,"gray30")
f6s <- funPredRange(predictor="instabilityPrec",dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfTransFarm,dfOriginal=dfFarm,modS=modStabilityFarm,transResponse = "log",trans="",xlabel=expression(paste("Precipitation instability (-(",mu,"/",sigma,"))")),ylabel="",0,25,"gray30")
g6s <- funPredRange(predictor="timePeriod",dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfTransFarm,dfOriginal=dfFarm,modS=modStabilityFarm,transResponse = "log",trans="",xlabel="Time",ylabel="",0,25,"gray30")


h6s <- funPredRange(predictor="diversity",      dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfTransFarm,dfOriginal=dfFarm,modS=modRatioSmallFarm,transResponse = "log",trans="",xlabel="Diversity",ylabel=expression(paste("Relative yield stability")),0,2,"gray30")
i6s <- funPredRange(predictor="fertilizer",     dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfTransFarm,dfOriginal=dfFarm,modS=modRatioSmallFarm,transResponse = "log",trans="sqrt",xlabel="Fertilizer (t/ha)",ylabel="",0,2,"gray30")
j6s <- funPredRange(predictor="irrigation",     dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfTransFarm,dfOriginal=dfFarm,modS=modRatioSmallFarm,transResponse = "log",trans="sqrt",xlabel="Irrigation (%)",ylabel="",0,2,"gray30")
k6s <- funPredRange(predictor="instabilityTemp",dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfTransFarm,dfOriginal=dfFarm,modS=modRatioSmallFarm,transResponse = "log",trans="",xlabel=expression(paste("Temperature instability (-(",mu,"/",sigma,"))")),ylabel="",0,2,"gray30")
l6s <- funPredRange(predictor="instabilityPrec",dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfTransFarm,dfOriginal=dfFarm,modS=modRatioSmallFarm,transResponse = "log",trans="",xlabel=expression(paste("Precipitation instability (-(",mu,"/",sigma,"))")),ylabel="",0,2,"gray30")
m6s <- funPredRange(predictor="timePeriod",dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfTransFarm,dfOriginal=dfFarm,modS=modRatioSmallFarm,transResponse = "log",trans="",xlabel="Time",ylabel="",0,2,"gray30")

jpeg("results/FigS6.jpeg", width = 30,height = (2/7)*30, units = 'cm',res = 600)
  ggarrange(a6s,b6s,c6s,d6s,e6s,f6s,g6s,
          h6s,i6s,j6s,k6s,l6s,m6s,
          # labels = c(letters[1:14],"",letters[10:12],"",""),font.label=list(size=8),
          labels = letters[1:13],font.label=list(size=8),
          ncol = 7, nrow = 2)
dev.off()




# regression tabes (includig transformations)

TableS1a <- funTables(modStabilityCountry,modStabilityRegion,modStabilityFarm,2, 
                      c("(Intercept)","Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability"),
                      c("(Intercept)","Diversity","sqrt(Fertilizer)","Precipitation instability"),
                      c("(Intercept)","Diversity","sqrt(Fertilizer)","sqrt(Irigation)","log(Area harvested)","Temperature instability","Precipitation instability","Time"))

TableS1a <- TableS1a[c(1,3,7:8,4,9,5,10,2,6),]

TableS1b <- funTables(modRatioSmallCountry,modRatioSmallRegion,modRatioSmallFarm,2, 
                      c("(Intercept)","Diversity","sqrt(Fertilizer)","Temperature instability","Precipitation instability","Time"),
                      c("(Intercept)","Diversity","sqrt(Fertilizer)","Precipitation instability","Time"),
                      c("(Intercept)","Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time"))

TableS1b <- TableS1b[c(1,3,6:8,4,9,2,5),]

TableS1c <- funTables(modRatioLargeCountry,modRatioLargeRegion,modRatioLargeFarm,2, 
                      c("(Intercept)","Diversity","sqrt(Fertilizer)","log(Area harvested)","Temperature instability"),
                      c("(Intercept)","sqrt(Fertilizer)"),
                      c("(Intercept)"))

TableS1c <- TableS1c[c(1,3,6,4,7,2,5),]

write.csv(rbind(TableS1a,TableS1b,TableS1c),"results/TableS1.csv",row.names = F)






rm(list=ls())





