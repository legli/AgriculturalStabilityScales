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

source("scripts/functionsAnalysesFinal.R")



############################################################################################
###################              DATA               ########################################
############################################################################################

###### Country level
dfCountry <- read.csv("datasetsDerived/dataFinal_global.csv")
hist(dfCountry$stability) 
hist(dfCountry$stabilityOverall)
# remove stability outliers (very skewed)
dfCountry <- dfCountry[-which(dfCountry$stability%in%boxplot.stats(dfCountry$stability)$out),]


###### Regional level
dfRegion <- read.csv("datasetsDerived/dataFinal_europe.csv")
dfRegion <- merge(dfRegion,dfCountry[,c("Country","timePeriod","fertilizer","irrigation")],by=c("Country","timePeriod"))
hist(dfRegion$stability)
hist(dfRegion$stabilityOverall)
# remove stability outliers (to be consistent with other levels)
dfRegion <- dfRegion[-which(dfRegion$stability%in%boxplot.stats(dfRegion$stability)$out),] 

###### Farm level
dfFarm <- read.csv("P:/dataFinal_farmlevel.csv")
hist(dfFarm$stability)
hist(dfFarm$stabilityOverall)
# remove stability outliers (very skewed)
dfFarm <- dfFarm[-which(dfFarm$stability%in%boxplot.stats(dfFarm$stability)$out),]




############################################################################################
###################          ANALYSES               ########################################
############################################################################################

##################### Regression 
###### Country level
### preparation
# get extremes
dfCountry$slopeStability <- dfCountry$stabilityOverall/dfCountry$stability
sum(dfCountry$slopeStability<1)/nrow(dfCountry)
dfCountry$slopeYield <- dfCountry$yieldOverall/dfCountry$yield

# check distribution of response variables
fitdist(dfCountry$slopeStability,"gamma")$aic - fitdist(dfCountry$slopeStability,"lnorm")$aic #log-normally distributed
fitdist(dfCountry$slopeYield,"gamma")$aic - fitdist(dfCountry$slopeYield,"lnorm")$aic #log-normally distributed
hist(log(dfCountry$slopeStability))
hist(log(dfCountry$slopeYield))

### regression analyses
## transformations
dfLogCountry=with(dfCountry,data.frame(Country,
                                       slopeStability = log(slopeStability),
                                       slopeYield = log(slopeYield),
                                       diversity, 
                                       irrigation=sqrt(irrigation),
                                       fertilizer=sqrt(fertilizer),
                                       instabilityTemp,instabilityPrec,
                                       timePeriod
))

# scale predictors for standardized regression
dfPredictorsCountry=sapply(dfLogCountry[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterCountry=data.frame(Country=dfLogCountry[,1],slopeStability=dfLogCountry[,2],slopeYield=dfLogCountry[,3],dfPredictorsCountry)
head(dfCenterCountry)

## check colinearity
cor(dfCenterCountry[,2:9],method='s') 

## regression models
modStabilityCountry <- lm(slopeStability~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterCountry)
summary(modStabilityCountry)
modStabilityCountryRaw <- lm(slopeStability~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfLogCountry)
summary(modStabilityCountryRaw)
vif(modStabilityCountry)

modYieldCountry <- lm(slopeYield~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterCountry)
summary(modYieldCountry)
modYieldCountryRaw <- lm(slopeYield~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfLogCountry)
summary(modYieldCountryRaw)
vif(modYieldCountry)


###### European level
### preparation
# get extremes
dfRegion$slopeStability <- dfRegion$stabilityOverall/dfRegion$stability
sum(dfRegion$slopeStability<1)/nrow(dfRegion)
dfRegion$slopeYield <- dfRegion$yieldOverall/dfRegion$yield

# check distribution of response variables
fitdist(dfRegion$slopeStability,"gamma")$aic - fitdist(dfRegion$slopeStability,"lnorm")$aic #log-normally distributed
fitdist(dfRegion$slopeYield,"gamma")$aic - fitdist(dfRegion$slopeYield,"lnorm")$aic #log-normally distributed
hist(log(dfRegion$slopeStability))
hist(log(dfRegion$slopeYield))

### regression analyses
## transformations
dfLogRegion=with(dfRegion,data.frame(Region,
                                     slopeStability = log(slopeStability),
                                     slopeYield = log(slopeYield),
                                     diversity, 
                                     irrigation=sqrt(irrigation),
                                     fertilizer=sqrt(fertilizer),
                                     instabilityTemp,instabilityPrec,
                                     timePeriod
))

# scale predictors for standardized regression
dfPredictorsRegion=sapply(dfLogRegion[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterRegion=data.frame(Region=dfLogRegion[,1],slopeStability=dfLogRegion[,2],slopeYield=dfLogRegion[,3],dfPredictorsRegion)
head(dfCenterRegion)

## check colinearity
cor(dfCenterRegion[,2:9],method='s') 

## regression models
modStabilityRegion <- lm(slopeStability~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterRegion)
summary(modStabilityRegion)
modStabilityRegionRaw <- lm(slopeStability~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfLogRegion)
summary(modStabilityRegionRaw)
vif(modStabilityRegion)

modYieldRegion <- lm(slopeYield~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterRegion)
summary(modYieldRegion)
modYieldRegionRaw <- lm(slopeYield~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfLogRegion)
summary(modYieldRegionRaw)
vif(modYieldRegion)

###### Farm level
### preparation

# get extremes
dfFarm$slopeStability <- dfFarm$stabilityOverall/dfFarm$stability
sum(dfFarm$slopeStability<1)/nrow(dfFarm)
dfFarm$slopeYield <- dfFarm$yieldOverall/dfFarm$yield

# check distribution of response variables
fitdist(dfFarm$slopeStability,"gamma")$aic-fitdist(dfFarm$slopeStability,"lnorm")$aic #log-normally distributed
fitdist(dfFarm$slopeYield,"gamma")$aic-fitdist(dfFarm$slopeYield,"lnorm")$aic #log-normally distributed
hist(log(dfFarm$slopeStability))
hist(log(dfFarm$slopeYield))


### regression analyses
## transformations
dfLogFarm=with(dfFarm,data.frame(Farm,
                                 slopeStability = log(slopeStability),
                                 slopeYield = log(slopeYield),
                                 diversity, 
                                 irrigation=sqrt(irrigation),
                                 fertilizer=sqrt(fertilizer),
                                 instabilityTemp,instabilityPrec,
                                 timePeriod
))

# scale predictors for standardized regression
dfPredictorsFarm=sapply(dfLogFarm[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterFarm=data.frame(Farm=dfLogFarm[,1],slopeStability=dfLogFarm[,2],slopeYield=dfLogFarm[,3],dfPredictorsFarm)
head(dfCenterFarm)

## check colinearity
cor(dfCenterFarm[,2:9],method='s') # remove instabilityPrec and time Period

## regression models
modStabilityFarm <- lm(slopeStability~diversity+fertilizer+irrigation+instabilityTemp,data=dfCenterFarm)
summary(modStabilityFarm)
modStabilityFarmRaw <- lm(slopeStability~diversity+fertilizer+irrigation+instabilityTemp,data=dfLogFarm)
summary(modStabilityFarmRaw)
vif(modStabilityFarm)

modYieldFarm <- lm(slopeYield~diversity+fertilizer+irrigation+instabilityTemp,data=dfCenterFarm)
summary(modYieldFarm)
modYieldFarmRaw <- lm(slopeYield~diversity+fertilizer+irrigation+instabilityTemp,data=dfLogFarm)
summary(modYieldFarmRaw)
vif(modYieldFarm)



############################################################################################
###################           FIGURES               ########################################
############################################################################################

##### Fig 1: maps of small and large scale stabilities/yields

# legend
g.legend <- ggplot(grd, aes(dim1,dim2,fill=factor(1:9)))+
  geom_tile()+
  scale_fill_manual(values=grd$color)+
  theme_void()+
  theme(legend.position="none",axis.title=element_text(size=5),
        panel.background=element_blank(),plot.margin=margin(t=10,b=10,l=10))+
  theme(axis.title=element_text(color="black",size=8),
        axis.title.y = element_text(angle = 90))+
  labs(x="Stability level",y="Stability overall") 

vp<-viewport(width=0.2,height=0.2,x=0.12,y=0.6)

# national level
mapCountry <- readOGR("spatial/countries_global.shp")
mapCountry$Country <-  countrycode(mapCountry$Area, 'country.name', 'iso3c')
dfCountryAgg <- aggregate(cbind(stabilityOverall,stability,yieldOverall,yield)~Country,dfCountry,mean)

# regional level
mapRegion <- readOGR("spatial/regions_europe.shp")
mapRegion$Region <- mapRegion$NUTS_ID
dfRegionAgg <- aggregate(cbind(stabilityOverall,stability,yieldOverall,yield)~Region,dfRegion,mean)

# farm level
mapFarm <- readOGR("spatial/vg2500_rbz_bld.shp")
mapFarm$REGION_ID <- mapFarm$district
dfFarmAgg <- aggregate(cbind(stabilityOverall,stability,yieldOverall,yield)~REGION_ID,dfFarm,mean)


a1 <- funFig2(dfCountryAgg,variable="stability",st_as_sf(mapCountry),"Country","a")
b1 <- funFig2(dfRegionAgg,"stability",st_as_sf(mapRegion),"Region","b")
c1 <- funFig2(dfFarmAgg,"stability",st_as_sf(mapFarm),"REGION_ID","c")
as1 <- funFig2(dfCountryAgg,variable="yield",st_as_sf(mapCountry),"Country","a")
bs1 <- funFig2(dfRegionAgg,"yield",st_as_sf(mapRegion),"Region","b")
cs1 <- funFig2(dfFarmAgg,"yield",st_as_sf(mapFarm),"REGION_ID","c")

jpeg("results/Fig1.jpeg", width = 16.9,height = 16.9, units = 'cm',res = 300)

grid.arrange(a1,b1,c1,
          layout_matrix = rbind(c(1, 1),
                                c(2, 3)))
print(g.legend,vp=vp)
dev.off()

jpeg("results/FigS1.jpeg", width = 16.9,height = 16.9, units = 'cm',res = 300)

grid.arrange(as1,bs1,cs1,
             layout_matrix = rbind(c(1, 1),
                                   c(2, 3)))
print(g.legend,vp=vp)
dev.off()

##### Fig 2
# barplot of  stability model
a2 <- funCombinePlot(modStabilityCountry,"all",T,"Country",myColors[1],0.4,"Yield stability ratio")
b2 <- funCombinePlot(modStabilityRegion,"all",T,"Region",myColors[2],0.4,"")
c2 <- funCombinePlot(modStabilityFarm,"farm",T,"Farm",myColors[3],0.4,"")
as2 <- funCombinePlot(modYieldCountry,"all",T,"Country",myColors[1],0.6,"Yield ratio")
bs2 <- funCombinePlot(modYieldRegion,"all",T,"Region",myColors[2],0.6,"")
cs2 <- funCombinePlot(modYieldFarm,"farm",T,"Farm",myColors[3],0.6,"")

# plot
jpeg("results/Fig2.jpeg", width = 16.9, height = 16.9*0.3, units = 'cm', res = 600)
  ggarrange(a2,b2,c2,
            labels = letters[1:3],font.label=list(size=8),
            ncol = 3, nrow = 1)
dev.off()
jpeg("results/FigS2.jpeg", width = 16.9, height = 16.9*0.3, units = 'cm', res = 600)
  ggarrange(as2,bs2,cs2,
            labels = letters[1:3],font.label=list(size=8),
            ncol = 3, nrow = 1)
dev.off()


##### Fig 3: predict if scale effect can be buffered by management (yield stability)

## get data of most recent time interval
dfPredictCountry <- dfCenterCountry[which(dfCenterCountry$timePeriod==max(dfCenterCountry$timePeriod)),c("Country","diversity","irrigation","fertilizer","instabilityTemp","instabilityPrec","timePeriod")]
dfPredictRegion <- dfCenterRegion[which(dfCenterRegion$timePeriod==max(dfCenterRegion$timePeriod)),c("Region","diversity","irrigation","fertilizer","instabilityTemp","instabilityPrec","timePeriod")]

## model slope stability
dfPredictStabilityCountry <- funPredictChange("Country",dfPredictCountry,modStabilityCountry,0.75,"Country")
dfPredictStabilityRegion <- funPredictChange("Region",dfPredictRegion,modStabilityRegion,0.75,"Region")

## combine
dfPredictStabilityAll <- rbind(dfPredictStabilityCountry,dfPredictStabilityRegion)
head(dfPredictStabilityAll)
dfPredictStabilityAll$plotLevel <- paste0(dfPredictStabilityAll$plot,dfPredictStabilityAll$level)
dfPredictStabilityAll$plotLevel <- factor(dfPredictStabilityAll$plotLevel,levels=c("DiversityCountry","FertilizerCountry","IrrigationCountry","AllCountry","DiversityRegion","FertilizerRegion","IrrigationRegion","AllRegion"))

levelLabs <- c("a     Diversity","b     Fertilizer","c     Irrigation","d     All","e","f","g","h  Diversity/Fertilizer")
names(levelLabs) <- c("DiversityCountry","FertilizerCountry","IrrigationCountry","AllCountry","DiversityRegion","FertilizerRegion","IrrigationRegion","AllRegion")

jpeg("results/Fig3.jpeg", width = 16.9, height = 16.9*0.5, units = 'cm', res = 600)
  ggplot(dfPredictStabilityAll, aes(x=slope, fill=scenario)) +
    geom_histogram(alpha=0.5, position="identity",breaks = seq(0,7.5,0.25))+
    facet_wrap(~plotLevel,labeller = labeller(plotLevel=levelLabs),nrow=2,ncol=4)+
    theme_void() +
    theme(axis.title=element_text(size=8)) +
    theme(axis.text = element_text(size=8))+
    theme(axis.title.y = element_text(angle=90))+
    geom_hline(yintercept=0,size=0)+
    geom_vline(xintercept=0,size=0)+
    geom_vline(xintercept=1,size=0,linetype="dotted")+
    theme(strip.text = element_text(hjust = 0.1))+
    labs(y= "Count", x = "Slope stability")+
    scale_fill_discrete(name = "")+
    ylim(0,80)
dev.off()


##### Fig S3: predict if scale effect can be buffered by management (yield)

## model slope Yield
dfPredictYieldCountry <- funPredictChange("Country",dfPredictCountry,modYieldCountry,0.75,"Country")
dfPredictYieldRegion <- funPredictChange("Region",dfPredictRegion,modYieldRegion,0.75,"Region")

## combine
dfPredictYieldAll <- rbind(dfPredictYieldCountry,dfPredictYieldRegion)
head(dfPredictYieldAll)
dfPredictYieldAll$plotLevel <- paste0(dfPredictYieldAll$plot,dfPredictYieldAll$level)
dfPredictYieldAll$plotLevel <- factor(dfPredictYieldAll$plotLevel,levels=c("DiversityCountry","FertilizerCountry","IrrigationCountry","AllCountry","DiversityRegion","FertilizerRegion","IrrigationRegion","AllRegion"))

levelLabs <- c("a     Diversity","b     Fertilizer","c     Irrigation","d     All","e","f","g","h  Diversity/Fertilizer")
names(levelLabs) <- c("DiversityCountry","FertilizerCountry","IrrigationCountry","AllCountry","DiversityRegion","FertilizerRegion","IrrigationRegion","AllRegion")

jpeg("results/FigS3.jpeg", width = 16.9, height = 16.9*0.5, units = 'cm', res = 600)
ggplot(dfPredictYieldAll, aes(x=slope, fill=scenario)) +
  geom_histogram(alpha=0.5, position="identity",breaks = seq(0,7.5,0.25))+
  facet_wrap(~plotLevel,labeller = labeller(plotLevel=levelLabs),nrow=2,ncol=4)+
  theme_void() +
  theme(axis.title=element_text(size=8)) +
  theme(axis.text = element_text(size=8))+
  theme(axis.title.y = element_text(angle=90))+
  geom_hline(yintercept=0,size=0)+
  geom_vline(xintercept=0,size=0)+
  geom_vline(xintercept=1,size=0,linetype="dotted")+
  theme(strip.text = element_text(hjust = 0.1))+
  labs(y= "Count", x = "Slope yield")+
  scale_fill_discrete(name = "")+
  ylim(0,120)
dev.off()



#### Fig S4-6: continious effect of predictors
# country 
dfPredictCountry <- data.frame(diversity=rep(0,1000),irrigation=0,fertilizer=0,instabilityTemp=0,instabilityPrec=0,timePeriod=0)
as4 <- funPredRange(predictor="diversity",dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfLogCountry,dfOriginal=dfCountry,modS=modStabilityCountry,modY=modYieldCountry,trans="",xlabel="Diversity",ylabel="Ratio",posX=0.85,posY=0.8,tStability="***",tYield="")
bs4 <- funPredRange(predictor="fertilizer",dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfLogCountry,dfOriginal=dfCountry,modS=modStabilityCountry,modY=modYieldCountry,trans="sqrt",xlabel="N use intensity (t/ha)",ylabel="",posX=-9999,posY=-9999,tStability="***",tYield="***")
cs4 <- funPredRange(predictor="irrigation",dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfLogCountry,dfOriginal=dfCountry,modS=modStabilityCountry,modY=modYieldCountry,trans="sqrt",xlabel="Irrigation (%)",ylabel="",posX=-9999,posY=-9999,tStability=" *",tYield="")
ds4 <- funPredRange(predictor="instabilityTemp",dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfLogCountry,dfOriginal=dfCountry,modS=modStabilityCountry,modY=modYieldCountry,trans="",xlabel=expression(paste("Temperature instability (-(",mu,"/",sigma,"))")),ylabel="Ratio",posX=-9999,posY=-9999,tStability="***",tYield="")
es4 <- funPredRange(predictor="instabilityPrec",dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfLogCountry,dfOriginal=dfCountry,modS=modStabilityCountry,modY=modYieldCountry,trans="",xlabel=expression(paste("Precipitation instability (-(",mu,"/",sigma,"))")),ylabel="",posX=-9999,posY=-9999,tStability="***",tYield="***")
fs4 <- funPredRange(predictor="timePeriod",dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfLogCountry,dfOriginal=dfCountry,modS=modStabilityCountry,modY=modYieldCountry,trans="",xlabel="Time",ylabel="",posX=-9999,posY=-9999,tStability="***",tYield="***")

jpeg("results/FigS4.jpeg", width = 16.9, height = 16.9*(2/3), units = 'cm', res = 600)

ggarrange(as4, bs4, cs4, ds4, es4,fs4,
          labels = c(letters[1:6]),font.label=list(size=6),
          ncol = 3, nrow = 2,heights = c(1,1))

dev.off()

# region 
dfPredictRegion <- data.frame(diversity=rep(0,1000),irrigation=0,fertilizer=0,instabilityTemp=0,instabilityPrec=0,timePeriod=0)
as5 <- funPredRange(predictor="diversity",dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfLogRegion,dfOriginal=dfRegion,modS=modStabilityRegion,modY=modYieldRegion,trans="",xlabel="Diversity",ylabel="Ratio",posX=0.85,posY=0.8,tStability="***",tYield="***")
bs5 <- funPredRange(predictor="fertilizer",dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfLogRegion,dfOriginal=dfRegion,modS=modStabilityRegion,modY=modYieldRegion,trans="sqrt",xlabel="N use intensity (t/ha)",ylabel="",posX=-9999,posY=-9999,tStability="***",tYield="***")
cs5 <- funPredRange(predictor="irrigation",dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfLogRegion,dfOriginal=dfRegion,modS=modStabilityRegion,modY=modYieldRegion,trans="sqrt",xlabel="Irrigation (%)",ylabel="",posX=-9999,posY=-9999,tStability="***",tYield="")
ds5 <- funPredRange(predictor="instabilityTemp",dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfLogRegion,dfOriginal=dfRegion,modS=modStabilityRegion,modY=modYieldRegion,trans="",xlabel=expression(paste("Temperature instability (-(",mu,"/",sigma,"))")),ylabel="Ratio",posX=-9999,posY=-9999,tStability=" *",tYield="***")
es5 <- funPredRange(predictor="instabilityPrec",dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfLogRegion,dfOriginal=dfRegion,modS=modStabilityRegion,modY=modYieldRegion,trans="",xlabel=expression(paste("Precipitation instability (-(",mu,"/",sigma,"))")),ylabel="",posX=-9999,posY=-9999,tStability=" *",tYield="**")
fs5 <- funPredRange(predictor="timePeriod",dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfLogRegion,dfOriginal=dfRegion,modS=modStabilityRegion,modY=modYieldRegion,trans="",xlabel="Time",ylabel="",posX=-9999,posY=-9999,tStability="***",tYield="")

jpeg("results/FigS5.jpeg", width = 16.9, height = 16.9*(2/3), units = 'cm', res = 600)

ggarrange(as5, bs5, cs5, ds5, es5,fs5,
          labels = c(letters[1:6]),font.label=list(size=6),
          ncol = 3, nrow = 2,heights = c(1,1))

dev.off()

# farm 
dfPredictFarm <- data.frame(diversity=rep(0,1000),irrigation=0,fertilizer=0,instabilityTemp=0,instabilityPrec=0,timePeriod=0)
as6 <- funPredRange(predictor="diversity",dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfLogFarm,dfOriginal=dfFarm,modS=modStabilityFarm,modY=modYieldFarm,trans="",xlabel="Diversity",ylabel="Ratio",posX=0.85,posY=0.8,tStability="***",tYield="")
bs6 <- funPredRange(predictor="fertilizer",dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfLogFarm,dfOriginal=dfFarm,modS=modStabilityFarm,modY=modYieldFarm,trans="sqrt",xlabel="N use intensity (t/ha)",ylabel="",posX=-9999,posY=-9999,tStability="***",tYield="***")
cs6 <- funPredRange(predictor="irrigation",dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfLogFarm,dfOriginal=dfFarm,modS=modStabilityFarm,modY=modYieldFarm,trans="sqrt",xlabel="Irrigation (%)",ylabel="",posX=-9999,posY=-9999,tStability="",tYield="***")
ds6 <- funPredRange(predictor="instabilityTemp",dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfLogFarm,dfOriginal=dfFarm,modS=modStabilityFarm,modY=modYieldFarm,trans="",xlabel=expression(paste("Temperature instability (-(",mu,"/",sigma,"))")),ylabel="Ratio",posX=-9999,posY=-9999,tStability="***",tYield="***")
# es6 <- funPredRange(predictor="instabilityPrec",dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfLogFarm,dfOriginal=dfFarm,modS=modStabilityFarm,modY=modYieldFarm,trans="",xlabel=expression(paste("Precipitation instability (-(",mu,"/",sigma,"))")),ylabel="",posX=-9999,posY=-9999,tStability="***",tYield="***")
# fs6 <- funPredRange(predictor="timePeriod",dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfLogFarm,dfOriginal=dfFarm,modS=modStabilityFarm,modY=modYieldFarm,trans="",xlabel="Time",ylabel="",posX=-9999,posY=-9999,tStability="",tYield="***")

jpeg("results/FigS6.jpeg", width = 16.9*(2/3), height = 16.9*(2/3), units = 'cm', res = 600)

ggarrange(as6, bs6, cs6, ds6, 
          labels = c(letters[1:6]),font.label=list(size=6),
          ncol = 2, nrow = 2,heights = c(1,1))

dev.off()



######## Tables
TableS1 <- funTables(modStabilityCountryRaw,modStabilityRegionRaw,modStabilityFarmRaw,4, c("(Intercept)","Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time"))
write.csv(TableS1,"results/TableS1.csv")
TableS2 <- funTables(modYieldCountryRaw,modYieldRegionRaw,modYieldFarmRaw,4, c("(Intercept)","Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time"))
write.csv(TableS2,"results/TableS2.csv")



  
# dfPredictStabilityCountryChange <- merge(dfPredictStabilityCountry[which(dfPredictStabilityCountry$scenario=="Scenario"&dfPredictStabilityCountry$plot=="All"),c("Area","slope")],dfPredictStabilityCountry[which(dfPredictStabilityCountry$scenario=="Original"&dfPredictStabilityCountry$plot=="All"),c("Area","slope")],by="Area")
# dfPredictStabilityCountryChange$ratioReduction <- dfPredictStabilityCountryChange$slope.x/dfPredictStabilityCountryChange$slope.y
# dfPredictStabilityCountryChange$dim1 <-car::recode(dfPredictStabilityCountryChange$ratioReduction,"0:0.2=1; 0.2:0.4=2; 0.4:0.6=3;0.6:0.8=4;0.8:1.=5;")
# dfPredictStabilityCountryChange$dim1 <- vecColMap[dfPredictStabilityCountryChange$dim1]
# dfPredictStabilityCountryChange$Country <- dfPredictStabilityCountryChange$Area  
# mapsBivariate <- fortify(st_as_sf(mapCountry),region=level)
# mapsBivariate = merge(mapsBivariate, dfPredictStabilityCountryChange[,c("Country","dim1")], by="Country")
# 
# 
# a3 <- funMapPlot(mapsBivariate,mapsBivariate$dim1,"a")
# b3 <- funMapPlot(mapsBivariate,mapsBivariate$dim2,"b")
# c3 <- funMapPlot(mapsBivariate,mapsBivariate$dim3,"c")
# d3 <- funMapPlot(mapsBivariate,mapsBivariate$dim4,"c")
# 
# b3 <- funMapPlot(mapCountryStability,mapCountryStability$dim2)
# c3 <- funMapPlot(mapCountryStability,mapCountryStability$dim3)
# d3 <- funMapPlot(mapCountryStability,mapCountryStability$dim4)
# legendStability <- funLegend("Yield stability ratio")
# 
# jpeg("results/Fig4.jpeg", width = 16.9, height = 16.9*0.5, units = 'cm', res = 600)
#   ggarrange(a3,b3,emptyPlot,c3,d3,legendStability, nrow=2,ncol=3,labels=c("a","b","","c","d",""),font.label=list(size=8),
#             widths = c(0.45,0.45,0.1))
# dev.off()
# 
# 
# # ## yield
# dfPredictYield <- funPredictCountry(dfPredict,modYieldCountry,"slopeYield")
# mapCountryYield = join(mapCountry, dfPredictYield[,c("id","dim1","dim2","dim3","dim4")], by="id")
# as3 <- funMapPlot(mapCountryYield,mapCountryYield$dim1)
# # bs3 <- funMapPlot(mapCountryYield,mapCountryYield$dim2)
# # cs3 <- funMapPlot(mapCountryYield,mapCountryYield$dim3)
# # ds3 <- funMapPlot(mapCountryYield,mapCountryYield$dim4)
# # es3 <- funMapPlot(mapCountryYield,mapCountryYield$dim5)
# legendYield <- funLegend("Yield ratio")
# # 
# jpeg("results/FigS3.jpeg", width = 16.9*0.5, height = 16.9*0.25, units = 'cm', res = 600)
#   ggarrange(as3,legendYield,nrow=1,ncol=2,labels=c("",""),font.label=list(size=8),
#             widths = c(0.82,0.18))
# dev.off()
# #   



rm(list=ls())





