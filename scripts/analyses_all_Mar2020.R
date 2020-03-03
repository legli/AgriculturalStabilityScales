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

source("scripts/functionsAnalyses_Mar2020.R")



############################################################################################
###################              DATA               ########################################
############################################################################################

###### Country level
dfCountry <- read.csv("datasetsDerived/dataFinal_global.csv")
hist(dfCountry$stability) 
hist(dfCountry$stabilityOverall)
# remove stability outliers (very skewed)
dfCountry <- dfCountry[-which(dfCountry$stability%in%boxplot.stats(dfCountry$stability)$out),]

dfCountryScale <- read.csv("datasetsDerived/dataScales_global.csv")
dfCountryScale <- dfCountryScale[,c("Level","timePeriod","stability","yield","areaHarvested")]
dfCountryScale <- unique(dfCountryScale)
names(dfCountryScale)[1] <- "Country"
hist(dfCountryScale$stability)
# remove stability outliers (very skewed)
dfCountryScale <- dfCountryScale[-which(dfCountryScale$stability%in%boxplot.stats(dfCountryScale$stability)$out),]


###### Regional level
dfRegion <- read.csv("datasetsDerived/dataFinal_europe.csv")
dfRegion <- merge(dfRegion,dfCountry[,c("Country","timePeriod","fertilizer","irrigation")],by=c("Country","timePeriod"))
hist(dfRegion$stability)
hist(dfRegion$stabilityOverall)
# remove stability outliers (to be consistent with other levels)
dfRegion <- dfRegion[-which(dfRegion$stability%in%boxplot.stats(dfRegion$stability)$out),] 

dfRegionScale <- read.csv("datasetsDerived/dataScales_europe.csv")
dfRegionScale <- dfRegionScale[,c("Level","timePeriod","stability","yield","areaHarvested")]
dfRegionScale <- unique(dfRegionScale)
names(dfRegionScale)[1] <- "Region"
hist(dfRegionScale$stability)
# remove stability outliers (very skewed)
dfRegionScale <- dfRegionScale[-which(dfRegionScale$stability%in%boxplot.stats(dfRegionScale$stability)$out),]


###### Farm level
dfFarm <- read.csv("P:/dataFinal_farmlevel.csv")
hist(dfFarm$stability)
hist(dfFarm$stabilityOverall)
# remove stability outliers (very skewed)
dfFarm <- dfFarm[-which(dfFarm$stability%in%boxplot.stats(dfFarm$stability)$out),]

dfFarmScale <- read.csv("P:/dataScales_farm.csv")
dfFarmScale <- dfFarmScale[,c("Level","timePeriod","stability","yield","areaHarvested","prop")]
dfFarmScale <- unique(dfFarmScale)
names(dfFarmScale)[1] <- "Farm"
hist(dfFarmScale$stability)
# remove stability outliers (very skewed)
dfFarmScale <- dfFarmScale[-which(dfFarmScale$stability%in%boxplot.stats(dfFarmScale$stability)$out),]



############################################################################################
###################          ANALYSES               ########################################
############################################################################################

##################### Regression local 
###### Country level

# check distribution of response variables
fitdist(dfCountry$stability,"gamma")$aic - fitdist(dfCountry$stability,"lnorm")$aic #log-normally distributed
fitdist(dfCountry$yield,"gamma")$aic - fitdist(dfCountry$yield,"lnorm")$aic #log-normally distributed
hist(log(dfCountry$stability))
hist(log(dfCountry$yield))

### regression analyses
## transformations
dfTransCountry=with(dfCountry,data.frame(Country,
                                       stability = stability,
                                       yield = yield,
                                       diversity, 
                                       irrigation=sqrt(irrigation),
                                       fertilizer=sqrt(fertilizer),
                                       instabilityTemp,instabilityPrec,
                                       timePeriod
))

# scale predictors for standardized regression
dfPredictorsCountry=sapply(dfTransCountry[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterCountry=data.frame(Country=dfTransCountry[,1],stability=dfTransCountry[,2],yield=dfTransCountry[,3],dfPredictorsCountry)
head(dfCenterCountry)

## check colinearity
cor(dfCenterCountry[,2:9],method='s') 

## regression models
modStabilityCountry <- lm(stability~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterCountry)
summary(modStabilityCountry)
modStabilityCountryRaw <- lm(stability~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfTransCountry)
summary(modStabilityCountryRaw)
vif(modStabilityCountry)

modYieldCountry <- lm(yield~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterCountry)
summary(modYieldCountry)
modYieldCountryRaw <- lm(yield~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfTransCountry)
summary(modYieldCountryRaw)
vif(modYieldCountry)


###### European level

# check distribution of response variables
fitdist(dfRegion$stability,"gamma")$aic - fitdist(dfRegion$stability,"lnorm")$aic #log-normally distributed
fitdist(dfRegion$slopeYield,"gamma")$aic - fitdist(dfRegion$slopeYield,"lnorm")$aic #log-normally distributed
hist(log(dfRegion$stability))
hist(log(dfRegion$slopeYield))

### regression analyses
## transformations
dfTransRegion=with(dfRegion,data.frame(Region,
                                     stability = stability,
                                     yield = yield,
                                     diversity, 
                                     irrigation=sqrt(irrigation),
                                     fertilizer=sqrt(fertilizer),
                                     instabilityTemp,instabilityPrec,
                                     timePeriod
))

# scale predictors for standardized regression
dfPredictorsRegion=sapply(dfTransRegion[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterRegion=data.frame(Region=dfTransRegion[,1],stability=dfTransRegion[,2],yield=dfTransRegion[,3],dfPredictorsRegion)
head(dfCenterRegion)

## check colinearity
cor(dfCenterRegion[,2:9],method='s') 

## regression models
modStabilityRegion <- lm(stability~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterRegion)
summary(modStabilityRegion)
modStabilityRegionRaw <- lm(stability~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfTransRegion)
summary(modStabilityRegionRaw)
vif(modStabilityRegion)

modYieldRegion <- lm(yield~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterRegion)
summary(modYieldRegion)
modYieldRegionRaw <- lm(yield~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfTransRegion)
summary(modYieldRegionRaw)
vif(modYieldRegion)

###### Farm level
# check distribution of response variables
fitdist(dfFarm$stability,"gamma")$aic-fitdist(dfFarm$stability,"lnorm")$aic #log-normally distributed
fitdist(dfFarm$yield,"gamma")$aic-fitdist(dfFarm$yield,"lnorm")$aic #log-normally distributed
hist(log(dfFarm$stability))
hist(log(dfFarm$slopeYield))


### regression analyses
## transformations
dfTransFarm=with(dfFarm,data.frame(Farm,
                                 stability = stability,
                                 yield = yield,
                                 diversity, 
                                 irrigation=sqrt(irrigation),
                                 fertilizer=sqrt(fertilizer),
                                 instabilityTemp,instabilityPrec,
                                 timePeriod
))

# scale predictors for standardized regression
dfPredictorsFarm=sapply(dfTransFarm[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterFarm=data.frame(Farm=dfTransFarm[,1],stability=dfTransFarm[,2],yield=dfTransFarm[,3],dfPredictorsFarm)
head(dfCenterFarm)

## check colinearity
cor(dfCenterFarm[,2:9],method='s') # remove instabilityPrec and time Period

## regression models
modStabilityFarm <- lm(stability~diversity+fertilizer+irrigation+instabilityTemp,data=dfCenterFarm)
summary(modStabilityFarm)
modStabilityFarmRaw <- lm(stability~diversity+fertilizer+irrigation+instabilityTemp,data=dfTransFarm)
summary(modStabilityFarmRaw)
vif(modStabilityFarm)

modYieldFarm <- lm(yield~diversity+fertilizer+irrigation+instabilityTemp,data=dfCenterFarm)
summary(modYieldFarm)
modYieldFarmRaw <- lm(yield~diversity+fertilizer+irrigation+instabilityTemp,data=dfTransFarm)
summary(modYieldFarmRaw)
vif(modYieldFarm)




##################### Regression scale 
###### Natinal level
### preparation
# add original data
dfCountryScaleFinal <- rbind(dfCountry[,c("Country","timePeriod","stability","yield","areaHarvested")],dfCountryScale[,c("Country","timePeriod","stability","yield","areaHarvested")])

# change area harvested to mio ha
dfCountryScaleFinal$areaHarvested <- dfCountryScaleFinal$areaHarvested/1000000

### regression analyses
## transformations
dfTransCountryScale=with(dfCountryScaleFinal,data.frame(Country,
                                                      stability = stability,
                                                      yield = yield,
                                                      areaHarvested=areaHarvested, 
                                                      timePeriod
))

dfPredictorsCountryScale=sapply(dfTransCountryScale[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterCountryScale=data.frame(Country=dfTransCountryScale[,1],stability=dfTransCountryScale[,2],yield=dfTransCountryScale[,3],dfPredictorsCountryScale)
head(dfCenterFarm)

## linear model
modStabilityCountryScale <- lm(stability~areaHarvested+timePeriod,dfCenterCountryScale)
summary(modStabilityCountryScale)
modStabilityCountryScaleRaw <- lm(stability~areaHarvested+timePeriod,dfTransCountryScale)
summary(modStabilityCountryScaleRaw)
modYieldCountryScale <- lm(yield~areaHarvested+timePeriod,dfCenterCountryScale)
summary(modYieldCountryScale)


###### Regional level
### preparation
# add original data
dfRegionScaleFinal <- rbind(dfRegion[,c("Region","timePeriod","stability","yield","areaHarvested")],dfRegionScale[,c("Region","timePeriod","stability","yield","areaHarvested")])

# change area harvested to mio ha
dfRegionScaleFinal$areaHarvested <- dfRegionScaleFinal$areaHarvested/1000000

### regression analyses
## transformations
dfTransRegionScale=with(dfRegionScaleFinal,data.frame(Region,
                                                    stability = stability,
                                                    yield = yield,
                                                    areaHarvested=sqrt(areaHarvested), 
                                                    timePeriod
))

dfPredictorsRegionScale=sapply(dfTransRegionScale[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterRegionScale=data.frame(Region=dfTransRegionScale[,1],stability=dfTransRegionScale[,2],yield=dfTransRegionScale[,3],dfPredictorsRegionScale)
head(dfCenterFarm)

## regression models
modStabilityRegionScale <- lm(stability~areaHarvested+timePeriod,dfCenterRegionScale)
summary(modStabilityRegionScale)
modStabilityRegionScaleRaw <- lm(stability~areaHarvested+timePeriod,dfTransRegionScale)
summary(modStabilityRegionScaleRaw)
modYieldRegionScale <- lm(yield~areaHarvested+timePeriod,dfCenterRegionScale)
summary(modYieldRegionScale)


###### Farm level
### preparation
# add original data
dfFarmScaleFinal <- rbind(dfFarm[,c("Farm","timePeriod","stability","yield","areaHarvested")],dfFarmScale[,c("Farm","timePeriod","stability","yield","areaHarvested")])

# change area harvested to mio ha
dfFarmScaleFinal$areaHarvested <- dfFarmScaleFinal$areaHarvested/1000000

### regression analyses
## transformations
dfTransFarmScale=with(dfFarmScaleFinal,data.frame(Farm,
                                                stability = stability,
                                                yield = yield,
                                                areaHarvested, 
                                                timePeriod
))

dfPredictorsFarmScale=sapply(dfTransFarmScale[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterFarmScale=data.frame(Farm=dfTransFarmScale[,1],stability=dfTransFarmScale[,2],yield=dfTransFarmScale[,3],dfPredictorsFarmScale)
head(dfCenterFarm)

## regression models
modStabilityFarmScale <- lm(stability~areaHarvested+timePeriod,dfCenterFarm)
summary(modStabilityFarmScale)
modStabilityFarmScaleRaw <- lm(stability~areaHarvested+timePeriod,dfTransFarmScale)
summary(modStabilityFarmScaleRaw)
modYieldFarmScale <- lm(yield~areaHarvested+timePeriod,dfCenterFarm)
summary(modYieldFarmScale)

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
        axis.title.y = element_text(angle = 90))

g.legend1 <- g.legend + labs(x="Small scale stability",y="Large scale stability") 
g.legend2 <- g.legend + labs(x="Small scale yields",y="Large scale yields") 
vp<-viewport(width=0.2,height=0.2,x=0.12,y=0.6)

# national level
mapCountry <- readOGR("spatial/countries_global.shp")
mapCountry$Country <-  countrycode(mapCountry$Area, 'country.name', 'iso3c')
# dfCountry$ratioStability <- dfCountry$stability/dfCountry$stabilityOverall
dfCountry$ratioStabilityReduced <- dfCountry$stabilityOverall/dfCountry$stabilityReduced
dfCountryAgg <- aggregate(cbind(stability,stabilityOverall,ratioStabilityReduced,yield,yieldOverall,yieldReduced)~Country,dfCountry,mean)

# regional level
mapRegion <- readOGR("spatial/regions_europe.shp")
mapRegion$Region <- mapRegion$NUTS_ID
dfRegion$ratioStabilityReduced <- dfRegion$stabilityOverall/dfRegion$stabilityReduced
dfRegionAgg <- aggregate(cbind(stability,stabilityOverall,ratioStabilityReduced,yield,yieldOverall,yieldReduced)~Region,dfRegion,mean)

# farm level
mapFarm <- readOGR("spatial/vg2500_rbz_bld.shp")
mapFarm$REGION_ID <- mapFarm$district
dfFarm$ratioStabilityReduced <- dfFarm$stabilityOverall/dfFarm$stabilityReduced
dfFarmAgg <- aggregate(cbind(stability,stabilityOverall,ratioStabilityReduced,yield,yieldOverall,yieldReduced)~Farm,dfFarm,mean)


a1 <- funFig1(dfCountryAgg,variable="stability",st_as_sf(mapCountry),"Country",seq(0,100,length.out = 11),c("#FFFFE5","#78C679","#004529"),                "Stability (small scale)")
b1 <- funFig1(dfCountryAgg,variable="stabilityOverall",st_as_sf(mapCountry),"Country",seq(0,100,length.out = 11),c("#FFFFE5","#78C679","#004529"),         "Stability (large scale)")
c1 <- funFig1(dfCountryAgg,variable="ratioStabilityReduced",st_as_sf(mapCountry),"Country",seq(0.85,1.15,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Stabilizing ratio      ")

jpeg("results/Fig1.jpeg", width = 16.9,height = 16.9/6.5, units = 'cm',res = 600)

ggarrange(a1,b1,c1,
          labels = letters[1:3],font.label=list(size=3),
          ncol = 3, nrow = 1)
dev.off()

a2 <- funFig1(dfRegionAgg,variable="stability",st_as_sf(mapRegion),"Region",seq(0,30,length.out = 11),c("#FFFFE5","#78C679","#004529"),                "Stability (small scale)")
b2 <- funFig1(dfRegionAgg,variable="stabilityOverall",st_as_sf(mapRegion),"Region",seq(0,30,length.out = 11),c("#FFFFE5","#78C679","#004529"),         "Stability (large scale)")
c2 <- funFig1(dfRegionAgg,variable="ratioStabilityReduced",st_as_sf(mapRegion),"Region",seq(0.85,1.15,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Stabilizing ratio      ")

jpeg("results/Fig2.jpeg", width = 16.9,height = 16.9/4, units = 'cm',res = 600)

ggarrange(a2,b2,c2,
          labels = letters[1:3],font.label=list(size=3),
          ncol = 3, nrow = 1)
dev.off()


a3 <- ggplot(dfFarmAgg, aes(x=stability)) +
  geom_histogram(alpha=0.5, position="identity",breaks = seq(0,50,5))+
  # theme_void() +
  theme(axis.title=element_text(size=8)) +
  theme(axis.text = element_text(size=8))+
  theme(axis.title.y = element_text(angle=90))+
  # geom_hline(yintercept=0,size=0)+
  # geom_vline(xintercept=0,size=0)+
  labs(y= "Count", x = "Stablity (small scale)")+
  scale_fill_discrete(name = "")+
  ylim(0,3000)

b3 <- ggplot(dfFarmAgg, aes(x=stabilityOverall)) +
  geom_histogram(alpha=0.5, position="identity",breaks = seq(0,50,5))+
  # theme_void() +
  theme(axis.title=element_text(size=8)) +
  theme(axis.text = element_text(size=8))+
  theme(axis.title.y = element_text(angle=90))+
  # geom_hline(yintercept=0,size=0)+
  # geom_vline(xintercept=0,size=0)+
  labs(y= "Count", x = "Stability (large scale)")+
  scale_fill_discrete(name = "")+
  ylim(0,3000)

c3 <- ggplot(dfFarmAgg, aes(x=ratioStabilityReduced)) +
  geom_histogram(alpha=0.5, position="identity",breaks = seq(0.5,1.5,0.1))+
  # theme_void() +
  theme(axis.title=element_text(size=8)) +
  theme(axis.text = element_text(size=8))+
  theme(axis.title.y = element_text(angle=90))+
  # geom_hline(yintercept=0,size=0)+
  # geom_vline(xintercept=0,size=0)+
  labs(y= "Count", x = "Stabilitzing ratio")+
  scale_fill_discrete(name = "")+
  ylim(0,5000)


jpeg("results/Fig3.jpeg", width = 16.9,height = 16.9/4, units = 'cm',res = 600)

ggarrange(a3,b3,c3,
          labels = letters[1:3],font.label=list(size=3),
          ncol = 3, nrow = 1)
dev.off()




##### Part 2: regression results

## Fig 4: barplots
# barplot of  stability model
a4 <- funCombinePlot("all",modStabilityCountry,modStabilityCountryScale,T,"National",myColors[1],8,"")
b4 <- funCombinePlot("all",modStabilityRegion,modStabilityRegionScale,T,"Regional",myColors[2],8,"Standardized regression coefficient")
c4 <- funCombinePlot("farm",modStabilityFarm,modStabilityFarmScale,T,"Farm",myColors[3],8,"")


# plot
jpeg("results/Fig4.jpeg", width = 8, height = 8*3, units = 'cm', res = 600)
  ggarrange(a4,b4,c4,
            labels = letters[1:3],font.label=list(size=8),
            ncol = 1, nrow = 3)
dev.off()

## Fig 5: predictions
dfPredictCountry <- data.frame(diversity=rep(0,1000),irrigation=0,fertilizer=0,instabilityTemp=0,instabilityPrec=0,timePeriod=0,areaHarvested=0)
dfPredictRegion <- data.frame(diversity=rep(0,1000),irrigation=0,fertilizer=0,instabilityTemp=0,instabilityPrec=0,timePeriod=0,areHarvested=0)
dfPredictFarm <- data.frame(diversity=rep(0,1000),irrigation=0,fertilizer=0,instabilityTemp=0,instabilityPrec=0,timePeriod=0,areaHarvested=0)

a5 <- funPredRange(predictor="diversity",dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,dfOriginal=dfCountry,modS=modStabilityCountry,trans="",xlabel="",ylabel=expression(paste("Yield stability (",mu,"/",sigma,")")),0,70,myColors[1])
b5 <- funPredRange(predictor="areaHarvested",dfPredict=dfPredictCountry,dfCenter=dfCenterCountryScale,dfLog=dfTransCountryScale,dfOriginal=dfCountryScaleFinal,modS=modStabilityCountryScale,trans="",xlabel="",ylabel="",0,70,myColors[1])
c5 <- funPredRange(predictor="diversity",dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,dfOriginal=dfRegion,modS=modStabilityRegion,trans="",xlabel="",ylabel=expression(paste("Yield stability (",mu,"/",sigma,")")),0,30,myColors[2])
d5 <- funPredRange(predictor="areaHarvested",dfPredict=dfPredictRegion,dfCenter=dfCenterRegionScale,dfLog=dfTransRegionScale,dfOriginal=dfRegionScaleFinal,modS=modStabilityRegionScale,trans="sqrt",xlabel="",ylabel="",0,30,myColors[2])
e5 <- funPredRange(predictor="diversity",dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfTransFarm,dfOriginal=dfFarm,modS=modStabilityFarm,trans="",xlabel="Diversity",ylabel=expression(paste("Yield stability (",mu,"/",sigma,")")),-10,30,myColors[3])
f5 <- funPredRange(predictor="areaHarvested",dfPredict=dfPredictFarm,dfCenter=dfCenterFarmScale,dfLog=dfTransFarmScale,dfOriginal=dfFarmScaleFinal,modS=modStabilityFarmScale,trans="",xlabel="Area harvested (Mio. ha)",ylabel="",-10,30,myColors[3])

jpeg("results/Fig5.jpeg", width = 16.9, height = 16.9*1.5, units = 'cm', res = 600)

ggarrange(a5, b5, c5, d5, e5,f5,
          labels = c(letters[1:6]),font.label=list(size=6),
          ncol = 2, nrow = 3,heights = c(1,1))

dev.off()

#### Fig S4-6: continious effect of predictors
# country 
as4 <- funPredRange(predictor="diversity",dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,dfOriginal=dfCountry,modS=modStabilityCountry,trans="",xlabel="Diversity",ylabel=expression(paste("Yield stability (",mu,"/",sigma,")")),0,70,"gray30")
bs4 <- funPredRange(predictor="fertilizer",dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,dfOriginal=dfCountry,modS=modStabilityCountry,trans="sqrt",xlabel="Fertilizer (t/ha)",ylabel="",0,70,"gray30")
cs4 <- funPredRange(predictor="irrigation",dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,dfOriginal=dfCountry,modS=modStabilityCountry,trans="sqrt",xlabel="Irrigation (%)",ylabel="",0,70,"gray30")
ds4 <- funPredRange(predictor="instabilityTemp",dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,dfOriginal=dfCountry,modS=modStabilityCountry,trans="",xlabel=expression(paste("Temperature instability (-(",mu,"/",sigma,"))")),ylabel=expression(paste("Yield stability (",mu,"/",sigma,")")),0,70,"gray30")
es4 <- funPredRange(predictor="instabilityPrec",dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,dfOriginal=dfCountry,modS=modStabilityCountry,trans="",xlabel=expression(paste("Precipitation instability (-(",mu,"/",sigma,"))")),ylabel="",0,70,"gray30")
fs4 <- funPredRange(predictor="areaHarvested",dfPredict=dfPredictCountry,dfCenter=dfCenterCountryScale,dfLog=dfTransCountryScale,dfOriginal=dfCountryScaleFinal,modS=modStabilityCountryScale,trans="",xlabel="Area harvested (Mio. ha)",ylabel="",0,70,"gray30")

jpeg("results/FigS1.jpeg", width = 16.9, height = 16.9*(2/3), units = 'cm', res = 600)

ggarrange(as4, bs4, cs4, ds4, es4,fs4,
          labels = c(letters[1:6]),font.label=list(size=6),
          ncol = 3, nrow = 2,heights = c(1,1))

dev.off()

# region 
as2 <- funPredRange(predictor="diversity",dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,dfOriginal=dfRegion,modS=modStabilityRegion,trans="",xlabel="Diversity",ylabel=expression(paste("Yield stability (",mu,"/",sigma,")")),0,30,"gray30")
bs2 <- funPredRange(predictor="fertilizer",dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,dfOriginal=dfRegion,modS=modStabilityRegion,trans="sqrt",xlabel="Fertilizer (t/ha)",ylabel="",0,30,"gray30")
cs2 <- funPredRange(predictor="areaHarvested",dfPredict=dfPredictRegion,dfCenter=dfCenterRegionScale,dfLog=dfTransRegionScale,dfOriginal=dfRegionScaleFinal,modS=modStabilityRegionScale,trans="sqrt",xlabel="Area harvested (Mio. ha)",ylabel="",0,30,"gray30")

jpeg("results/FigS2.jpeg", width = 16.9, height = 16.9*(1/3), units = 'cm', res = 600)

ggarrange(as2, bs2, cs2, 
          labels = c(letters[1:3]),font.label=list(size=6),
          ncol = 3, nrow = 1,heights = c(1,1))

dev.off()

# farm 
as3 <- funPredRange(predictor="diversity",dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfTransFarm,dfOriginal=dfFarm,modS=modStabilityFarm,trans="",xlabel="Diversity",ylabel=expression(paste("Yield stability (",mu,"/",sigma,")")),-10,30,"gray30")
bs3 <- funPredRange(predictor="fertilizer",dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfTransFarm,dfOriginal=dfFarm,modS=modStabilityFarm,trans="sqrt",xlabel=paste0("Fertilizer (","\u20AC","/ha)"),ylabel="",-10,30,"gray30")
cs3 <- funPredRange(predictor="instabilityTemp",dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfTransFarm,dfOriginal=dfFarm,modS=modStabilityFarm,trans="",xlabel=expression(paste("Temperature instability (-(",mu,"/",sigma,"))")),ylabel=expression(paste("Yield stability (",mu,"/",sigma,")")),-10,30,"gray30")
ds3 <- funPredRange(predictor="areaHarvested",dfPredict=dfPredictFarm,dfCenter=dfCenterFarmScale,dfLog=dfTransFarmScale,dfOriginal=dfFarmScaleFinal,modS=modStabilityFarmScale,trans="",xlabel="Area harvested (Mio. ha)",ylabel="",-10,30,"gray30")

jpeg("results/FigS3.jpeg", width = 16.9*(2/3), height = 16.9*(2/3), units = 'cm', res = 600)

ggarrange(as3, bs3, cs3, ds3, 
          labels = c(letters[1:6]),font.label=list(size=6),
          ncol = 2, nrow = 2,heights = c(1,1))

dev.off()



######## Tables
TableS1a <- funTables(modStabilityCountryRaw,modStabilityRegionRaw,modStabilityFarmRaw,2, c("(Intercept)","Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time"))
TableS1b <- funTablesScales(modStabilityCountryScaleRaw,modStabilityRegionScaleRaw,modStabilityFarmScaleRaw,2, c("(Intercept)","Area harvested","Time"))
TableS1 <- rbind(TableS1a,TableS1b)
write.csv(TableS1,"results/TableS1.csv")
# TableS2 <- funTables(modYieldCountryRaw,modYieldRegionRaw,modYieldFarmRaw,4, c("(Intercept)","Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time"))
# write.csv(TableS2,"results/TableS2.csv")



  
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





