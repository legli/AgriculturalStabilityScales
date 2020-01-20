library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(sf)
library(rgdal)
library(raster)
library(plyr)

## globals

vecColors <- brewer.pal(5,"PuBu")
vecColors2 <- c(brewer.pal(11,"PuOr")[c(1,4,10)],"darkgreen")

lev <- c("National","Subnational","Farm")
myColors <- c("#4daf4a",vecColors[5], "#ff7f00")
names(myColors) <- factor(lev,levels=lev)

source("scripts/functionsAnalyses.R")

##################### Regression local

###### National level
### preparation
# read df
dfNational <- read.csv("datasetsDerived/dataFinal_global.csv")

# remove outliers
hist(dfNational$stability)
dfNational <- dfNational[-which(dfNational$stability%in%boxplot.stats(dfNational$stability)$out),]

### regression analyses
## transformations
dfLogNational=with(dfNational,data.frame(Country,
                                  stability = log(stability),
                                  yield = log(yield),
                                  diversity, 
                                  irrigation=sqrt(meanIrrigation_share),
                                  fertilizer=sqrt(meanNitrogen),
                                  instabilityTemp,instabilityPrec,
                                  timePeriod
))
names(dfLogNational)
head(dfLogNational)

# scale predictors for standardized regression
dfPredictorsNational=sapply(dfLogNational[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterNational=data.frame(Country=dfLogNational[,1],stability=dfLogNational[,2],yield=dfLogNational[,3],dfPredictorsNational)
head(dfCenterNational)

## regression models
modStabilityNational <- lm(stability~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterNational)
summary(modStabilityNational)

modYieldNational <- lm(yield~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterNational)
summary(modYieldNational)


###### SUBNATIONAL LEVEL
### preparation
dfSubnational <- read.csv("datasetsDerived/dataFinal_europe.csv")
dfSubnational <- merge(dfSubnational,dfNational[,c("Country","timePeriod","meanNitrogen","meanIrrigation_share")],by=c("Country","timePeriod"))

# remove outliers
hist(dfSubnational$stability)
dfSubnational <- dfSubnational[-which(dfSubnational$stability%in%boxplot.stats(dfSubnational$stability)$out),]

### regression analyses
## transformations
dfLogSubnational=with(dfSubnational,data.frame(RegionCode,
                                  stability = log(stability),
                                  yield = log(yield),
                                  diversity, 
                                  irrigation=sqrt(meanIrrigation_share),
                                  fertilizer=sqrt(meanNitrogen),                                  
                                  instabilityTemp,instabilityPrec,
                                  timePeriod
))
names(dfLogSubnational)
head(dfLogSubnational)

# scale predictors for standardized regression
dfPredictorsSubnational=sapply(dfLogSubnational[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterSubnational=data.frame(Area=dfLogSubnational[,1],ratioStability=dfLogSubnational[,2],ratioYield=dfLogSubnational[,3],dfPredictorsSubnational)
head(dfCenterSubnational)

# regression models
modStabilitySubnational <- lm(ratioStability~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterSubnational)
summary(modStabilitySubnational)

modYieldSubnational <- lm(ratioYield~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterSubnational)
summary(modYieldSubnational)


####### FARM LEVEL
### preparation
dfFarm <- read.csv("P:/dataFinal_farmlevel.csv")

# remove outliers
hist(dfFarm$stability)
dfFarm <- dfFarm[-which(dfFarm$stability%in%boxplot.stats(dfFarm$stability)$out),]


### regression analyses
## transformations
dfLogFarm=with(dfFarm,data.frame(Farmer,
                                  stability = log(stability),
                                  yield = log(yield),
                                  diversity,
                                  irrigation=sqrt(meanIrrigation),
                                  fertilizer=sqrt(meanFertilizer),
                                  instabilityTemp,instabilityPrec,
                                  timePeriod
))
names(dfLogFarm)
head(dfLogFarm)

# scale predictors for standardized regression
dfPredictorsFarm=sapply(dfLogFarm[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterFarm=data.frame(Farm=dfLogFarm[,1],ratioStability=dfLogFarm[,2],ratioYield=dfLogFarm[,3],dfPredictorsFarm)
head(dfCenterFarm)

# regression models
modStabilityFarm <- lm(ratioStability~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterFarm)
summary(modStabilityFarm)

modYieldFarm <- lm(ratioYield~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterFarm)
summary(modYieldFarm)


##################### Regression scale
###### National level
### preparation
dfNationalScale <- read.csv("datasetsDerived/dataScales_global.csv")
dfNationalScale <- dfNationalScale[,c("Area","timePeriod","stability","yield","prop")]
names(dfNationalScale)[1] <- "Country"
dfNationalScaleAgg <- aggregate(cbind(stability,yield)~Country+timePeriod+prop,dfNationalScale,mean)

# add original data
dfNationalRed <- dfNational
dfNationalRed$prop <- 0
dfNationalScaleAgg <- rbind(dfNationalRed[,c("Country","timePeriod","stability","yield","prop")],dfNationalScaleAgg[,c("Country","timePeriod","stability","yield","prop")])

# remove outliers
hist(dfNationalScaleAgg$stability)
intOutlierNational <- which(dfNationalScaleAgg$stability%in%boxplot.stats(dfNationalScaleAgg$stability)$out)
if (length(intOutlierNational)>0)
{
  dfNationalScaleAgg <- dfNationalScaleAgg[-intOutlierNational,]
}

### regression analyses
## transformations
dfLogNationalScale=with(dfNationalScaleAgg,data.frame(Country,
                                        stability = log(stability),
                                        yield = log(yield),
                                        prop, 
                                        timePeriod
))

# scale predictors for standardized regression
dfPredictorsNationalScale=sapply(dfLogNationalScale[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterNationalScale=data.frame(Country=dfLogNationalScale[,1],stability=dfLogNationalScale[,2],yield=dfLogNationalScale[,3],dfPredictorsNationalScale)
head(dfCenterNationalScale)

## regression models
modStabilityNationalScale <- lm(stability~prop+timePeriod,dfCenterNationalScale)
summary(modStabilityNationalScale)

modYieldNationalScale <- lm(yield~prop+timePeriod,dfCenterNationalScale)
summary(modYieldNationalScale)

###### European level
### preparation
dfSubnationalScale <- read.csv("datasetsDerived/dataScales_europe.csv")
dfSubnationalScale <- dfSubnationalScale[,c("Area","timePeriod","stability","yield","prop")]
names(dfSubnationalScale)[1] <- "RegionCode"
dfSubnationalScaleAgg <- aggregate(cbind(stability,yield)~RegionCode+timePeriod+prop,dfSubnationalScale,mean)

# add original data
dfSubnationalRed <- dfSubnational
dfSubnationalRed$prop <- 0
dfSubnationalScaleAgg <- rbind(dfSubnationalRed[,c("RegionCode","timePeriod","stability","yield","prop")],dfSubnationalScaleAgg[,c("RegionCode","timePeriod","stability","yield","prop")])

# remove outliers
hist(dfSubnationalScaleAgg$stability)
intOutlierSubnational <- which(dfSubnationalScaleAgg$stability%in%boxplot.stats(dfSubnationalScaleAgg$stability)$out)
if (length(intOutlierSubnational)>0)
{
  dfSubnationalScaleAgg <- dfSubnationalScaleAgg[-intOutlierSubnational,]
}

### regression analyses
## transformations
dfLogSubnationalScale=with(dfSubnationalScaleAgg,data.frame(RegionCode,
                                                      stability = log(stability),
                                                      yield = log(yield),
                                                      prop, 
                                                      timePeriod
))

# scale predictors for standardized regression
dfPredictorsSubnationalScale=sapply(dfLogSubnationalScale[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterSubnationalScale=data.frame(RegionCode=dfLogSubnationalScale[,1],stability=dfLogSubnationalScale[,2],yield=dfLogSubnationalScale[,3],dfPredictorsSubnationalScale)
head(dfCenterSubnationalScale)

## regression models
modStabilitySubnationalScale <- lm(stability~prop+timePeriod,dfCenterSubnationalScale)
summary(modStabilitySubnationalScale)

modYieldSubnationalScale <- lm(yield~prop+timePeriod,dfCenterSubnationalScale)
summary(modYieldSubnationalScale)

###### Farm level
### preparation
dfFarmScale <- read.csv("datasetsDerived/dataScales_farm.csv")
dfFarmScale <- dfFarmScale[,c("Area","timePeriod","stability","yield","prop")]
names(dfFarmScale)[1] <- "Farmer"
dfFarmScaleAgg <- aggregate(cbind(stability,yield)~Farmer+timePeriod+prop,dfFarmScale,mean)

# add original data
dfFarmRed <- dfFarm
dfFarmRed$prop <- 0
dfFarmScaleAgg <- rbind(dfFarmRed[,c("Farmer","timePeriod","stability","yield","prop")],dfFarmScaleAgg[,c("Farmer","timePeriod","stability","yield","prop")])

# remove outliers
hist(dfFarmScaleAgg$stability)
intOutlierFarm <- which(dfFarmScaleAgg$stability%in%boxplot.stats(dfFarmScaleAgg$stability)$out)
if (length(intOutlierFarm)>0)
{
  dfFarmScaleAgg <- dfFarmScaleAgg[-intOutlierFarm,]
}

### regression analyses
## transformations
dfLogFarmScale=with(dfFarmScaleAgg,data.frame(Farmer,
                                              stability = log(stability),
                                              yield = log(yield),
                                              prop, 
                                              timePeriod
))

# scale predictors for standardized regression
dfPredictorsFarmScale=sapply(dfLogFarmScale[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterFarmScale=data.frame(Country=dfLogFarmScale[,1],stability=dfLogFarmScale[,2],yield=dfLogFarmScale[,3],dfPredictorsFarmScale)
head(dfCenterFarmScale)

## regression models
modStabilityFarmScale <- lm(stability~prop+timePeriod,dfCenterFarmScale)
summary(modStabilityFarmScale)

modYieldFarmScale <- lm(yield~prop+timePeriod,dfCenterFarmScale)
summary(modYieldFarmScale)

##################### Regression combined
###### National level
### preparation

# get extremes
dfNationalCombined <- merge(dfNationalRed,dfNationalScaleAgg[which(dfNationalScaleAgg$prop==1),],by=c("Country","timePeriod"))
head(dfNationalCombined)
dfNationalCombined$slopeStability <- dfNationalCombined$stability.y/dfNationalCombined$stability.x
dfNationalCombined$slopeYield <- dfNationalCombined$yield.y/dfNationalCombined$yield.x

# remove outliers
hist(dfNationalCombined$slopeStability)
intOutlierNational <- which(dfNationalCombined$slopeStability%in%boxplot.stats(dfNationalCombined$slopeStability)$out)
if (length(intOutlierNational)>0)
{
  dfNationalCombined <- dfNationalCombined[-intOutlierNational,]
}

### regression analyses
## transformations
dfLogNationalCombined=with(dfNationalCombined,data.frame(Country,
                                         slopeStability = log(slopeStability),
                                         slopeYield = log(slopeYield),
                                         diversity, 
                                         irrigation=sqrt(meanIrrigation_share),
                                         fertilizer=sqrt(meanNitrogen),
                                         instabilityTemp,instabilityPrec,
                                         timePeriod
))

# scale predictors for standardized regression
dfPredictorsNationalCombined=sapply(dfLogNationalCombined[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterNationalCombined=data.frame(Country=dfLogNationalCombined[,1],slopeStability=dfLogNationalCombined[,2],slopeYield=dfLogNationalCombined[,3],dfPredictorsNationalCombined)
head(dfCenterNationalCombined)

## regression models
modStabilityNationalCombined <- lm(slopeStability~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterNationalCombined)
summary(modStabilityNationalCombined)

modYieldNationalCombined <- lm(slopeYield~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterNationalCombined)
summary(modYieldNationalCombined)

###### European level
### preparation

# get extremes
dfSubnationalCombined <- merge(dfSubnationalRed,dfSubnationalScaleAgg[which(dfSubnationalScaleAgg$prop==1),],by=c("RegionCode","timePeriod"))
head(dfSubnationalCombined)
dfSubnationalCombined$slopeStability <- dfSubnationalCombined$stability.y/dfSubnationalCombined$stability.x
dfSubnationalCombined$slopeYield <- dfSubnationalCombined$yield.y/dfSubnationalCombined$yield.x

# remove outliers
hist(dfSubnationalCombined$slopeStability)
intOutlierSubnational <- which(dfSubnationalCombined$slopeStability%in%boxplot.stats(dfSubnationalCombined$slopeStability)$out)
if (length(intOutlierSubnational)>0)
{
  dfSubnationalCombined <- dfSubnationalCombined[-intOutlierSubnational,]
}
### regression analyses
## transformations
dfLogSubnationalCombined=with(dfSubnationalCombined,data.frame(Country,
                                                         slopeStability = log(slopeStability),
                                                         slopeYield = log(slopeYield),
                                                         diversity, 
                                                         irrigation=sqrt(meanIrrigation_share),
                                                         fertilizer=sqrt(meanNitrogen),
                                                         instabilityTemp,instabilityPrec,
                                                         timePeriod
))

# scale predictors for standardized regression
dfPredictorsSubnationalCombined=sapply(dfLogSubnationalCombined[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterSubnationalCombined=data.frame(Country=dfLogSubnationalCombined[,1],slopeStability=dfLogSubnationalCombined[,2],slopeYield=dfLogSubnationalCombined[,3],dfPredictorsSubnationalCombined)
head(dfCenterSubnationalCombined)

## regression models
modStabilitySubnationalCombined <- lm(slopeStability~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterSubnationalCombined)
summary(modStabilitySubnationalCombined)

modYieldSubnationalCombined <- lm(slopeYield~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterSubnationalCombined)
summary(modYieldSubnationalCombined)

###### Farm level
### preparation

# get extremes
dfFarmCombined <- merge(dfFarmRed,dfFarmScaleAgg[which(dfFarmScaleAgg$prop==1),],by=c("Farmer","timePeriod"))
head(dfFarmCombined)
dfFarmCombined$slopeStability <- dfFarmCombined$stability.y/dfFarmCombined$stability.x
dfFarmCombined$slopeYield <- dfFarmCombined$yield.y/dfFarmCombined$yield.x

# remove outliers
hist(dfFarmCombined$slopeStability)
intOutlierFarm <- which(dfFarmCombined$slopeStability%in%boxplot.stats(dfFarmCombined$slopeStability)$out)
if (length(intOutlierFarm)>0)
{
  dfFarmCombined <- dfFarmCombined[-intOutlierFarm,]
}
### regression analyses
## transformations
dfLogFarmCombined=with(dfFarmCombined,data.frame(Farmer,
                                                         slopeStability = log(slopeStability),
                                                         slopeYield = log(slopeYield),
                                                         diversity, 
                                                         irrigation=sqrt(meanIrrigation),
                                                         fertilizer=sqrt(meanFertilizer),
                                                         instabilityTemp,instabilityPrec,
                                                         timePeriod
))

# scale predictors for standardized regression
dfPredictorsFarmCombined=sapply(dfLogFarmCombined[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterFarmCombined=data.frame(Country=dfLogFarmCombined[,1],slopeStability=dfLogFarmCombined[,2],slopeYield=dfLogFarmCombined[,3],dfPredictorsFarmCombined)
head(dfCenterFarmCombined)

## regression models
modStabilityFarmCombined <- lm(slopeStability~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterFarmCombined)
summary(modStabilityFarmCombined)

modYieldFarmCombined <- lm(slopeYield~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterFarmCombined)
summary(modYieldFarmCombined)

###### FIGURES

### Fig 1. 
# barplot of local stability model
dfRegStabilityNational <- data.frame(summary(modStabilityNational)$coefficients[2:7,c(1,2,4)])
names(dfRegStabilityNational) <- c("Effect","SE","pVal")
dfRegStabilityNational$nam <- c("Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time")
dfRegStabilityNational$Level <- "National"

dfRegStabilitySubnational <- data.frame(summary(modStabilitySubnational)$coefficients)[2:7,c(1,2,4)]
names(dfRegStabilitySubnational) <- c("Effect","SE","pVal")
dfRegStabilitySubnational$nam <- c("Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time")
dfRegStabilitySubnational$Level <-  "Subnational"

dfRegStabilityFarm <- data.frame(summary(modStabilityFarm)$coefficients)[2:7,c(1,2,4)]
names(dfRegStabilityFarm) <- c("Effect","SE","pVal")
dfRegStabilityFarm$nam <- c("Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time")
dfRegStabilityFarm$Level <-  "Farm"

dfCombinedStabiliy <- funCombine(dfRegStabilityNational,dfRegStabilitySubnational,dfRegStabilityFarm)
dfCombinedStabiliy <- dfCombinedStabiliy[unlist(lapply(1:6,function(i)seq(i,18,6))),]
dfTextStability <- data.frame(xpos=sort(c(1:6-0.3,1:6,1:6+0.3)),ypos=dfCombinedStabiliy$labHeight,lab=dfCombinedStabiliy$lab,Level=dfCombinedStabiliy$Level)

a1 <- funPlot(dfCombinedStabiliy,dfTextStability,0.3,"Standardized regression coefficient",F)

# b
# barplot of effects: combine coefficents of both modesl
dfRegYieldNational <- data.frame(summary(modYieldNational)$coefficients[2:7,c(1,2,4)])
names(dfRegYieldNational) <- c("Effect","SE","pVal")
dfRegYieldNational$nam <- c("Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time")
dfRegYieldNational$Level <- "National"

dfRegYieldSubnational <- data.frame(summary(modYieldSubnational)$coefficients)[2:7,c(1,2,4)]
names(dfRegYieldSubnational) <- c("Effect","SE","pVal")
dfRegYieldSubnational$nam <- c("Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time")
dfRegYieldSubnational$Level <-  "Subnational"

dfRegYieldFarm <- data.frame(summary(modYieldFarm)$coefficients)[2:7,c(1,2,4)]
names(dfRegYieldFarm) <- c("Effect","SE","pVal")
dfRegYieldFarm$nam <- c("Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time")
dfRegYieldFarm$Level <-  "Farm"

dfCombinedYield <- funCombine(dfRegYieldNational,dfRegYieldSubnational,dfRegYieldFarm)
dfCombinedYield <- dfCombinedYield[unlist(lapply(1:6,function(i)seq(i,18,6))),]
dfTextYield <- data.frame(xpos=sort(c(1:6-0.3,1:6,1:6+0.3)),ypos=dfCombinedYield$labHeight,lab=dfCombinedYield$lab,Level=dfCombinedYield$Level)

b1 <- funPlot(dfCombinedYield,dfTextYield,0.6,"Standardized regression coefficient",T)

# plot
jpeg("results/Fig1.jpeg", width = 16.9, height = 16.9*0.5, units = 'cm', res = 600)

ggarrange(a1,b1,
          labels = letters[1:2],font.label=list(size=8),
          ncol = 2, nrow = 1)

dev.off()

### Fig 2
# barplot of scale stability model
dfRegStabilityNationalScale <- data.frame(summary(modStabilityNationalScale)$coefficients[2:3,c(1,2,4)])
names(dfRegStabilityNationalScale) <- c("Effect","SE","pVal")
dfRegStabilityNationalScale$nam <- c("Scale","Time")
dfRegStabilityNationalScale$Level <- "National"

dfRegStabilitySubnationalScale <- data.frame(summary(modStabilitySubnationalScale)$coefficients[2:3,c(1,2,4)])
names(dfRegStabilitySubnationalScale) <- c("Effect","SE","pVal")
dfRegStabilitySubnationalScale$nam <- c("Scale","Time")
dfRegStabilitySubnationalScale$Level <- "Subnational"

dfRegStabilityFarmScale <- data.frame(summary(modStabilityFarmScale)$coefficients[2:3,c(1,2,4)])
names(dfRegStabilityFarmScale) <- c("Effect","SE","pVal")
dfRegStabilityFarmScale$nam <- c("Scale","Time")
dfRegStabilityFarmScale$Level <- "Farm"

dfCombinedStabilityScale <- funCombine(dfRegStabilityNationalScale,dfRegStabilitySubnationalScale,dfRegStabilityFarmScale)
dfCombinedStabilityScale <- dfCombinedStabilityScale[unlist(lapply(1:2,function(i)seq(i,6,2))),]
dfTextStabilityScale <- data.frame(xpos=sort(c(1:2-0.3,1:2,1:2+0.3)),ypos=dfCombinedStabilityScale$labHeight,lab=dfCombinedStabilityScale$lab,Level=dfCombinedStabilityScale$Level)
a2 <- funPlot(dfCombinedStabilityScale,dfTextStabilityScale,0.4,"Standardized regression coefficient",F)

# barplot of scale yield model
dfRegYieldNationalScale <- data.frame(summary(modYieldNationalScale)$coefficients[2:3,c(1,2,4)])
names(dfRegYieldNationalScale) <- c("Effect","SE","pVal")
dfRegYieldNationalScale$nam <- c("Scale","Time")
dfRegYieldNationalScale$Level <- "National"

dfRegYieldSubnationalScale <- data.frame(summary(modYieldSubnationalScale)$coefficients[2:3,c(1,2,4)])
names(dfRegYieldSubnationalScale) <- c("Effect","SE","pVal")
dfRegYieldSubnationalScale$nam <- c("Scale","Time")
dfRegYieldSubnationalScale$Level <- "Subnational"

dfRegYieldFarmScale <- data.frame(summary(modYieldFarmScale)$coefficients[2:3,c(1,2,4)])
names(dfRegYieldFarmScale) <- c("Effect","SE","pVal")
dfRegYieldFarmScale$nam <- c("Scale","Time")
dfRegYieldFarmScale$Level <- "Farm"

dfCombinedYieldScale <- funCombine(dfRegYieldNationalScale,dfRegYieldSubnationalScale,dfRegYieldFarmScale)
dfCombinedYieldScale <- dfCombinedYieldScale[unlist(lapply(1:2,function(i)seq(i,6,2))),]
dfTextYieldScale <- data.frame(xpos=sort(c(1:2-0.3,1:2,1:2+0.3)),ypos=dfCombinedYieldScale$labHeight,lab=dfCombinedYieldScale$lab,Level=dfCombinedYieldScale$Level)
b2 <- funPlot(dfCombinedYieldScale,dfTextYieldScale,0.3,"Standardized regression coefficient",T)

# plot
jpeg("results/Fig2.jpeg", width = 16.9, height = 16.9*0.5, units = 'cm', res = 600)

ggarrange(a2,b2,
          labels = letters[1:2],font.label=list(size=8),
          ncol = 2, nrow = 1)

dev.off()

### Fig 3: raw effect of diversity and scales
## global
# stability
dfPredictNational <- data.frame(diversity=rep(0,1000),instabilityTemp=0,instabilityPrec=0,irrigation=0,fertilizer=0,timePeriod=0)
dfPredictSubnational <- data.frame(diversity=rep(0,1000),instabilityTemp=0,instabilityPrec=0,irrigation=0,fertilizer=0,timePeriod=0)
dfPredictFarm <- data.frame(diversity=rep(0,1000),instabilityTemp=0,instabilityPrec=0,irrigation=0,fertilizer=0,timePeriod=0)
dfPredictNationalScale <- data.frame(prop=rep(0,1000),timePeriod=0)
dfPredictSubnationalScale <- data.frame(prop=rep(0,1000),timePeriod=0)
dfPredictFarmScale <- data.frame(prop=rep(0,1000),timePeriod=0)

dfPredictStability <- rbind(
                  funPred(dfPredictNational,dfCenterNational,dfLogNational,modStabilityNational,"diversity","National"),
                  funPred(dfPredictSubnational,dfCenterSubnational,dfLogSubnational,modStabilitySubnational,"diversity","Subnational"),
                  funPred(dfPredictFarm,dfCenterFarm,dfLogFarm,modStabilityFarm,"diversity","Farm")
                  )

dfPredictStabilityScale <- rbind(
  funPred(dfPredictNationalScale,dfCenterNationalScale,dfLogNationalScale,modStabilityNationalScale,"prop","National"),
  funPred(dfPredictSubnationalScale,dfCenterSubnationalScale,dfLogSubnationalScale,modStabilitySubnationalScale,"prop","Subnational"),
  funPred(dfPredictFarmScale,dfCenterFarmScale,dfLogFarmScale,modStabilityFarmScale,"prop","Farm")
)

# yield
dfPredictYield <- rbind(
  funPred(dfPredictNational,dfCenterNational,dfLogNational,modYieldNational,"diversity","National"),
  funPred(dfPredictSubnational,dfCenterSubnational,dfLogSubnational,modYieldSubnational,"diversity","Subnational"),
  funPred(dfPredictFarm,dfCenterFarm,dfLogFarm,modYieldFarm,"diversity","Farm")
)

dfPredictYieldScale <- rbind(
  funPred(dfPredictNationalScale,dfCenterNationalScale,dfLogNationalScale,modYieldNationalScale,"prop","National"),
  funPred(dfPredictSubnationalScale,dfCenterSubnationalScale,dfLogSubnationalScale,modYieldSubnationalScale,"prop","Subnational"),
  funPred(dfPredictFarmScale,dfCenterFarmScale,dfLogFarmScale,modYieldFarmScale,"prop","Farm")
)

a3 <- funPlotPred(dfPredictStability,80,"Diversity","Yield stability",T)
# b3 <- funPlotPred(dfPredictYield,40000000,"Diversity","Yield",F)
b3 <- funPlotPred(dfPredictStabilityScale,80,"Scale","Yield stability",F)

# plot
jpeg("results/Fig3.jpeg", width = 16.9, height = 16.9*0.5, units = 'cm', res = 600)

ggarrange(a3,b3,
          labels = letters[1:2],font.label=list(size=8),
          ncol = 2, nrow = 1)

dev.off()



### Fig 4
# barplot of combined stability model
dfRegStabilityNationalCombined <- data.frame(summary(modStabilityNationalCombined)$coefficients[2:7,c(1,2,4)])
names(dfRegStabilityNationalCombined) <- c("Effect","SE","pVal")
dfRegStabilityNationalCombined$nam <- c("Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time")
dfRegStabilityNationalCombined$Level <- "National"

dfRegStabilitySubnationalCombined <- data.frame(summary(modStabilitySubnationalCombined)$coefficients[2:7,c(1,2,4)])
names(dfRegStabilitySubnationalCombined) <- c("Effect","SE","pVal")
dfRegStabilitySubnationalCombined$nam <- c("Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time")
dfRegStabilitySubnationalCombined$Level <- "Subnational"

dfRegStabilityFarmCombined <- data.frame(summary(modStabilityFarmCombined)$coefficients[2:7,c(1,2,4)])
names(dfRegStabilityFarmCombined) <- c("Effect","SE","pVal")
dfRegStabilityFarmCombined$nam <-c("Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time")
dfRegStabilityFarmCombined$Level <- "Farm"

dfCombinedStabilityCombined <- funCombine(dfRegStabilityNationalCombined,dfRegStabilitySubnationalCombined,dfRegStabilityFarmCombined)
dfCombinedStabilityCombined <- dfCombinedStabilityCombined[unlist(lapply(1:6,function(i)seq(i,18,6))),]
dfTextStabilityCombined <- data.frame(xpos=sort(c(1:6-0.3,1:6,1:6+0.3)),ypos=dfCombinedStabilityCombined$labHeight,lab=dfCombinedStabilityCombined$lab,Level=dfCombinedStabilityCombined$Level)
a4 <- funPlot(dfCombinedStabilityCombined,dfTextStabilityCombined,0.4,"Standardized regression coefficient",F)

# barplot of combined yield model
dfRegYieldNationalCombined <- data.frame(summary(modYieldNationalCombined)$coefficients[2:7,c(1,2,4)])
names(dfRegYieldNationalCombined) <- c("Effect","SE","pVal")
dfRegYieldNationalCombined$nam <- c("Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time")
dfRegYieldNationalCombined$Level <- "National"

dfRegYieldSubnationalCombined <- data.frame(summary(modYieldSubnationalCombined)$coefficients[2:7,c(1,2,4)])
names(dfRegYieldSubnationalCombined) <- c("Effect","SE","pVal")
dfRegYieldSubnationalCombined$nam <- c("Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time")
dfRegYieldSubnationalCombined$Level <- "Subnational"

dfRegYieldFarmCombined <- data.frame(summary(modYieldFarmCombined)$coefficients[2:7,c(1,2,4)])
names(dfRegYieldFarmCombined) <- c("Effect","SE","pVal")
dfRegYieldFarmCombined$nam <- c("Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time")
dfRegYieldFarmCombined$Level <- "Farm"

dfCombinedYieldCombined <- funCombine(dfRegYieldNationalCombined,dfRegYieldSubnationalCombined,dfRegYieldFarmCombined)
dfCombinedYieldCombined <- dfCombinedYieldCombined[unlist(lapply(1:6,function(i)seq(i,18,6))),]
dfTextYieldCombined <- data.frame(xpos=sort(c(1:6-0.3,1:6,1:6+0.3)),ypos=dfCombinedYieldCombined$labHeight,lab=dfCombinedYieldCombined$lab,Level=dfCombinedYieldCombined$Level)
b4 <- funPlot(dfCombinedYieldCombined,dfTextYieldCombined,0.6,"Standardized regression coefficient",T)

# plot
jpeg("results/Fig4.jpeg", width = 16.9, height = 16.9*0.5, units = 'cm', res = 600)

ggarrange(a4,b4,
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
dfNationalMean <- aggregate(cbind(ratioStability,ratioYield)~Country,dfNational,mean)
head(dfNationalMean)
dfNationalState <- aggregate(dfNational[,c("benefitStability","benefitYield")],list(dfNational$Country),function(i){sum(as.character(i)=="winner")/length(i)})
dfNationalMap <- merge(dfNationalMean,dfNationalState,by.x="Country",by.y="Group.1")
dfNationalMap$id <- as.character(dfNationalMap$Country)
head(dfNationalMap)

trintStabilityNational <- as.numeric(quantile(dfNationalMap$ratioStability,probs=seq(0,1,length.out = 4)))
trintYieldNational <- as.numeric(quantile(dfNationalMap$ratioYield,probs=seq(0,1,length.out = 4)))

dfNationalMap$dim1 <-car::recode(dfNationalMap$ratioStability,"trintStabilityNational[1]:trintStabilityNational[2]=1; trintStabilityNational[2]:trintStabilityNational[3]=2; trintStabilityNational[3]:trintStabilityNational[4]=3;")
dfNationalMap$dim2 <-car::recode(dfNationalMap$ratioYield,"trintYieldNational[1]:trintYieldNational[2]=1; trintYieldNational[2]:trintYieldNational[3]=2; trintYieldNational[3]:trintYieldNational[4]=3;")
dfNationalMap <- merge(dfNationalMap[,c("id","benefitStability","benefitYield","dim1","dim2")],grd,by=c("dim1","dim2"))
dfNationalMap$dim1 <-car::recode(dfNationalMap$benefitStability,"0:(1/3)=1; (1/3):(2/3)=2; (2/3):1=3;")
dfNationalMap$dim2 <-car::recode(dfNationalMap$benefitYield,"0:(1/3)=1; (1/3):(2/3)=2; (2/3):1=3;")
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
dfSubnationalMean <- aggregate(cbind(ratioStability,ratioYield)~RegionCode,dfSubnational,mean)
head(dfSubnationalMean)
dfSubnationalState <- aggregate(dfSubnational[,c("benefitStability","benefitYield")],list(dfSubnational$RegionCode),function(i){sum(as.character(i)=="winner")/length(i)})
dfSubnationalMap <- merge(dfSubnationalMean,dfSubnationalState,by.x="RegionCode",by.y="Group.1")
dfSubnationalMap$id <- as.character(dfSubnationalMap$RegionCode)
head(dfSubnationalMap)

trintStabilitySubnational <- as.numeric(quantile(dfSubnationalMap$ratioStability,probs=seq(0,1,length.out = 4)))
trintYieldSubnational <- as.numeric(quantile(dfSubnationalMap$ratioYield,probs=seq(0,1,length.out = 4)))

dfSubnationalMap$dim1 <-car::recode(dfSubnationalMap$ratioStability,"trintStabilitySubnational[1]:trintStabilitySubnational[2]=1; trintStabilitySubnational[2]:trintStabilitySubnational[3]=2; trintStabilitySubnational[3]:trintStabilitySubnational[4]=3;")
dfSubnationalMap$dim2 <-car::recode(dfSubnationalMap$ratioYield,"trintYieldSubnational[1]:trintYieldSubnational[2]=1; trintYieldSubnational[2]:trintYieldSubnational[3]=2; trintYieldSubnational[3]:trintYieldSubnational[4]=3;")
dfSubnationalMap <- merge(dfSubnationalMap[,c("id","benefitStability","benefitYield","dim1","dim2")],grd,by=c("dim1","dim2"))
dfSubnationalMap$dim1 <-car::recode(dfSubnationalMap$benefitStability,"0:(1/3)=1; (1/3):(2/3)=2; (2/3):1=3;")
dfSubnationalMap$dim2 <-car::recode(dfSubnationalMap$benefitYield,"0:(1/3)=1; (1/3):(2/3)=2; (2/3):1=3;")
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
dfFarmMean <- aggregate(cbind(ratioStability,ratioYield)~districtCode,dfFarm,mean)
head(dfFarmMean)
dfFarmState <- aggregate(dfFarm[,c("benefitStability","benefitYield")],list(dfFarm$districtCode),function(i){sum(as.character(i)=="winner")/length(i)})
dfFarmMap <- merge(dfFarmMean,dfFarmState,by.x="districtCode",by.y="Group.1")
dfFarmMap$id <- as.character(dfFarmMap$districtCode)
head(dfFarmMap)

trintStabilityFarm <- as.numeric(quantile(dfFarmMap$ratioStability,probs=seq(0,1,length.out = 4)))
trintYieldFarm <- as.numeric(quantile(dfFarmMap$ratioYield,probs=seq(0,1,length.out = 4)))

dfFarmMap$dim1 <-car::recode(dfFarmMap$ratioStability,"trintStabilityFarm[1]:trintStabilityFarm[2]=1; trintStabilityFarm[2]:trintStabilityFarm[3]=2; trintStabilityFarm[3]:trintStabilityFarm[4]=3;")
dfFarmMap$dim2 <-car::recode(dfFarmMap$ratioYield,"trintYieldFarm[1]:trintYieldFarm[2]=1; trintYieldFarm[2]:trintYieldFarm[3]=2; trintYieldFarm[3]:trintYieldFarm[4]=3;")
dfFarmMap <- merge(dfFarmMap[,c("id","benefitStability","benefitYield","dim1","dim2")],grd,by=c("dim1","dim2"))
dfFarmMap$dim1 <-car::recode(dfFarmMap$benefitStability,"0:(1/3)=1; (1/3):(2/3)=2; (2/3):1=3;")
dfFarmMap$dim2 <-car::recode(dfFarmMap$benefitYield,"0:(1/3)=1; (1/3):(2/3)=2; (2/3):1=3;")
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
  labs(x="Stability benefit",y="     Yield benefit") 
# theme(axis.title=element_text(size=8))
library(grid)
vp<-viewport(width=0.12,height=0.3,x=0.05,y=0.16)


jpeg("results/Fig3.jpeg", width = 16.9, height = 16.9/((2.074088+1.480554+1.177826)*0.5),units = 'cm', res = 300)
  plot_grid(a3, b3,c3,d3,e3,f3,
            labels = c(letters[1:6]),font.label=list(size=8),
            nrow = 2, ncol=3, rel_widths = c(2.074088, 1.480554,1.177826))
  print(g.legend,vp=vp)
dev.off()

























rm(list=ls())


