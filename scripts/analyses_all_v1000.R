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


lev <- c("Yield stability","Yield")
myColors <- c("#4daf4a","#045A8D")
source("scripts/functionsAnalyses.R")

##################### Regression local

###### National level
### preparation
# read df
dfNational <- read.csv("datasetsDerived/dataFinal_global.csv")

# remove outliers
hist(dfNational$stability)

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
# detect and remove outliersbased on 4/n criteria
# dfCenterNational$cooksd <- cooks.distance(modStabilityNational)
# modStabilityNational <- lm(stability~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterNational[which(dfCenterNational$cooksd < 4/nrow(dfCenterNational)),])
# summary(modStabilityNational)

modYieldNational <- lm(yield~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterNational)
summary(modYieldNational)


###### SUBNATIONAL LEVEL
### preparation
dfSubnational <- read.csv("datasetsDerived/dataFinal_europe.csv")
dfSubnational <- merge(dfSubnational,dfNational[,c("Country","timePeriod","meanNitrogen","meanIrrigation_share")],by=c("Country","timePeriod"))

# remove outliers
hist(dfSubnational$stability)
# dfSubnational <- dfSubnational[-which(dfSubnational$stability%in%boxplot.stats(dfSubnational$stability)$out),]

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
dfCenterSubnational=data.frame(Area=dfLogSubnational[,1],stability=dfLogSubnational[,2],yield=dfLogSubnational[,3],dfPredictorsSubnational)
head(dfCenterSubnational)

# regression models
modStabilitySubnational <- lm(stability~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterSubnational)
summary(modStabilitySubnational)

modYieldSubnational <- lm(yield~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterSubnational)
summary(modYieldSubnational)


####### FARM LEVEL
### preparation
dfFarm <- read.csv("P:/dataFinal_farmlevel.csv")

# remove outliers
hist(dfFarm$stability)

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
dfCenterFarm=data.frame(Farm=dfLogFarm[,1],stability=dfLogFarm[,2],yield=dfLogFarm[,3],dfPredictorsFarm)
head(dfCenterFarm)

# regression models
modStabilityFarm <- lm(stability~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterFarm)
summary(modStabilityFarm)

modYieldFarm <- lm(yield~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterFarm)
summary(modYieldFarm)


##################### Regression scale
###### National level
### preparation
dfNationalScale <- read.csv("datasetsDerived/dataScales_global.csv")
dfNationalScale <- dfNationalScale[,c("Area","timePeriod","stability","yield","areaHarvested","prop")]
dfNationalScale <- unique(dfNationalScale)
names(dfNationalScale)[1] <- "Country"

# add original data
dfNationalRed <- dfNational
dfNationalRed$prop <- 0
dfNationalScaleFinal <- rbind(dfNationalRed[,c("Country","timePeriod","stability","yield","areaHarvested")],dfNationalScale[,c("Country","timePeriod","stability","yield","areaHarvested")])

### regression analyses
## transformations
dfLogNationalScale=with(dfNationalScaleFinal,data.frame(Country,
                                        stability = log(stability),
                                        yield = log(yield),
                                        areaHarvested, 
                                        timePeriod
))

# scale predictors for standardized regression
dfPredictorsNationalScale=sapply(dfLogNationalScale[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterNationalScale=data.frame(Country=dfLogNationalScale[,1],stability=dfLogNationalScale[,2],yield=dfLogNationalScale[,3],dfPredictorsNationalScale)
head(dfCenterNationalScale)

## regression models
modStabilityNationalScale <- lm(stability~areaHarvested+timePeriod,dfCenterNationalScale)
summary(modStabilityNationalScale)

modYieldNationalScale <- lm(yield~areaHarvested+timePeriod,dfCenterNationalScale)
summary(modYieldNationalScale)

###### European level
### preparation
dfSubnationalScale <- read.csv("datasetsDerived/dataScales_europe.csv")
dfSubnationalScale <- dfSubnationalScale[,c("Area","timePeriod","stability","yield","areaHarvested","prop")]
dfSubnationalScale <- unique(dfSubnationalScale)
names(dfSubnationalScale)[1] <- "RegionCode"

# add original data
dfSubnationalRed <- dfSubnational
dfSubnationalRed$prop <- 0
dfSubnationalScaleFinal <- rbind(dfSubnationalRed[,c("RegionCode","timePeriod","stability","yield","areaHarvested")],dfSubnationalScale[,c("RegionCode","timePeriod","stability","yield","areaHarvested")])

### regression analyses
## transformations
dfLogSubnationalScale=with(dfSubnationalScale,data.frame(RegionCode,
                                                      stability = log(stability),
                                                      yield = log(yield),
                                                      areaHarvested, 
                                                      timePeriod
))

# scale predictors for standardized regression
dfPredictorsSubnationalScale=sapply(dfLogSubnationalScale[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterSubnationalScale=data.frame(RegionCode=dfLogSubnationalScale[,1],stability=dfLogSubnationalScale[,2],yield=dfLogSubnationalScale[,3],dfPredictorsSubnationalScale)
head(dfCenterSubnationalScale)

## regression models
modStabilitySubnationalScale <- lm(stability~areaHarvested+timePeriod,dfCenterSubnationalScale)
summary(modStabilitySubnationalScale)

modYieldSubnationalScale <- lm(yield~areaHarvested+timePeriod,dfCenterSubnationalScale)
summary(modYieldSubnationalScale)

###### Farm level
### preparation
dfFarmScale <- read.csv("datasetsDerived/dataScales_farm.csv")
dfFarmScale <- dfFarmScale[,c("Area","timePeriod","stability","yield","areaHarvested","prop")]
dfFarmScale <- unique(dfFarmScale)
names(dfFarmScale)[1] <- "Farmer"

# add original data
dfFarmRed <- dfFarm
dfFarmRed$prop <- 0
dfFarmScaleFinal <- rbind(dfFarmRed[,c("Farmer","timePeriod","stability","yield","areaHarvested")],dfFarmScale[,c("Farmer","timePeriod","stability","yield","areaHarvested")])

### regression analyses
## transformations
dfLogFarmScale=with(dfFarmScaleFinal,data.frame(Farmer,
                                              stability = log(stability),
                                              yield = log(yield),
                                              areaHarvested, 
                                              timePeriod
))

# scale predictors for standardized regression
dfPredictorsFarmScale=sapply(dfLogFarmScale[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterFarmScale=data.frame(Country=dfLogFarmScale[,1],stability=dfLogFarmScale[,2],yield=dfLogFarmScale[,3],dfPredictorsFarmScale)
head(dfCenterFarmScale)

## regression models
modStabilityFarmScale <- lm(stability~areaHarvested+timePeriod,dfCenterFarmScale)
summary(modStabilityFarmScale)

modYieldFarmScale <- lm(yield~areaHarvested+timePeriod,dfCenterFarmScale)
summary(modYieldFarmScale)

##################### Regression combined
###### National level
### preparation

# get extremes
dfNationalCombined <- merge(dfNationalRed,dfNationalScale[which(dfNationalScale$prop==1),],by=c("Country","timePeriod"))
head(dfNationalCombined)
dfNationalCombined$slopeStability <- dfNationalCombined$stability.y/dfNationalCombined$stability.x
dfNationalCombined$slopeYield <- dfNationalCombined$yield.y/dfNationalCombined$yield.x

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
dfSubnationalCombined <- merge(dfSubnationalRed,dfSubnationalScale[which(dfSubnationalScale$prop==1),],by=c("RegionCode","timePeriod"))
head(dfSubnationalCombined)
dfSubnationalCombined$slopeStability <- dfSubnationalCombined$stability.y/dfSubnationalCombined$stability.x
dfSubnationalCombined$slopeYield <- dfSubnationalCombined$yield.y/dfSubnationalCombined$yield.x

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
dfFarmCombined <- merge(dfFarmRed,dfFarmScale[which(dfFarmScale$prop==1),],by=c("Farmer","timePeriod"))
head(dfFarmCombined)
dfFarmCombined$slopeStability <- dfFarmCombined$stability.y/dfFarmCombined$stability.x
dfFarmCombined$slopeYield <- dfFarmCombined$yield.y/dfFarmCombined$yield.x

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
a1 <- funCombinePlot(modStabilityNational,modStabilityNationalScale,F,"National",myColors[1],0.4,"Yield stability")
b1 <- funCombinePlot(modStabilitySubnational,modStabilitySubnationalScale,F,"Subnational",myColors[2],0.4,"")
c1 <- funCombinePlot(modStabilityFarm,modStabilityFarmScale,F,"Farm",myColors[3],0.4,"")
d1 <- funCombinePlot(modYieldNational,modYieldNationalScale,T,"",myColors[1],0.6,"Yield")
e1 <- funCombinePlot(modYieldSubnational,modYieldSubnationalScale,T,"",myColors[2],0.6,"")
f1 <- funCombinePlot(modYieldFarm,modYieldFarmScale,T,"",myColors[3],0.6,"")

# plot
jpeg("results/Fig1.jpeg", width = 16.9, height = 16.9*0.5, units = 'cm', res = 600)

ggarrange(a1,b1,c1,d1,e1,f1,
          labels = letters[1:6],font.label=list(size=8),
          ncol = 3, nrow = 2,heights=c(0.4,0.6))

dev.off()

### Fig 2
# barplot of combined stability model
a2 <- funCombinePlot(modStabilityNationalCombined,NULL,F,"National",myColors[1],0.4,"Yield stability ratio")
b2 <- funCombinePlot(modStabilitySubnationalCombined,NULL,F,"Subnational",myColors[2],0.4,"")
c2 <- funCombinePlot(modStabilityFarmCombined,NULL,F,"Farm",myColors[3],0.4,"")
d2 <- funCombinePlot(modYieldNationalCombined,NULL,T,"",myColors[1],0.6,"Yield ratio")
e2 <- funCombinePlot(modYieldSubnationalCombined,NULL,T,"",myColors[2],0.6,"")
f2 <- funCombinePlot(modYieldFarmCombined,NULL,T,"",myColors[3],0.6,"")

# plot
jpeg("results/Fig2.jpeg", width = 16.9, height = 16.9*0.5, units = 'cm', res = 600)

ggarrange(a2,b2,c2,d2,e2,f2,
          labels = letters[1:6],font.label=list(size=8),
          ncol = 3, nrow = 2,heights=c(0.4,0.6))

dev.off()



### Fig 3: predict if scale minimization can be buffered by diversification
dfPredict <- dfCenterNationalCombined[which(dfCenterNationalCombined$timePeriod==max(dfCenterNationalCombined$timePeriod)),c("Country","diversity","irrigation","fertilizer","instabilityTemp","instabilityPrec","timePeriod")]
dfPredict$diversity <- max(dfPredict$diversity)


dfPredict <-  cbind(dfPredict,exp(data.frame(predict(modStabilityNationalCombined,newdata = dfPredict[,2:7],interval = "confidence"))))
dfPredict <- merge(dfPredict, dfNationalCombined[which(dfNationalCombined$timePeriod==2008),c("Country","slopeStability")])
mean(dfPredict$slopeStability)
mean(dfPredict$fit)
sum(dfPredict$slopeStability<1)
sum(dfPredict$fit<1)
hist(dfPredict$fit)
dfPredict$rationSlope <- dfPredict$fit/dfPredict$slopeStability
hist(dfPredict$rationSlope)

### Fig 3: raw effect of diversity and scales
## global
# stability
dfPredictNational <- data.frame(diversity=rep(0,1000),instabilityTemp=0,instabilityPrec=0,irrigation=0,fertilizer=0,timePeriod=0)
dfPredictSubnational <- data.frame(diversity=rep(0,1000),instabilityTemp=0,instabilityPrec=0,irrigation=0,fertilizer=0,timePeriod=0)
dfPredictFarm <- data.frame(diversity=rep(0,1000),instabilityTemp=0,instabilityPrec=0,irrigation=0,fertilizer=0,timePeriod=0)
dfPredictNationalScale <- data.frame(areaHarvested=rep(0,1000),timePeriod=0)
dfPredictSubnationalScale <- data.frame(areaHarvested=rep(0,1000),timePeriod=0)
dfPredictFarmScale <- data.frame(areaHarvested=rep(0,1000),timePeriod=0)

dfPredictStability <- rbind(
                  funPred(dfPredictNational,dfCenterNational,dfLogNational,modStabilityNational,"diversity","National"),
                  funPred(dfPredictSubnational,dfCenterSubnational,dfLogSubnational,modStabilitySubnational,"diversity","Subnational"),
                  funPred(dfPredictFarm,dfCenterFarm,dfLogFarm,modStabilityFarm,"diversity","Farm")
                  )

dfPredictStabilityScale <- rbind(
  funPred(dfPredictNationalScale,dfCenterNationalScale,dfLogNationalScale,modStabilityNationalScale,"areaHarvested","National"),
  funPred(dfPredictSubnationalScale,dfCenterSubnationalScale,dfLogSubnationalScale,modStabilitySubnationalScale,"areaHarvested","Subnational"),
  funPred(dfPredictFarmScale,dfCenterFarmScale,dfLogFarmScale,modStabilityFarmScale,"areaHarvested","Farm")
)

# yield
dfPredictYield <- rbind(
  funPred(dfPredictNational,dfCenterNational,dfLogNational,modYieldNational,"diversity","National"),
  funPred(dfPredictSubnational,dfCenterSubnational,dfLogSubnational,modYieldSubnational,"diversity","Subnational"),
  funPred(dfPredictFarm,dfCenterFarm,dfLogFarm,modYieldFarm,"diversity","Farm")
)

dfPredictYieldScale <- rbind(
  funPred(dfPredictNationalScale,dfCenterNationalScale,dfLogNationalScale,modYieldNationalScale,"areaHarvested","National"),
  funPred(dfPredictSubnationalScale,dfCenterSubnationalScale,dfLogSubnationalScale,modYieldSubnationalScale,"areaHarvested","Subnational"),
  funPred(dfPredictFarmScale,dfCenterFarmScale,dfLogFarmScale,modYieldFarmScale,"areaHarvested","Farm")
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


