library(ggplot2)
library(ggpubr)
# library(sf)
library(rgdal)
# library(raster)
library(plyr)
library(RColorBrewer)
library(nlme)
library(MuMIn)
library(countrycode)
library(car)
library(grid)

## globals

lev <- c("Country","Region","Farm")
myColors <- c("#4daf4a","#045A8D", "#ff7f00")
names(myColors) <- factor(lev,levels=lev)

vecColMap <- brewer.pal(9,"PuOr")
vecColMap[5] <- "gray75"

source("scripts/functionsAnalyses.R")

##################### Read data

###### Country level
dfCountry <- read.csv("datasetsDerived/dataFinal_global.csv")
hist(dfCountry$stability)
dfCountry <- dfCountry[-which(dfCountry$stability%in%boxplot.stats(dfCountry$stability)$out),]

dfCountryScale <- read.csv("datasetsDerived/dataScales_global.csv")
dfCountryScale <- dfCountryScale[,c("Level","timePeriod","stability","yield","areaHarvested","prop")]
dfCountryScale <- unique(dfCountryScale)
names(dfCountryScale)[1] <- "Country"
dfCountryScale <- dfCountryScale[-which(dfCountryScale$stability%in%boxplot.stats(dfCountryScale$stability)$out),]


dfCountryScale$class<-cut(dfCountryScale$areaHarvested, seq(from=min(dfCountryScale$areaHarvested),to=max(dfCountryScale$areaHarvested),length.out=10), right=FALSE, labels=c(1:9))
table(dfCountryScale$class)
dfCountryScale$class <- as.numeric(cut2(dfCountryScale$areaHarvested, g=10))
boxplot(stability~class,dfCountryScale)

###### Regional level
dfRegion <- read.csv("datasetsDerived/dataFinal_europe.csv")
dfRegion <- merge(dfRegion,dfCountry[,c("Country","timePeriod","fertilizer","irrigation")],by=c("Country","timePeriod"))
hist(dfRegion$stability)
dfRegion <- dfRegion[-which(dfRegion$stability%in%boxplot.stats(dfRegion$stability)$out),]

dfRegionScale <- read.csv("datasetsDerived/dataScales_europe.csv")
dfRegionScale <- dfRegionScale[,c("Level","timePeriod","stability","yield","areaHarvested","prop")]
dfRegionScale <- unique(dfRegionScale)
names(dfRegionScale)[1] <- "Region"
dfRegionScale <- dfRegionScale[-which(dfRegionScale$stability%in%boxplot.stats(dfRegionScale$stability)$out),]

###### Farm level
dfFarm <- read.csv("P:/dataFinal_farmlevel.csv")
hist(dfFarm$stability)
dfFarm <- dfFarm[-which(dfFarm$stability%in%boxplot.stats(dfFarm$stability)$out),]

dfFarmScale <- read.csv("P:/dataScales_farm.csv")
dfFarmScale <- dfFarmScale[,c("Level","timePeriod","stability","yield","areaHarvested","prop")]
dfFarmScale <- unique(dfFarmScale)
names(dfFarmScale)[1] <- "Farm"
dfFarmScale <- dfFarmScale[-which(dfFarmScale$stability%in%boxplot.stats(dfFarmScale$stability)$out),]


##################### Regression scale
###### Country level
### preparation
# add original data
dfCountryRed <- dfCountry
dfCountryRed$prop <- 0
dfCountryScaleFinal <- rbind(dfCountryRed[,c("Country","timePeriod","stability","yield","areaHarvested")],dfCountryScale[,c("Country","timePeriod","stability","yield","areaHarvested")])

# change area harvested to mio ha
dfCountryScaleFinal$areaHarvested <- dfCountryScaleFinal$areaHarvested/1000000

### regression analyses
## transformations
dfLogCountryScale=with(dfCountryScaleFinal,data.frame(Country,
                                        stability = log(stability),
                                        yield = log(yield),
                                        areaHarvested=areaHarvested, 
                                        timePeriod
))

## linear model
modStabilityCountryScale <- lm(stability~areaHarvested+timePeriod,dfLogCountryScale)
summary(modStabilityCountryScale)
modYieldCountryScale <- lm(yield~areaHarvested+timePeriod,dfLogCountryScale)
summary(modYieldCountryScale)

## mixed effect model
dfLogCountryScale$timePeriod <- factor(dfLogCountryScale$timePeriod,levels=c("1968","1978","1988","1998","2008"))
modStabilityCountryScaleMixed=lme(stability~areaHarvested,data=dfLogCountryScale,random=~areaHarvested|timePeriod,method="ML")
summary(modStabilityCountryScaleMixed)
AICc(modStabilityCountryScale,modStabilityCountryScaleMixed) # mixed effects model makes sense
modStabilityCountryScaleMixedFixed=fixef(modStabilityCountryScaleMixed)
modStabilityCountryScaleMixedTime <-   coef(modStabilityCountryScaleMixed)
r.squaredGLMM(modStabilityCountryScaleMixed) 

modYieldCountryScaleMixed=lme(yield~areaHarvested,data=dfLogCountryScale,random=~areaHarvested|timePeriod,method="ML")
summary(modYieldCountryScaleMixed)
AICc(modYieldCountryScale,modYieldCountryScaleMixed) # mixed effects model makes sense
modYieldCountryScaleMixedFixed=fixef(modYieldCountryScaleMixed)
modYieldCountryScaleMixedTime <-   coef(modYieldCountryScaleMixed)
r.squaredGLMM(modYieldCountryScaleMixed) 


###### European level
### preparation
# add original data
dfRegionRed <- dfRegion
dfRegionRed$prop <- 0
dfRegionScaleFinal <- rbind(dfRegionRed[,c("Region","timePeriod","stability","yield","areaHarvested")],dfRegionScale[,c("Region","timePeriod","stability","yield","areaHarvested")])

# change area harvested to mio ha
dfRegionScaleFinal$areaHarvested <- dfRegionScaleFinal$areaHarvested/1000000

### regression analyses
## transformations
dfLogRegionScale=with(dfRegionScaleFinal,data.frame(Region,
                                                      stability = log(stability),
                                                      yield = log(yield),
                                                      areaHarvested, 
                                                      timePeriod
))

## regression models
modStabilityRegionScale <- lm(stability~areaHarvested+timePeriod,dfLogRegionScale)
summary(modStabilityRegionScale)
modYieldRegionScale <- lm(yield~areaHarvested+timePeriod,dfLogRegionScale)
summary(modYieldRegionScale)

## mixed effect model
dfLogRegionScale$timePeriod <- factor(dfLogRegionScale$timePeriod,levels=c("1978","1988","1998","2008"))
modStabilityRegionScaleMixed=lme(stability~areaHarvested,data=dfLogRegionScale,random=~areaHarvested|timePeriod,method="ML")
summary(modStabilityRegionScaleMixed)
AICc(modStabilityRegionScale,modStabilityRegionScaleMixed) # mixed effects model makes sense
modStabilityRegionScaleMixedFixed=fixef(modStabilityRegionScaleMixed)
modStabilityRegionScaleMixedTime <-   coef(modStabilityRegionScaleMixed)
r.squaredGLMM(modStabilityRegionScaleMixed) 

modYieldRegionScaleMixed=lme(yield~areaHarvested,data=dfLogRegionScale,random=~areaHarvested|timePeriod,method="ML")
summary(modYieldRegionScaleMixed)
AICc(modYieldRegionScale,modYieldRegionScaleMixed) # mixed effects model makes sense
modYieldRegionScaleMixedFixed=fixef(modYieldRegionScaleMixed)
modYieldRegionScaleMixedTime <-   coef(modYieldRegionScaleMixed)
r.squaredGLMM(modYieldRegionScaleMixed) 

###### Farm level
### preparation
# add original data
dfFarmRed <- dfFarm
dfFarmRed$prop <- 0
dfFarmScaleFinal <- rbind(dfFarmRed[,c("Farm","timePeriod","stability","yield","areaHarvested")],dfFarmScale[,c("Farm","timePeriod","stability","yield","areaHarvested")])

# change area harvested to mio ha
dfFarmScaleFinal$areaHarvested <- dfFarmScaleFinal$areaHarvested/1000000

### regression analyses
## transformations
dfLogFarmScale=with(dfFarmScaleFinal,data.frame(Farm,
                                              stability = log(stability),
                                              yield = log(yield),
                                              areaHarvested, 
                                              timePeriod
))

## regression models
modStabilityFarmScale <- lm(stability~areaHarvested+timePeriod,dfLogFarmScale)
summary(modStabilityFarmScale)
modYieldFarmScale <- lm(yield~areaHarvested+timePeriod,dfLogFarmScale)
summary(modYieldFarmScale)

## mixed effect model
dfLogFarmScale$timePeriod <- factor(dfLogFarmScale$timePeriod,levels=c("1998","2008"))
modStabilityFarmScaleMixed=lme(stability~areaHarvested,data=dfLogFarmScale,random=~areaHarvested|timePeriod,method="ML")
summary(modStabilityFarmScaleMixed)
AICc(modStabilityFarmScale,modStabilityFarmScaleMixed) # mixed effects model makes sense
modStabilityFarmScaleMixedFixed=fixef(modStabilityFarmScaleMixed)
modStabilityFarmScaleMixedTime <-   coef(modStabilityFarmScaleMixed)
r.squaredGLMM(modStabilityFarmScaleMixed) 

modYieldFarmScaleMixed=lme(yield~areaHarvested,data=dfLogFarmScale,random=~areaHarvested|timePeriod,method="ML")
summary(modYieldFarmScaleMixed)
AICc(modYieldFarmScale,modYieldFarmScaleMixed) # mixed effects model makes sense
modYieldFarmScaleMixedFixed=fixef(modYieldFarmScaleMixed)
modYieldFarmScaleMixedTime <-   coef(modYieldFarmScaleMixed)
r.squaredGLMM(modYieldFarmScaleMixed) 



##################### Regression combined
###### Country level
### preparation
# get extremes
dfCountryCombined <- merge(dfCountryRed,dfCountryScale[which(dfCountryScale$prop==1),],by=c("Country","timePeriod"))
head(dfCountryCombined)
dfCountryCombined$slopeStability <- dfCountryCombined$stability.y/dfCountryCombined$stability.x
sum(dfCountryCombined$timePeriod==2008&dfCountryCombined$slopeStability<1)/sum(dfCountryCombined$timePeriod==2008)
dfCountryCombined$slopeYield <- dfCountryCombined$yield.y/dfCountryCombined$yield.x

### regression analyses
## transformations
dfLogCountryCombined=with(dfCountryCombined,data.frame(Country,
                                         slopeStability = log(slopeStability),
                                         slopeYield = log(slopeYield),
                                         diversity, 
                                         irrigation=sqrt(irrigation),
                                         fertilizer=sqrt(fertilizer),
                                         instabilityTemp,instabilityPrec,
                                         timePeriod
))

# scale predictors for standardized regression
dfPredictorsCountryCombined=sapply(dfLogCountryCombined[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterCountryCombined=data.frame(Country=dfLogCountryCombined[,1],slopeStability=dfLogCountryCombined[,2],slopeYield=dfLogCountryCombined[,3],dfPredictorsCountryCombined)
head(dfCenterCountryCombined)

## regression models
modStabilityCountryCombined <- lm(slopeStability~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterCountryCombined)
summary(modStabilityCountryCombined)
modStabilityCountryCombinedRaw <- lm(slopeStability~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfLogCountryCombined)
summary(modStabilityCountryCombinedRaw)

modYieldCountryCombined <- lm(slopeYield~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterCountryCombined)
summary(modYieldCountryCombined)
modYieldCountryCombinedRaw <- lm(slopeYield~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfLogCountryCombined)
summary(modYieldCountryCombinedRaw)

###### European level
### preparation
# get extremes
dfRegionCombined <- merge(dfRegionRed,dfRegionScale[which(dfRegionScale$prop==1),],by=c("Region","timePeriod"))
head(dfRegionCombined)
dfRegionCombined$slopeStability <- dfRegionCombined$stability.y/dfRegionCombined$stability.x
sum(dfRegionCombined$timePeriod==2008&dfRegionCombined$slopeStability<1)/sum(dfRegionCombined$timePeriod==2008)
dfRegionCombined$slopeYield <- dfRegionCombined$yield.y/dfRegionCombined$yield.x

### regression analyses
## transformations
dfLogRegionCombined=with(dfRegionCombined,data.frame(Region,
                                                         slopeStability = log(slopeStability),
                                                         slopeYield = log(slopeYield),
                                                         diversity, 
                                                         irrigation=sqrt(irrigation),
                                                         fertilizer=sqrt(fertilizer),
                                                         instabilityTemp,instabilityPrec,
                                                         timePeriod
))

# scale predictors for standardized regression
dfPredictorsRegionCombined=sapply(dfLogRegionCombined[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterRegionCombined=data.frame(Country=dfLogRegionCombined[,1],slopeStability=dfLogRegionCombined[,2],slopeYield=dfLogRegionCombined[,3],dfPredictorsRegionCombined)
head(dfCenterRegionCombined)

## regression models
modStabilityRegionCombined <- lm(slopeStability~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterRegionCombined)
summary(modStabilityRegionCombined)
modStabilityRegionCombinedRaw <- lm(slopeStability~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfLogRegionCombined)
summary(modStabilityRegionCombinedRaw)

modYieldRegionCombined <- lm(slopeYield~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterRegionCombined)
summary(modYieldRegionCombined)
modYieldRegionCombinedRaw <- lm(slopeYield~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfLogRegionCombined)
summary(modYieldRegionCombinedRaw)

###### Farm level
### preparation

# get extremes
dfFarmCombined <- merge(dfFarmRed,dfFarmScale[which(dfFarmScale$prop==1),],by=c("Farm","timePeriod"))
dfFarmCombined <- dfFarmCombined[which(dfFarmCombined$Farm%in%unique(dfFarmScaleFinal$Farm)),]
head(dfFarmCombined)
dfFarmCombined$slopeStability <- dfFarmCombined$stability.y/dfFarmCombined$stability.x
sum(dfFarmCombined$timePeriod==2008&dfFarmCombined$slopeStability<1)/sum(dfFarmCombined$timePeriod==2008)
dfFarmCombined$slopeYield <- dfFarmCombined$yield.y/dfFarmCombined$yield.x

### regression analyses
## transformations
dfLogFarmCombined=with(dfFarmCombined,data.frame(Farm,
                                                         slopeStability = log(slopeStability),
                                                         slopeYield = log(slopeYield),
                                                         diversity, 
                                                         irrigation=sqrt(irrigation),
                                                         fertilizer=sqrt(fertilizer),
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
modStabilityFarmCombinedRaw <- lm(slopeStability~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfLogFarmCombined)
summary(modStabilityFarmCombinedRaw)

modYieldFarmCombined <- lm(slopeYield~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterFarmCombined)
summary(modYieldFarmCombined)
modYieldFarmCombinedRaw <- lm(slopeYield~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfLogFarmCombined)
summary(modYieldFarmCombinedRaw)

###### FIGURES

### Fig 1: scale model
vecColors <- brewer.pal(5,"PuBu")

a1 <- funFig1(dfLogCountryScale,dfLogCountryScale$stability,modStabilityCountryScaleMixedTime,vecColors,T)
b1 <- funFig1(dfLogRegionScale,dfLogRegionScale$stability,modStabilityRegionScaleMixedTime,vecColors[2:5],F)
c1 <- funFig1(dfLogFarmScale,dfLogFarmScale$stability,modStabilityFarmScaleMixedTime,vecColors[4:5],F)

as1 <- funFig1(dfLogCountryScale,dfLogCountryScale$yield,modYieldCountryScaleMixedTime,vecColors,T)
bs1 <- funFig1(dfLogRegionScale,dfLogRegionScale$yield,modYieldRegionScaleMixedTime,vecColors[2:5],F)
cs1 <- funFig1(dfLogFarmScale,dfLogFarmScale$yield,modYieldFarmScaleMixedTime,vecColors[4:5],F)

# plot
jpeg("results/Fig1.jpeg", width = 16.9, height = 16.9*0.3, units = 'cm', res = 600)
  ggarrange(a1,b1,c1,
            labels = letters[1:3],font.label=list(size=8),
            ncol = 3, nrow = 1,align="h")
dev.off()
jpeg("results/FigS1.jpeg", width = 16.9, height = 16.9*0.3, units = 'cm', res = 600)
  ggarrange(as1,bs1,cs1,
            labels = letters[1:3],font.label=list(size=8),
            ncol = 3, nrow = 1,align="h")
dev.off()


### Fig 2
g.legend <- ggplot(grd, aes(dim1,dim2,fill=factor(1:9)))+
  geom_tile()+
  scale_fill_manual(values=grd$color)+
  theme_void()+
  theme(legend.position="none",axis.title=element_text(size=5),
        panel.background=element_blank(),plot.margin=margin(t=10,b=10,l=10))+
  theme(axis.title=element_text(color="black",size=8),
        axis.title.y = element_text(angle = 90))+
  labs(x="Stability level",y="Stability overall") 
# theme(axis.title=element_text(size=8))

vp<-viewport(width=0.24,height=0.4,x=0.12,y=0.3)


mapCountry <- readOGR("spatial/countries_global.shp")
mapCountry$Country <-  countrycode(mapCountry$Area, 'country.name', 'iso3c')
dfCountryAgg <- aggregate(cbind(stability.x,stability.y)~Country,dfCountryCombined,mean)

mapRegion <- readOGR("spatial/regions_europe.shp")
mapRegion$Region <- mapRegion$NUTS_ID
dfRegionAgg <- aggregate(cbind(stability.x,stability.y)~Region,dfRegionCombined,mean)

a2 <- funFig2(dfCountryAgg,"stability",mapCountry,"Country")
b2 <- funFig2(dfRegionAgg,"stability",mapRegion,"Region")

# jpeg("results/Fig2.jpeg", width = 16.9, height = 16.9*0.3, units = 'cm', res = 600)
jpeg("results/Fig2.jpeg", res = 600)

ggarrange(a2,b2,
          labels = letters[1:3],font.label=list(size=8),
          ncol = 1, nrow = 2,align="h")
print(g.legend,vp=vp)
# grid.arrange(a2, b2, nrow = 2)
dev.off()




merge_with_shape <- function(df, shape, by) {
  merge(shape, df, by=by, sort=FALSE) %>% st_as_sf
}


plotG <- ggplot(df, aes(fill = as.numeric(variable_to_plot)))+
  geom_sf(color = "grey", size = 0.001)+
  coord_sf(xlim = c(-125, -60), ylim = c(20,50))+
  scale_fill_gradient(low = "#ef8a62", high = "#67a9cf")+
  theme_bw()

US_EU_N <- grid.arrange(EUplot, USplot, nrow = 2)


mapCountry <- getMap()

mapCountry$Country <- mapCountry$ISO3
mapCountry <- fortify(mapCountry,region="ISO3")

mapCountryStability = join(mapCountry, dfCountryCombined[,c("Country","stability.x","stability.y")], by="Country")
a3 <- funMapPlot(mapCountryStability,mapCountryStability$dim1)

### Fig 3
# barplot of combined stability model
a2 <- funCombinePlot(modStabilityCountryCombined,NULL,T,"Country",myColors[1],0.4,"Yield stability ratio")
b2 <- funCombinePlot(modStabilityRegionCombined,NULL,T,"Region",myColors[2],0.4,"")
c2 <- funCombinePlot(modStabilityFarmCombined,NULL,T,"Farm",myColors[3],0.4,"")
as2 <- funCombinePlot(modYieldCountryCombined,NULL,T,"Country",myColors[1],0.6,"Yield ratio")
bs2 <- funCombinePlot(modYieldRegionCombined,NULL,T,"Region",myColors[2],0.6,"")
cs2 <- funCombinePlot(modYieldFarmCombined,NULL,T,"Farm",myColors[3],0.6,"")

# plot
jpeg("results/Fig3.jpeg", width = 16.9, height = 16.9*0.3, units = 'cm', res = 600)
  ggarrange(a2,b2,c2,
            labels = letters[1:3],font.label=list(size=8),
            ncol = 3, nrow = 1)
dev.off()
jpeg("results/FigS3.jpeg", width = 16.9, height = 16.9*0.3, units = 'cm', res = 600)
  ggarrange(as2,bs2,cs2,
            labels = letters[1:3],font.label=list(size=8),
            ncol = 3, nrow = 1)
dev.off()


### Fig 4: predict if scale minimization can be buffered by diversification
# spatial
mapCountry <- readOGR("spatial/countries_global.shp")
mapCountry$Area <- factor(as.character(mapCountry$Area))
mapCountry <- fortify(mapCountry,region="Area")
emptyPlot <- ggplot(data.frame())+theme_void()

dfPredict <- dfCenterCountryCombined[which(dfCenterCountryCombined$timePeriod==max(dfCenterCountryCombined$timePeriod)),c("Country","diversity","irrigation","fertilizer","instabilityTemp","instabilityPrec","timePeriod")]

## stability
dfPredictStability <- funPredictCountry(dfPredict,modStabilityCountryCombined,"slopeStability")

mapCountryStability = join(mapCountry, dfPredictStability[,c("id","dim1","dim2","dim3","dim4")], by="id")
a3 <- funMapPlot(mapCountryStability,mapCountryStability$dim1)
b3 <- funMapPlot(mapCountryStability,mapCountryStability$dim2)
c3 <- funMapPlot(mapCountryStability,mapCountryStability$dim3)
d3 <- funMapPlot(mapCountryStability,mapCountryStability$dim4)
legendStability <- funLegend("Yield stability ratio")

jpeg("results/Fig3.jpeg", width = 16.9, height = 16.9*0.5, units = 'cm', res = 600)
  ggarrange(a3,b3,emptyPlot,c3,d3,legendStability, nrow=2,ncol=3,labels=c("a","b","","c","d",""),font.label=list(size=8),
            widths = c(0.45,0.45,0.1))
dev.off()


# ## yield
dfPredictYield <- funPredictCountry(dfPredict,modYieldCountryCombined,"slopeYield")
mapCountryYield = join(mapCountry, dfPredictYield[,c("id","dim1","dim2","dim3","dim4")], by="id")
as3 <- funMapPlot(mapCountryYield,mapCountryYield$dim1)
# bs3 <- funMapPlot(mapCountryYield,mapCountryYield$dim2)
# cs3 <- funMapPlot(mapCountryYield,mapCountryYield$dim3)
# ds3 <- funMapPlot(mapCountryYield,mapCountryYield$dim4)
# es3 <- funMapPlot(mapCountryYield,mapCountryYield$dim5)
legendYield <- funLegend("Yield ratio")
# 
jpeg("results/FigS3.jpeg", width = 16.9*0.5, height = 16.9*0.25, units = 'cm', res = 600)
  ggarrange(as3,legendYield,nrow=1,ncol=2,labels=c("",""),font.label=list(size=8),
            widths = c(0.82,0.18))
dev.off()
#   

######## Tables
Table2 <- funTables(modStabilityCountryCombinedRaw,modStabilityRegionCombinedRaw,modStabilityFarmCombinedRaw,4, c("(Intercept)","Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time"))
write.csv(Table2,"results/Table2.csv")
TableS2 <- funTables(modYieldCountryCombinedRaw,modYieldRegionCombinedRaw,modYieldFarmCombinedRaw,4, c("(Intercept)","Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time"))
write.csv(TableS2,"results/TableS2.csv")


rm(list=ls())















### Fig 3: raw effect of diversity and scales
## global
# stability
dfPredictCountry <- data.frame(diversity=rep(0,1000),instabilityTemp=0,instabilityPrec=0,irrigation=0,fertilizer=0,timePeriod=0)
dfPredictRegion <- data.frame(diversity=rep(0,1000),instabilityTemp=0,instabilityPrec=0,irrigation=0,fertilizer=0,timePeriod=0)
dfPredictFarm <- data.frame(diversity=rep(0,1000),instabilityTemp=0,instabilityPrec=0,irrigation=0,fertilizer=0,timePeriod=0)
dfPredictCountryScale <- data.frame(areaHarvested=rep(0,1000),timePeriod=0)
dfPredictRegionScale <- data.frame(areaHarvested=rep(0,1000),timePeriod=0)
dfPredictFarmScale <- data.frame(areaHarvested=rep(0,1000),timePeriod=0)

dfPredictStability <- rbind(
                  funPred(dfPredictCountry,dfCenterCountry,dfLogCountry,modStabilityCountry,"diversity","Country"),
                  funPred(dfPredictRegion,dfCenterRegion,dfLogRegion,modStabilityRegion,"diversity","Region"),
                  funPred(dfPredictFarm,dfCenterFarm,dfLogFarm,modStabilityFarm,"diversity","Farm")
                  )

dfPredictStabilityScale <- rbind(
  funPred(dfPredictCountryScale,dfCenterCountryScale,dfLogCountryScale,modStabilityCountryScale,"areaHarvested","Country"),
  funPred(dfPredictRegionScale,dfCenterRegionScale,dfLogRegionScale,modStabilityRegionScale,"areaHarvested","Region"),
  funPred(dfPredictFarmScale,dfCenterFarmScale,dfLogFarmScale,modStabilityFarmScale,"areaHarvested","Farm")
)

# yield
dfPredictYield <- rbind(
  funPred(dfPredictCountry,dfCenterCountry,dfLogCountry,modYieldCountry,"diversity","Country"),
  funPred(dfPredictRegion,dfCenterRegion,dfLogRegion,modYieldRegion,"diversity","Region"),
  funPred(dfPredictFarm,dfCenterFarm,dfLogFarm,modYieldFarm,"diversity","Farm")
)

dfPredictYieldScale <- rbind(
  funPred(dfPredictCountryScale,dfCenterCountryScale,dfLogCountryScale,modYieldCountryScale,"areaHarvested","Country"),
  funPred(dfPredictRegionScale,dfCenterRegionScale,dfLogRegionScale,modYieldRegionScale,"areaHarvested","Region"),
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
mapCountry <- readOGR("spatial/countries_global.shp")
plot(mapCountry)
mapCountry$Area <- as.character(mapCountry$Area)

# aggregate ratios
sum(is.na(dfCountry))
dfCountryMean <- aggregate(cbind(ratioStability,ratioYield)~Country,dfCountry,mean)
head(dfCountryMean)
dfCountryState <- aggregate(dfCountry[,c("benefitStability","benefitYield")],list(dfCountry$Country),function(i){sum(as.character(i)=="winner")/length(i)})
dfCountryMap <- merge(dfCountryMean,dfCountryState,by.x="Country",by.y="Group.1")
dfCountryMap$id <- as.character(dfCountryMap$Country)
head(dfCountryMap)

trintStabilityCountry <- as.numeric(quantile(dfCountryMap$ratioStability,probs=seq(0,1,length.out = 4)))
trintYieldCountry <- as.numeric(quantile(dfCountryMap$ratioYield,probs=seq(0,1,length.out = 4)))

dfCountryMap$dim1 <-car::recode(dfCountryMap$ratioStability,"trintStabilityCountry[1]:trintStabilityCountry[2]=1; trintStabilityCountry[2]:trintStabilityCountry[3]=2; trintStabilityCountry[3]:trintStabilityCountry[4]=3;")
dfCountryMap$dim2 <-car::recode(dfCountryMap$ratioYield,"trintYieldCountry[1]:trintYieldCountry[2]=1; trintYieldCountry[2]:trintYieldCountry[3]=2; trintYieldCountry[3]:trintYieldCountry[4]=3;")
dfCountryMap <- merge(dfCountryMap[,c("id","benefitStability","benefitYield","dim1","dim2")],grd,by=c("dim1","dim2"))
dfCountryMap$dim1 <-car::recode(dfCountryMap$benefitStability,"0:(1/3)=1; (1/3):(2/3)=2; (2/3):1=3;")
dfCountryMap$dim2 <-car::recode(dfCountryMap$benefitYield,"0:(1/3)=1; (1/3):(2/3)=2; (2/3):1=3;")
dfCountryMap <- merge(dfCountryMap[,c("id","color","dim1","dim2")],grd,by=c("dim1","dim2"))
names(dfCountryMap)[4:5] <- c("colorMean","colorState")

mapsBivariateCountry <- fortify(mapCountry,region="Area")
mapsBivariateCountry = join(mapsBivariateCountry, dfCountryMap[,c("id","colorMean","colorState")], by="id")

a3 <- ggplot() +
  geom_map(data = mapsBivariateCountry, map = mapsBivariateCountry,
           aes(x = long, y = lat,  map_id=id, fill=factor(colorMean)),
           colour = "#7f7f7f", size=0.05) +
  scale_fill_identity()+
  theme_void()+
  theme(legend.position="none")

d3 <- ggplot() +
  geom_map(data = mapsBivariateCountry, map = mapsBivariateCountry,
           aes(x = long, y = lat,  map_id=id, fill=factor(colorState)),
           colour = "#7f7f7f", size=0.05) +
  scale_fill_identity()+
  theme_void()+
  theme(legend.position="none")

# europe
mapRegion <- readOGR("spatial/regions_europe.shp")
plot(mapRegion)
mapRegion$NUTS_ID <- as.character(mapRegion$NUTS_ID)

# aggregate ratios
sum(is.na(dfRegion))
dfRegionMean <- aggregate(cbind(ratioStability,ratioYield)~RegionCode,dfRegion,mean)
head(dfRegionMean)
dfRegionState <- aggregate(dfRegion[,c("benefitStability","benefitYield")],list(dfRegion$RegionCode),function(i){sum(as.character(i)=="winner")/length(i)})
dfRegionMap <- merge(dfRegionMean,dfRegionState,by.x="RegionCode",by.y="Group.1")
dfRegionMap$id <- as.character(dfRegionMap$RegionCode)
head(dfRegionMap)

trintStabilityRegion <- as.numeric(quantile(dfRegionMap$ratioStability,probs=seq(0,1,length.out = 4)))
trintYieldRegion <- as.numeric(quantile(dfRegionMap$ratioYield,probs=seq(0,1,length.out = 4)))

dfRegionMap$dim1 <-car::recode(dfRegionMap$ratioStability,"trintStabilityRegion[1]:trintStabilityRegion[2]=1; trintStabilityRegion[2]:trintStabilityRegion[3]=2; trintStabilityRegion[3]:trintStabilityRegion[4]=3;")
dfRegionMap$dim2 <-car::recode(dfRegionMap$ratioYield,"trintYieldRegion[1]:trintYieldRegion[2]=1; trintYieldRegion[2]:trintYieldRegion[3]=2; trintYieldRegion[3]:trintYieldRegion[4]=3;")
dfRegionMap <- merge(dfRegionMap[,c("id","benefitStability","benefitYield","dim1","dim2")],grd,by=c("dim1","dim2"))
dfRegionMap$dim1 <-car::recode(dfRegionMap$benefitStability,"0:(1/3)=1; (1/3):(2/3)=2; (2/3):1=3;")
dfRegionMap$dim2 <-car::recode(dfRegionMap$benefitYield,"0:(1/3)=1; (1/3):(2/3)=2; (2/3):1=3;")
dfRegionMap <- merge(dfRegionMap[,c("id","color","dim1","dim2")],grd,by=c("dim1","dim2"))
names(dfRegionMap)[4:5] <- c("colorMean","colorState")

mapsBivariateRegion <- fortify(mapRegion,region="NUTS_ID")
mapsBivariateRegion = join(mapsBivariateRegion, dfRegionMap[,c("id","colorMean","colorState")], by="id")

b3 <- ggplot() +
  geom_map(data = mapsBivariateRegion, map = mapsBivariateRegion,
           aes(x = long, y = lat,  map_id=id, fill=factor(colorMean)),
           colour = "#7f7f7f", size=0.05) +
  scale_fill_identity()+
  theme_void()+
  theme(legend.position="none")

e3 <- ggplot() +
  geom_map(data = mapsBivariateRegion, map = mapsBivariateRegion,
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


