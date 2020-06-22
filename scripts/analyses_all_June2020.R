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
library(nlme)
library(MuMIn)
library(tidyr)
library(openxlsx)

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

source("scripts/functionsAnalyses_June2020.R")


############################################################################################
###################              DATA               ########################################
############################################################################################

###### Country level
dfCountry <- read.csv("datasetsDerived/dataFinal_national.csv")
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
dfRegion <- read.csv("datasetsDerived/dataFinal_regional.csv")
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
dfFarm <- read.csv("P:/dataFinal_farm.csv")
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

fitdist(dfCountry$stability,"norm")$aic - fitdist(dfCountry$stability,"lnorm")$aic # log normally distributed

### regression analyses
## transformations
dfTransCountry=with(dfCountry,data.frame(Country,Continent,
                                         stability = log(stability),
                                         diversity, 
                                         irrigation=sqrt(irrigation),
                                         fertilizer=sqrt(fertilizer),
                                         instabilityTemp,instabilityPrec,
                                         sdTemp,sdPrec,
                                         areaHarvested = log(areaHarvested),
                                         timePeriod
))


# scale predictors for standardized regression
dfPredictorsCountry=sapply(dfTransCountry[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterCountry=data.frame(Country=dfTransCountry[,1],Continent=dfTransCountry[,2],stability=dfTransCountry[,3],dfPredictorsCountry)
head(dfCenterCountry)

## check colinearity
cor(dfCenterCountry[,3:12],method='s') 

## regression models
# stability
modStabilityCountry <- lm(stability~diversity+fertilizer+irrigation+sdTemp+sdPrec+instabilityTemp+instabilityPrec+timePeriod+
                                diversity:fertilizer+diversity:irrigation+diversity:sdTemp+diversity:sdPrec+diversity:instabilityTemp+diversity:instabilityPrec+diversity:timePeriod
                          ,data=dfCenterCountry)
summary(modStabilityCountry)

modStabilityCountryLME <- lme(stability~diversity+fertilizer+irrigation+sdTemp+sdPrec+instabilityTemp+instabilityPrec+timePeriod+
                                diversity:fertilizer+diversity:irrigation+diversity:sdTemp+diversity:sdPrec+diversity:instabilityTemp+diversity:instabilityPrec+diversity:timePeriod,
                              random=~1|Country,method = "ML",
                              data=dfCenterCountry)
summary(modStabilityCountryLME)
r.squaredGLMM(modStabilityCountryLME)
fixed.effects(modStabilityCountryLME)
random.effects(modStabilityCountryLME)
vif(modStabilityCountryLME)

# test if lme is better
anova(modStabilityCountryLME,modStabilityCountry)




###### European level

# check distribution of response variables
hist(dfRegion$stability)
fitdist(dfRegion$stability,"norm")$aic - fitdist(dfRegion$stability,"lnorm")$aic #log normally distributed

### regression analyses
## transformations
dfTransRegion=with(dfRegion,data.frame(Region,Country,
                                         stability = log(stability),
                                         diversity, 
                                         irrigation=sqrt(irrigation),
                                         fertilizer=sqrt(fertilizer),
                                         instabilityTemp,instabilityPrec,
                                         sdTemp,sdPrec,
                                         areaHarvested = log(areaHarvested),
                                         timePeriod
))

# scale predictors for standardized regression
dfPredictorsRegion=sapply(dfTransRegion[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterRegion=data.frame(Region=dfTransRegion[,1],Country=dfTransRegion[,2],stability=dfTransRegion[,3],dfPredictorsRegion)
head(dfCenterRegion)

## check colinearity
cor(dfCenterRegion[,3:12],method='s') 

## regression models
# stability
modStabilityRegion <- lm(stability~diversity+fertilizer+irrigation+sdTemp+sdPrec+instabilityTemp+instabilityPrec+timePeriod+
                           diversity:fertilizer+diversity:irrigation+diversity:sdTemp+diversity:sdPrec+diversity:instabilityTemp+diversity:instabilityPrec+diversity:timePeriod
                          ,data=dfCenterRegion)
summary(modStabilityRegion)

modStabilityRegionLME <- lme(stability~diversity+fertilizer+irrigation+sdTemp+sdPrec+instabilityTemp+instabilityPrec+timePeriod+
                               diversity:fertilizer+diversity:irrigation+diversity:sdTemp+diversity:sdPrec+diversity:instabilityTemp+diversity:instabilityPrec+diversity:timePeriod,
                              random=~1|Country/Region,method = "ML",
                              data=dfCenterRegion)
summary(modStabilityRegionLME)
r.squaredGLMM(modStabilityRegionLME)
fixed.effects(modStabilityRegionLME)
random.effects(modStabilityRegionLME)
vif(modStabilityRegionLME)

# test if lme is better
anova(modStabilityRegionLME,modStabilityRegion)


###### Farm level
# check distribution of response variables
hist(dfFarm$stability)

fitdist(dfFarm$stability,"norm")$aic-fitdist(dfFarm$stability,"lnorm")$aic # log normally distributed

### regression analyses
## transformations
dfTransFarm=with(dfFarm,data.frame(Farm,District=REGION_ID,
                                       stability = log(stability),
                                       diversity, 
                                       irrigation=sqrt(irrigation),
                                       fertilizer=sqrt(fertilizer),
                                       instabilityTemp,instabilityPrec,
                                       sdTemp,sdPrec,
                                       areaHarvested = log(areaHarvested),
                                       timePeriod
))

# scale predictors for standardized regression
dfPredictorsFarm=sapply(dfTransFarm[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterFarm=data.frame(Farm=dfTransFarm[,1],District=dfTransFarm[,2],stability=dfTransFarm[,3],dfPredictorsFarm)
head(dfCenterFarm)

## check colinearity
cor(dfCenterFarm[,3:12],method='s') # instabilityTime, instabilityPrec and time very correlated (but no need to exclude due to stepwise model selection)
# remove time (> 0.8) and instability Prec (hig influence on vif)  
## regression models
# stability
modStabilityFarm <- lm(stability~diversity+fertilizer+irrigation+areaHarvested+sdTemp+sdPrec+instabilityTemp+
                           diversity:fertilizer+diversity:irrigation+diversity:areaHarvested+diversity:sdTemp+diversity:sdPrec+diversity:instabilityTemp,
                         data=dfCenterFarm)
summary(modStabilityFarm)

modStabilityFarmLME <- lme(stability~diversity+fertilizer+irrigation+areaHarvested+instabilityTemp+
                             diversity:fertilizer+diversity:irrigation+diversity:areaHarvested+diversity:instabilityTemp,
                             random=~1|District/Farm,method = "ML",
                             data=dfCenterFarm)
summary(modStabilityFarmLME)
r.squaredGLMM(modStabilityFarmLME)
fixed.effects(modStabilityFarmLME)
random.effects(modStabilityFarmLME)
vif(modStabilityFarmLME)

# test if lme is better
anova(modStabilityFarmLME,modStabilityFarm)



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

##### Fig 1: maps of stability

# national level
mapCountry <- readOGR("spatial/countries_global.shp")
mapCountry$Country <-  countrycode(mapCountry$Area, 'country.name', 'iso3c')
dfCountryAgg <- aggregate(cbind(stability,ratioS,ratioL,diversity)~Country,dfCountry,mean)

# regional level
mapRegion <- readOGR("spatial/regions_europe.shp")
mapRegion$Region <- mapRegion$NUTS_ID
dfRegionAgg <- aggregate(cbind(stability,ratioS,ratioL,diversity)~Region,dfRegion,mean)

# farm level
mapFarm <- readOGR("spatial/vg2500_rbz_bld.shp")
mapFarm$REGION_ID <- mapFarm$district
dfFarmAgg <- aggregate(cbind(stability,ratioS,ratioL,diversity)~Farm,dfFarm,mean)
dfFarmAggRegion <- aggregate(cbind(stability,ratioS,ratioL,diversity)~REGION_ID,dfFarm,mean)
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




###### Fig 2: stability comparisons
dfCountryG <- dfCountry[,c("stability","stabilityOverall")] %>% gather(Scale, Stability, c("stability","stabilityOverall"))
dfCountryG$Stability <- log(dfCountryG$Stability)
dfCountryG[which(dfCountryG$Scale=="stability"),"Scale"] <- "National"
dfCountryG[which(dfCountryG$Scale=="stabilityOverall"),"Scale"] <- "Global"
dfCountryG$Scale <- factor(dfCountryG$Scale,levels=c("National","Global"))
  
dfRegionG <- dfRegion[,c("stability","stabilityOverall")] %>% gather(Scale, Stability, c("stability","stabilityOverall"))
dfRegionG$Stability <- log(dfRegionG$Stability)
dfRegionG[which(dfRegionG$Scale=="stability"),"Scale"] <- "Regional"
dfRegionG[which(dfRegionG$Scale=="stabilityOverall"),"Scale"] <- "European"
dfRegionG$Scale <- factor(dfRegionG$Scale,levels=c("Regional","European"))

dfFarmG <- dfFarm[,c("stability","stabilityOverall")] %>% gather(Scale, Stability, c("stability","stabilityOverall"))
dfFarmG$Stability <- log(dfFarmG$Stability)
dfFarmG[which(dfFarmG$Scale=="stability"),"Scale"] <- "Farm"
dfFarmG[which(dfFarmG$Scale=="stabilityOverall"),"Scale"] <- "German"
dfFarmG$Scale <- factor(dfFarmG$Scale,levels=c("Farm","German"))

a2 <- funBoxplot(dfCountryG,myColors[1],"log(Yield stability)","National")
b2 <- funBoxplot(dfRegionG,myColors[2],"","Regional")
c2 <- funBoxplot(dfFarmG,myColors[3],"","Farm")

# plot
jpeg("results/Fig2.jpeg", width = 16.9, height = 8, units = 'cm', res = 600)
ggarrange(a2,b2,c2,
          labels = letters[1:3],font.label=list(size=8),
          ncol = 3, nrow = 1,align='h')
dev.off()



# Table 2: contribution to larger scale stability
dfCountryC <- dfCountryAgg[order(dfCountryAgg$ratioL,decreasing = T),c("Country","ratioL")]
dfCountryC <- dfCountryC[c(1:10,(nrow(dfCountryC)-10):(nrow(dfCountryC))),]
mean(dfCountry$ratioL)
sd(dfCountry$ratioL)

dfRegionC <- dfRegionAgg[order(dfRegionAgg$ratioL,decreasing = T),c("Region","ratioL")]
dfRegionC <- dfRegionC[c(1:10,(nrow(dfRegionC)-10):(nrow(dfRegionC))),]
mean(dfRegion$ratioL)
sd(dfRegion$ratioL)

dfTable2 <- cbind(dfCountryC,dfRegionC)
names(dfTable2) <- c("Country","Yield stability contribution","Region","Yield stability contribution")
dfTable2[,c(2,4)] <- round(dfTable2[,c(2,4)],2)
write.xlsx(dfTable2,"results/Table2.xlsx")

##### Part 2: regression results

## Fig 3: barplots of  stability model
a3 <- funEffect(modStabilityCountryLME,9,c("Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature heterogeneity","Precipitation heterogeneity","Temperature instability","Precipitation instability","Time"),"National",myColors[1],0.4,0.1,1,"Standardized regression coefficient",0.02,0.03)
b3 <- funEffect(modStabilityRegionLME,9,c("Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature heterogeneity","Precipitation heterogeneity","Temperature instability","Precipitation instability","Time"),"Regional",myColors[2],0.4,0.1,1,"",0.02,0.03)
c3 <- funEffect(modStabilityFarmLME,6,c("Diversity","sqrt(Fertilizer)","sqrt(Irigation)","log(Area harvested)","Temperature instability"),"Farm",myColors[3],0.4,0.1,1,"",0.02,0.03)

# plot
jpeg("results/Fig3.jpeg", width = 16.9, height = 8, units = 'cm', res = 600)
  ggarrange(a3,b3,c3,
            labels = letters[1:3],font.label=list(size=8),
            ncol = 3, nrow = 1,align='h')
dev.off()

## Fig 4-6: interactions
  
dfPredictCountry <- data.frame(diversity=rep(0,1000),irrigation=0,fertilizer=0,sdTemp=0,sdPrec=0,instabilityTemp=0,instabilityPrec=0,timePeriod=0)
dfPredictRegion <- data.frame(diversity=rep(0,1000),irrigation=0,fertilizer=0,sdTemp=0,sdPrec=0,instabilityTemp=0,instabilityPrec=0,timePeriod=0)
dfPredictFarm <- data.frame(diversity=rep(0,1000),irrigation=0,fertilizer=0,instabilityTemp=0,instabilityPrec=0,timePeriod=0,areaHarvested=0)

a4 <- funInteraction(dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,moderator="irrigation",modS=modStabilityCountryLME,xlabel="Diversity",ylabel=expression(paste("Yield stability (",mu,"/",sigma,")")),
                     modLabel="Irrigation",yVal1=0,yVal2=50)
b4 <- funInteraction(dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,moderator="instabilityPrec",modS=modStabilityCountryLME,xlabel="Diversity",ylabel="",
                     modLabel="Precipitation instability",yVal1=0,yVal2=50) 

jpeg("results/Fig4.jpeg", width = 16.9, height = 8, units = 'cm',res = 600)
  ggarrange(a4,b4,
            labels = letters[1:2],font.label=list(size=8),
            ncol = 2, nrow = 1)
dev.off()

a5 <- funInteraction(dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,moderator="irrigation",modS=modStabilityRegionLME,xlabel="Diversity",ylabel=expression(paste("Yield stability (",mu,"/",sigma,")")),
                     modLabel="Irrigation",yVal1=0,yVal2=30)

jpeg("results/Fig5.jpeg", width = 8, height = 8, units = 'cm',res = 600)
  ggarrange(a5,ncol = 1, nrow = 1)
dev.off()

a6 <- funInteraction(dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfTransFarm,moderator="irrigation",modS=modStabilityFarmLME,xlabel="Diversity",ylabel=expression(paste("Yield stability (",mu,"/",sigma,")")),
                     modLabel="Irrigation",yVal1=0,yVal2=25)

b6 <- funInteraction(dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfTransFarm,moderator="areaHarvested",modS=modStabilityFarmLME,xlabel="Diversity",ylabel="",
                     modLabel="Area harvested",yVal1=0,yVal2=25)

c6 <- funInteraction(dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfTransFarm,moderator="instabilityTemp",modS=modStabilityFarmLME,xlabel="Diversity",ylabel="",
                     modLabel="Temperature instability",yVal1=0,yVal2=25)

jpeg("results/Fig6.jpeg", width = 16.9, height = 8, units = 'cm',res = 600)
  ggarrange(a6,b6,c6,
            labels = letters[1:3],font.label=list(size=8),
            ncol = 3, nrow = 1)
dev.off()



####### supplementary figures

# regression tabes (includig transformations)

TableS1 <- funTables(modStabilityCountryLME,modStabilityRegionLME,modStabilityFarmLME,2, 
                      c("(Intercept)","Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature heterogeneity","Precipitation heterogeneity","Temperature instability","Precipitation instability","Time",
                         "Diversity:sqrt(Fertilizer)","Diversity:sqrt(Irigation)","Diversity:Temperature heterogeneity","Diversity:Precipitation heterogeneity","Diversity:Temperature instability","Diversity:Precipitation instability","Diversity:Time"),
                      c("(Intercept)","Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature heterogeneity","Precipitation heterogeneity","Temperature instability","Precipitation instability","Time",
                         "Diversity:sqrt(Fertilizer)","Diversity:sqrt(Irigation)","Diversity:Temperature heterogeneity","Diversity:Precipitation heterogeneity","Diversity:Temperature instability","Diversity:Precipitation instability","Diversity:Time"),
                      c("(Intercept)","Diversity","sqrt(Fertilizer)","sqrt(Irigation)","log(Area harvested)","Temperature instability",
                         "Diversity:sqrt(Fertilizer)","Diversity:sqrt(Irigation)","Diversity:log(Area harvested)","Diversity:Temperature instability"))

TableS1 <- TableS1[c(1,3,17:19,13,12,20,14,21,7:9,5,4,10,6,11,16,15,2),]

write.xlsx(TableS1,"results/TableS1.xlsx")


## Fig S1: diversity 
a1s <- funMaps(dfCountryAgg,variable="diversity",st_as_sf(mapCountry),"Country",c(seq(0,5,length.out = 6),seq(6,21,length.out = 5)),c("#FFFFE5","#78C679","#004529"),"Diversity","a",T)
b1s <- funMaps(dfRegionAgg,variable="diversity",st_as_sf(mapRegion),"Region",c(seq(0,5,length.out = 6),seq(6,21,length.out = 5)),c("#FFFFE5","#78C679","#004529"),"","b",F)
c1s <- funMaps(dfFarmAggRegion,variable="diversity",st_as_sf(mapFarm),"REGION_ID",c(seq(0,5,length.out = 6),seq(6,21,length.out = 5)),c("#FFFFE5","#78C679","#004529"),"","c",F)

jpeg("results/FigS1.jpeg", width = 16.9,height = 16.9, units = 'cm',res = 300)
grid.arrange(a1s,b1s,c1s,
             layout_matrix = rbind(c(1, 1),
                                   c(2, 3)))
dev.off()

## Fig S2: ratio small scale
a2s <- funMaps(dfCountryAgg,variable="ratioS",st_as_sf(mapCountry),"Country",seq(0,2,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Ratio","a",T)
b2s <- funMaps(dfRegionAgg,variable="ratioS",st_as_sf(mapRegion),"Region",seq(0,2,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"","b",F)
c2s <- funMaps(dfFarmAggRegion,variable="ratioS",st_as_sf(mapFarm),"REGION_ID",seq(0,2,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"","c",F)

jpeg("results/FigS2.jpeg", width = 16.9,height = 16.9, units = 'cm',res = 300)
grid.arrange(a2s,b2s,c2s,
             layout_matrix = rbind(c(1, 1),
                                   c(2, 3)))
dev.off()

## Fig S3: ratio large scale
a2s <- funMaps(dfCountryAgg,variable="ratioL",st_as_sf(mapCountry),"Country",seq(0.85,1.15,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"Ratio","a",T)
b2s <- funMaps(dfRegionAgg,variable="ratioL",st_as_sf(mapRegion),"Region",seq(0.85,1.15,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"","b",F)
c2s <- funMaps(dfFarmAggRegion,variable="ratioL",st_as_sf(mapFarm),"REGION_ID",seq(0.85,1.15,length.out = 11),c("#40004B","#F7F7F7","#00441B"),"","c",F)

jpeg("results/FigS3.jpeg", width = 16.9,height = 16.9, units = 'cm',res = 300)
grid.arrange(a3s,b3s,c3s,
             layout_matrix = rbind(c(1, 1),
                                   c(2, 3)))
dev.off()









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

jpeg("results/FigS4.jpeg", width = 30*(5/5),height = (3/5)*30*(5/5), units = 'cm',res = 600)

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

jpeg("results/FigS5.jpeg", width = 30*(4/5),height = (3/4)*30*(4/5), units = 'cm',res = 600)
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
e6s <- funPredRange(predictor="instabilityTemp",dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfTransFarm,dfOriginal=dfFarm,modS=modStabilityFarm,transResponse = "log",trans="",xlabel=expression(paste("Temperature instability (-(",mu,"/",sigma,"))")),ylabel=expression(paste("Yield stability (",mu,"/",sigma,")")),0,25,"gray30")
f6s <- funPredRange(predictor="instabilityPrec",dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfTransFarm,dfOriginal=dfFarm,modS=modStabilityFarm,transResponse = "log",trans="",xlabel=expression(paste("Precipitation instability (-(",mu,"/",sigma,"))")),ylabel="",0,25,"gray30")
g6s <- funPredRange(predictor="timePeriod",dfPredict=dfPredictFarm,dfCenter=dfCenterFarm,dfLog=dfTransFarm,dfOriginal=dfFarm,modS=modStabilityFarm,transResponse = "log",trans="",xlabel="Time",ylabel="",0,25,"gray30")

jpeg("results/FigS6.jpeg", width = 30*(4/5),height = (4/5)*30, units = 'cm',res = 600)
ggarrange(a6s,b6s,c6s,d6s,e6s,f6s,g6s,empty,
          h6s,i6s,j6s,k6s,l6s,m6s,
          labels = c(letters[1:7],"",letters[8:13],"",""),font.label=list(size=8),
          # labels = letters[1:13],font.label=list(size=8),
          ncol = 4, nrow = 4)
dev.off()



rm(list=ls())





