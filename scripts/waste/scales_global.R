library(rgdal)
library(maptools)
library(tidyr)
library(vegan)

################ DATA PREPARATION
set.seed(9999)

############# Global
#### laod releant data

# load("datasetsDerived/dfYieldCalories_global.RData")
load("datasetsDerived/dfProductionFullFinal_global.RData")
dfNational <- read.csv("datasetsDerived/dataFinal_global.csv")
dfProductionFullFinal <- dfProductionFullFinal[which(dfProductionFullFinal$Area%in%unique(dfNational$Country)),]




## summarize per time frame 
vecAreaFinal <- as.character(unique(dfProductionFullFinal$Area))
lsAll <- lapply(vecAreaFinal,function(a){
  # detrend yields
  show(a)
  lsTime <- lapply(c(1968,1978,1988,1998,2008),function(yearStart){
  
    dfProductionWorldTime <- dfProductionFullFinal[which(dfProductionFullFinal$Year>=yearStart&dfProductionFullFinal$Year<=(yearStart+9)),]
    if (sum(dfProductionWorldTime$Area==a)>0) {  
      vecCrop <- unique(dfProductionWorldTime[which(dfProductionWorldTime$Area==a),"Crop"])
      vecAreaCrop <- as.character(unique(dfProductionWorldTime[which(dfProductionWorldTime$Crop%in%vecCrop),"Area"]))
    
    # dfNationalTime <- dfNational[which(dfNational$timePeriod==yearStart),]
    
      
      lsScales <- lapply(seq(0.1,1,0.1),function(p){
        lsProp <- lapply(1:5,function(s){
          randomCountries <- c(a,sample(vecAreaCrop[-which(vecAreaCrop==a)],p*(length(vecAreaCrop)-1)))
          dfProductionRegion <- dfProductionWorldTime[which(dfProductionWorldTime$Area%in%randomCountries),]
          dfProductionRegionAgg <- aggregate(cbind(Production,AreaHarvested)~Year,dfProductionRegion,sum)
          dfProductionRegionAgg$Yield <- dfProductionRegionAgg$Production/dfProductionRegionAgg$AreaHarvested
          dfProductionRegionAgg$YieldDet <-resid(lm(Yield ~ Year^2,data=dfProductionRegionAgg)) 
          stability <- mean(dfProductionRegionAgg$Yield,na.rm=T)/sd(dfProductionRegionAgg$YieldDet,na.rm=T)
          yield <- mean(dfProductionRegionAgg$Yield,na.rm=T)
          data.frame(stability,yield)
          # # predictors
          # dfHarvestedAreaRegionAgg <- aggregate(AreaHarvested~Crop,dfProductionRegion,sum)
          # diversity <- exp(diversity(dfHarvestedAreaRegionAgg$AreaHarvested,index="shannon"))
          # dfNationalRegion <- dfNationalTime[which(dfNationalTime$Area%in%randomCountries),]
          # meanNitrogen <- mean(dfNationalRegion$meanNitrogen)
          # meanIrrigation <- mean(dfNationalRegion$meanIrrigation_share)
          # instabilityTemp <- mean(dfNationalRegion$instabilityTemp)
          # instabilityPrec <- mean(dfNationalRegion$instabilityPrec)
          # data.frame(stability,yield,diversity,meanNitrogen,meanIrrigation,instabilityTemp,instabilityPrec)
        })
        dfProp <- do.call(rbind,lsProp)
        dfProp$prop <- p
        dfProp
      })
      dfScales <- do.call(rbind,lsScales)
      dfScales$timePeriod <- yearStart
      dfScales
    }
  })
  dfTime <- do.call(rbind,lsTime)
  dfTime$Area <- a
  dfTime
})
dfAll <- do.call(rbind,lsAll)
head(dfAll)
write.csv(dfAll, "datasetsDerived/dataScales_global.csv",row.names=F)

############# European level
#### load releant data

load("datasetsDerived/dfProductionFullFinal_europe.RData")
dfEurope <- read.csv("datasetsDerived/dataFinal_europe.csv")
dfProductionFullFinal <- dfProductionFullFinal[which(dfProductionFullFinal$Area%in%unique(dfEurope$RegionCode)),]

## summarize per time frame 
vecAreaFinal <- unique(dfProductionFullFinal$Area)
lsAllEurope <- lapply(vecAreaFinal,function(a){
  # detrend yields
  show(a)
  lsTime <- lapply(c(1978,1988,1998,2008),function(yearStart){
    
    dfProductionWorldTime <- dfProductionFullFinal[which(dfProductionFullFinal$Year>=yearStart&dfProductionFullFinal$Year<=(yearStart+9)),]
    if (sum(dfProductionWorldTime$Area==a)>0) {  
      vecCrop <- unique(dfProductionWorldTime[which(dfProductionWorldTime$Area==a),"Crop"])
      vecAreaCrop <- as.character(unique(dfProductionWorldTime[which(dfProductionWorldTime$Crop%in%vecCrop),"Area"]))
      
      lsScales <- lapply(seq(0.1,1,0.1),function(p){
        lsProp <- lapply(1:5,function(s){
          randomCountries <- c(a,sample(vecAreaCrop[-which(vecAreaCrop==a)],p*(length(vecAreaCrop)-1)))
          dfProductionRegion <- dfProductionWorldTime[which(dfProductionWorldTime$Area%in%randomCountries),]
          dfProductionRegionAgg <- aggregate(cbind(Production,AreaHarvested)~Year,dfProductionRegion,sum)
          dfProductionRegionAgg$Yield <- dfProductionRegionAgg$Production/dfProductionRegionAgg$AreaHarvested
          dfProductionRegionAgg$YieldDet <-resid(lm(Yield ~ Year^2,data=dfProductionRegionAgg)) 
          stability <- mean(dfProductionRegionAgg$Yield,na.rm=T)/sd(dfProductionRegionAgg$YieldDet,na.rm=T)
          yield <- mean(dfProductionRegionAgg$Yield,na.rm=T)
          data.frame(stability,yield)
        })
        dfProp <- do.call(rbind,lsProp)
        dfProp$prop <- p
        dfProp
      })
      dfScales <- do.call(rbind,lsScales)
      dfScales$timePeriod <- yearStart
      dfScales
    }
  })
  dfTime <- do.call(rbind,lsTime)
  dfTime$Area <- a
  dfTime
})
dfAllEurope <- do.call(rbind,lsAllEurope)
head(dfAllEurope)
write.csv(dfAllEurope, "datasetsDerived/dataScales_europe.csv",row.names=F)


############# Farm level
#### load releant data

load("P:/dfProductionFullFinal_farm.RData")
dfFarm <- read.csv("P:/dataFinal_farmlevel.csv")
dfProductionFullFinal <- dfProductionFullFinal[which(dfProductionFullFinal$Area%in%unique(dfFarm$Farmer)),]

## summarize per time frame 
vecAreaFinal <- unique(dfProductionFullFinal$Area)
lsAllFarm <- lapply(sample(vecAreaFinal,1000),function(a){
  # detrend yields
  show(a)
  lsTime <- lapply(c(1998,2008),function(yearStart){
    
    dfProductionWorldTime <- dfProductionFullFinal[which(dfProductionFullFinal$Year>=yearStart&dfProductionFullFinal$Year<=(yearStart+9)),]
    if (sum(dfProductionWorldTime$Area==a)>0) {  
      vecCrop <- unique(dfProductionWorldTime[which(dfProductionWorldTime$Area==a),"Crop"])
      vecAreaCrop <- as.character(unique(dfProductionWorldTime[which(dfProductionWorldTime$Crop%in%vecCrop),"Area"]))
      
      lsScales <- lapply(seq(0.1,1,0.1),function(p){
        lsProp <- lapply(1:5,function(s){
          randomCountries <- c(a,sample(vecAreaCrop[-which(vecAreaCrop==a)],p*(length(vecAreaCrop)-1)))
          dfProductionRegion <- dfProductionWorldTime[which(dfProductionWorldTime$Area%in%randomCountries),]
          dfProductionRegionAgg <- aggregate(cbind(Production,AreaHarvested)~Year,dfProductionRegion,sum)
          dfProductionRegionAgg$Yield <- dfProductionRegionAgg$Production/dfProductionRegionAgg$AreaHarvested
          dfProductionRegionAgg$YieldDet <-resid(lm(Yield ~ Year^2,data=dfProductionRegionAgg)) 
          stability <- mean(dfProductionRegionAgg$Yield,na.rm=T)/sd(dfProductionRegionAgg$YieldDet,na.rm=T)
          yield <- mean(dfProductionRegionAgg$Yield,na.rm=T)
          data.frame(stability,yield)
        })
        dfProp <- do.call(rbind,lsProp)
        dfProp$prop <- p
        dfProp
      })
      dfScales <- do.call(rbind,lsScales)
      dfScales$timePeriod <- yearStart
      dfScales
    }
  })
  dfTime <- do.call(rbind,lsTime)
  dfTime$Area <- a
  dfTime
})
dfAllFarm <- do.call(rbind,lsAllFarm)
head(dfAllFarm)
write.csv(dfAllFarm, "datasetsDerived/dataScales_farm.csv",row.names=F)


rm(list=ls())





# ## summarize per time frame 
# vecCountryFinal <- as.character(unique(dfProductionFullFinal$Area))
# lsAll <- lapply(c(1968,1978,1988,1998,2008),function(yearStart){
#   # detrend yields
#   show(yearStart)
#   
#   # subset data for the target period
#   dfProductionWorldTime <- dfProductionFullFinal[which(dfProductionFullFinal$Year>=yearStart&dfProductionFullFinal$Year<=(yearStart+9)),]
#   
#   # dfSummary <- data.frame(Area=ctry, timePeriod= yearStart)
#   # subset crops in the global data
#   # dfSummary <- data.frame(prop=0,variability =sd(dfProductionSumCtry[which(dfProductionSumCtry$Year>=yearStart&dfProductionSumCtry$Year<=(yearStart+7)),"YieldDet"],na.rm=T)/mean(dfProductionSumCtry[which(dfProductionSumCtry$Year>=yearStart&dfProductionSumCtry$Year<=(yearStart+7)),"Yield"],na.rm=T))
#   
#   lsScales <- lapply(seq(0.1,1,0.1),function(p){
#     
#     lsProp <- lapply(1:100,function(s){
#       randomCountries <- sample(vecCountryFinal,p*length(vecCountryFinal))
#       
#       dfProductionWorld <- aggregate(cbind(Production,AreaHarvested)~Year,dfProductionWorldTime[which(dfProductionWorldTime$Area%in%randomCountries),],sum)
#       dfProductionWorld$Yield <- dfProductionWorld$Production/dfProductionWorld$AreaHarvested
#       dfProductionWorld$YieldDet <-resid(lm(Yield ~ Year^2,data=dfProductionWorld)) 
#       stability <- mean(dfProductionWorld$Yield,na.rm=T)/sd(dfProductionWorld$YieldDet,na.rm=T)
#       yield <- mean(dfProductionWorld$Yield,na.rm=T)
#       data.frame(stability,yield)
#     })
#     dfProp <- do.call(rbind,lsProp)
#     dfProp$prop <- p
#     dfProp
#   })
#   dfScales <- do.call(rbind,lsScales)
#   dfScales$timePeriod <- yearStart
#   dfScales
# })
# dfAll <- do.call(rbind,lsAll)
# head(dfAll)
# dfAllTime <- aggregate(cbind(stability,yield)~prop+timePeriod,dfAll,mean)
# dfAllTime$col <- "black"
# dfAllTime[which(dfAllTime$timePeriod==1978),"col"] <- "red"
# dfAllTime[which(dfAllTime$timePeriod==1988),"col"] <- "green"
# dfAllTime[which(dfAllTime$timePeriod==1998),"col"] <- "blue"
# dfAllTime[which(dfAllTime$timePeriod==2008),"col"] <- "yellow"
# plot(dfAllTime$prop,dfAllTime$stability,col=dfAllTime$col)
# 
# dfLogNational=with(dfAllTime,data.frame(stability = log(stability),
#                                         yield = log(yield),
#                                         prop, 
#                                         timePeriod
# ))
# names(dfLogNational)
# head(dfLogNational)
# 
# # scale predictors for standardized regression
# dfPredictorsNational=sapply(dfLogNational[,-c(1:2)],function(x)scale(x,center = T,scale=T)[,1])
# dfCenterNational=data.frame(stability=dfLogNational[,1],yield=dfLogNational[,2],dfPredictorsNational)
# head(dfCenterNational)
# 
# modStabilityScalesNational <- lm(stability~prop+timePeriod,data=dfCenterNational)
# summary(modStabilityScalesNational)
# motYieldScalesNational <- lm(yield~prop+timePeriod,data=dfCenterNational)
# summary(modStabilityScalesNational)
# 
# par(mfrow=c(1,2))
# dfTest <- dfAll[which(dfAll$timePeriod==2008),]
# boxplot(stability~prop,dfTest,ylim=c(0,80))
# dfTest <- dfAll[which(dfAll$timePeriod==1978),]
# boxplot(stability~prop,dfTest,ylim=c(0,80))
