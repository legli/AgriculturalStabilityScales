library(rgdal)
library(maptools)
library(tidyr)
library(vegan)

################ DATA PREPARATION


#### laod releant data

load("datasetsDerived/dfYieldCalories_global.RData")
load("datasetsDerived/dfProductionFull_Final_global.RData")



## summarize per time frame 
vecCountryFinal <- as.character(unique(dfYieldCalories$Area))
lsAll <- lapply(vecCountryFinal,function(ctry){
  # detrend yields
  show(ctry)
  
  # subset data for the target country
  # dfProductionCtry <- dfProductionFull_Final[which(dfProductionFull_Final$Area==ctry),c("Area","Group2","Year","ProductionDet")]
  dfProductionSumCtry <- dfYieldCalories[which(dfYieldCalories$Area==ctry),]
  dfProductionSumCtry$ProductionDet <- resid(loess(Production ~ Year,data=dfProductionSumCtry))
  dfProductionSumCtry$YieldDet <- resid(loess(Yield ~ Year,data=dfProductionSumCtry))

  yearStart <- 2009
   
    # dfSummary <- data.frame(Area=ctry, timePeriod= yearStart)
    # subset crops in the global data
    vecCrop <- unique(dfProductionFull_Final[which(dfProductionFull_Final$Area==ctry&dfProductionFull_Final$Year>=yearStart&dfProductionFull_Final$Year<=(yearStart+7)),"Group2"])
     dfSummary <- data.frame(prop=0,variability =sd(dfProductionSumCtry[which(dfProductionSumCtry$Year>=yearStart&dfProductionSumCtry$Year<=(yearStart+7)),"YieldDet"],na.rm=T)/mean(dfProductionSumCtry[which(dfProductionSumCtry$Year>=yearStart&dfProductionSumCtry$Year<=(yearStart+7)),"Yield"],na.rm=T))
    
    lsScales <- lapply(seq(0.1,1,0.1),function(p){
      
      vecCV <- sapply(1:10,function(s){
      randomCountries <- sample(vecCountryFinal,p*length(vecCountryFinal))
      dfProductionWorld <- aggregate(cbind(Production,AreaHarvested)~Year,dfProductionFull_Final[which(dfProductionFull_Final$Area%in%randomCountries&dfProductionFull_Final$Group2%in%vecCrop),],sum)
      dfProductionWorld$ProductionDet <- resid(loess(Production ~ Year,data=dfProductionWorld))
      dfProductionWorld$Yield <- dfProductionWorld$Production/dfProductionWorld$AreaHarvested
      dfProductionWorld$YieldDet <- resid(loess(Yield ~ Year,data=dfProductionWorld))
      sd(dfProductionWorld[which(dfProductionWorld$Year>=yearStart&dfProductionWorld$Year<=(yearStart+7)),"YieldDet"],na.rm=T)/mean(dfProductionWorld[which(dfProductionWorld$Year>=yearStart&dfProductionWorld$Year<=(yearStart+7)),"Yield"],na.rm=T)
      })
      data.frame(prop=p,variability=vecCV)
    })
    dfScales <- do.call(rbind,lsScales)
    dfSummary <- rbind(dfSummary,dfScales)
    dfSummary$Area <- ctry
    dfSummary
})
dfAll <- do.call(rbind,lsAll)
head(dfAll)


write.csv(dfAll, "datasetsDerived/dataScales_global.csv",row.names=F)





sum(is.na(dfAll))

dfAllArea <- aggregate(variability~Area+prop,dfAll,mean)

dfTest <- dfAll[which(dfAll$Area=="Brazil"),]
mod <- lm(variability~prop,dfTest)
summary(mod)

boxplot(variability~prop,dfAllArea)

rm(list=ls())
