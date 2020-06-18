######## CALCULATE VARIABLES FOR THE 5 TIME PERIOS

# get regions across datasets
vecLevelFinal <- Reduce(intersect,list(dfCropland$Level,dfProduction$Level,dfYield$Level,dfShannon$Level,dfFertilizer$Level,dfIrrigation$Level,dfClimateFinal$Level))

# remove countries listed by Renard & Tilman 2019
vecLevelFinal <- vecLevelFinal[-which(vecLevelFinal%in%c("EGY","PRK", "GIN", "KEN","MOZ","ZMB","IRL","NLD","NZL"))] 

# adapt dfProduction
dfProduction <- dfProduction[which(dfProduction$Level%in%vecLevelFinal),]

## summarize per time frame 
lsAll <- lapply(vecLevelFinal,function(lev){
  # detrend yields
  show(as.character(lev))
  dfYieldLevel <- dfYield[which(dfYield$Level==lev),]
  dfShannonLevel <- dfShannon[which(dfShannon$Level==lev),]
  if (length(unique(dfYieldLevel$Year))==50)
  {
    dfYieldLevel$YieldDet <- resid(lm(Yield ~ Year^2,data=dfYieldLevel))
    dfShannonLevel$diversityDet <- resid(lm(diversity ~ Year^2,data=dfShannonLevel))
    
    merge(dfYieldLevel,dfShannonLevel,by=c("Level","Year"))
  }
})
dfYieldFinal <- do.call(rbind,lsAll)

# dfFinal <- merge(dfYieldFinal,dfShannon,by=c("Level","Year"))
dfFinal <- merge(dfYieldFinal,dfFertilizer,by=c("Level","Year"))
dfFinal <- merge(dfFinal,dfIrrigation,by=c("Level","Year"))
dfFinal <- merge(dfFinal,dfClimateFinal,by=c("Level","Year"))
nrow(dfFinal)
sum(is.na(dfFinal))
head(dfFinal)

sum(dfFinal$Nitrogen==0)
dfFinalTrans <- dfFinal[which(dfFinal$Nitrogen>0),]
dfFinalTrans <- dfFinalTrans[which(dfFinalTrans$Irrigation>0),]

dfFinalTrans=with(dfFinalTrans,data.frame(Level,
                                         YieldDet = YieldDet,
                                         diversity=diversityDet, 
                                        Irrigation=log(Irrigation),
                                     Nitrogen=log(Nitrogen),
                                         meanTemp,meanPrec,
                                     Year
))
dfPredictorsCountry=sapply(dfFinalTrans[,-c(1:2)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterCountry=data.frame(Level=dfFinalTrans[,1],YieldDet=dfFinalTrans[,2],dfPredictorsCountry)
head(dfCenterCountry)


library(nlme)
mod <- lme(YieldDet~diversity+Year,
           random=~1+diversity|Level,data=dfCenterCountry)

dfMod <- lapply(unique(dfCenterCountry$Level),function(lev){
  # detrend yields
  show(as.character(lev))
  dfCountry <- dfCenterCountry[which(dfCenterCountry$Level==lev),]
  
  mod <- lm(YieldDet~diversity+Nitrogen+Irrigation+meanTemp+meanPrec+Year
                                ,data=dfCenterCountry)
  
  data.frame(Level=lev, R2=summary(mod)$r.squared, div=summary(mod)$coefficients[2,1],p=summary(mod)$coefficients[2,4])

})
dfModd <- do.call(rbind,dfMod)
hist(dfModd$R2)


library(nlme)
mod <- lme(YieldDet~diversity+Nitrogen+Irrigation+meanTemp+meanPrec+Year,
                     random=~1+diversity|Level,data=dfFinalTrans)

mod <- lme(YieldDet~diversity+Nitrogen+Irrigation+meanTemp+meanPrec+Year,
           random=~1+diversity|Level,data=dfFinalTrans)

modStabilityCountryFull <- lme(stability~diversity+fertilizer+irrigation+areaHarvested+instabilityTemp+instabilityPrec+timePeriod,
                               random=~1+diversity|Country,data=dfCenterCountry,
                               method="ML",
                               na.action=na.exclude)

mod <- lme(stability~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,
                               random=~1|Country
                              ,data=dfCenterCountry)
summary(modStabilityCountryFull)
fixed.effects(mod)
random.effects(mod)
vec <- random.effects(mod)[2]

library(MuMIn)
r.squaredGLMM(mod)


  lsAggregate <- lapply(c(1968,1978,1988,1998,2008),function(yearStart){
    
    sumLevel <- sum(dfYield$Level==lev&dfYield$Year>=yearStart&dfYield$Year<=(yearStart+9))
    if(sumLevel==10){
      # get global production data for target country
      dfProductionOverall <- dfProduction[which(dfProduction$Year>=yearStart&dfProduction$Year<=(yearStart+9)),]
      vecItem <- unique(dfProductionOverall[which(dfProductionOverall$Level==lev),"Item"])
      dfProductionOverall <- dfProductionOverall[which(dfProductionOverall$Item%in%vecItem),]
      dfProductionOverallAgg <- aggregate(cbind(Production,AreaHarvested)~Year,dfProductionOverall,sum)
      dfProductionOverallAgg$Yield <- dfProductionOverallAgg$Production/dfProductionOverallAgg$AreaHarvested
      dfProductionOverallAgg$YieldDet <-resid(lm(Yield ~ Year^2,data=dfProductionOverallAgg)) 
      
      # global production withouth the target country
      dfProductionReduced <- dfProductionOverall[-which(dfProductionOverall$Level==lev),]
      dfProductionReducedAgg <- aggregate(cbind(Production,AreaHarvested)~Year,dfProductionReduced,sum)
      dfProductionReducedAgg$Yield <- dfProductionReducedAgg$Production/dfProductionReducedAgg$AreaHarvested
      dfProductionReducedAgg$YieldDet <-resid(lm(Yield ~ Year^2,data=dfProductionReducedAgg)) 
      
      # subset data for the target country
      dfProductionSumLevel <- dfYield[which(dfYield$Level==lev&dfYield$Year>=yearStart&dfYield$Year<=(yearStart+9)),]
      dfProductionSumLevel$YieldDet <- resid(lm(Yield ~ Year^2,data=dfProductionSumLevel))
      dfShannonLevel <- dfShannon[which(dfShannon$Level==lev&dfShannon$Year>=yearStart&dfShannon$Year<=(yearStart+9)),]
      dfCroplandLevel <- dfCropland[which(dfCropland$Level==lev&dfCropland$Year>=yearStart&dfCropland$Year<=(yearStart+9)),]
      dfFertilizerLevel <- dfFertilizer[which(dfFertilizer$Level==lev&dfFertilizer$Year>=yearStart&dfFertilizer$Year<=(yearStart+9)),]
      dfIrrigationLevel <- dfIrrigation[which(dfIrrigation$Level==lev&dfIrrigation$Year>=yearStart&dfIrrigation$Year<=(yearStart+9)),]
      dfClimateLevel <- dfClimateFinal[which(dfClimateFinal$Level==lev&dfClimateFinal$Year>=yearStart&dfClimateFinal$Year<=(yearStart+9)),]
      
      dfSummary <- data.frame(Level=lev, timePeriod= yearStart)
      dfSummary$stability <- mean(dfProductionSumLevel$Yield,na.rm=T)/sd(dfProductionSumLevel$YieldDet,na.rm=T)
      dfSummary$stabilityOverall <- mean(dfProductionOverallAgg$Yield,na.rm=T)/sd(dfProductionOverallAgg$YieldDet,na.rm=T)
      dfSummary$stabilityReduced <- mean(dfProductionReducedAgg$Yield,na.rm=T)/sd(dfProductionReducedAgg$YieldDet,na.rm=T)
      dfSummary$yield <- mean(dfProductionSumLevel$Yield,na.rm=T)
      dfSummary$yieldOverall <- mean(dfProductionOverallAgg$Yield,na.rm=T)
      dfSummary$yieldReduced <- mean(dfProductionReducedAgg$Yield,na.rm=T)
      dfSummary$areaHarvested <- mean(dfProductionSumLevel$AreaHarvested,na.rm=T)
      dfSummary$areaHarvestedOverall <-  mean(dfProductionOverallAgg$AreaHarvested)  
      dfSummary$areaHarvestedReduced <-  mean(dfProductionReducedAgg$AreaHarvested)  
      dfSummary$diversity <- mean(dfShannonLevel$diversity,na.rm=T)
      dfSummary$meanCropland <- mean(dfCroplandLevel$croplandArea,na.rm=T)
      dfSummary$meanNitrogen <- mean(dfFertilizerLevel$Nitrogen,na.rm=T)
      dfSummary$irrigation <- mean(dfIrrigationLevel$Irrigation,na.rm=T)
      dfSummary$instabilityTemp <- -(mean(dfClimateLevel$meanTemp,na.rm=T)/sd(dfClimateLevel$meanTemp,na.rm=T))
      dfSummary$instabilityPrec <- -(mean(dfClimateLevel$meanPrec,na.rm=T)/sd(dfClimateLevel$meanPrec,na.rm=T))
      na.omit(dfSummary)
    }
  })
  do.call(rbind,lsAggregate)
})


rm(list=ls())
