
################ SET SEED
set.seed(9999)

############# Global
#### laod releant data

# load("datasetsDerived/dfYieldCalories_global.RData")
load("datasetsDerived/dfProduction_global.RData")


## summarize per time frame 
vecLevelFinal <- as.character(unique(dfProduction$Level))
lsAll <- lapply(vecLevelFinal,function(lev){
  # detrend yields
  show(lev)
  lsTime <- lapply(c(1968,1978,1988,1998,2008),function(yearStart){
    
    dfProductionWorldTime <- dfProduction[which(dfProduction$Year>=yearStart&dfProduction$Year<=(yearStart+9)),]
    if (sum(dfProductionWorldTime$Level==lev)>0) {  
      vecItem <- unique(dfProductionWorldTime[which(dfProductionWorldTime$Level==lev),"Item"])
      vecLevelItem <- as.character(unique(dfProductionWorldTime[which(dfProductionWorldTime$Item%in%vecItem),"Level"]))
      
      
      lsScales <- lapply(seq(0.1,1,0.1),function(p){
        lsProp <- lapply(1:5,function(s){
          randomCountries <- c(lev,sample(vecLevelItem[-which(vecLevelItem==lev)],p*(length(vecLevelItem)-1)))
          dfProductionRegion <- dfProductionWorldTime[which(dfProductionWorldTime$Level%in%randomCountries&dfProductionWorldTime$Item%in%vecItem),]
          dfProductionRegionAgg <- aggregate(cbind(Production,AreaHarvested)~Year,dfProductionRegion,sum)
          dfProductionRegionAgg$Yield <- dfProductionRegionAgg$Production/dfProductionRegionAgg$AreaHarvested
          dfProductionRegionAgg$YieldDet <-resid(lm(Yield ~ Year^2,data=dfProductionRegionAgg)) 
          stability <- mean(dfProductionRegionAgg$Yield,na.rm=T)/sd(dfProductionRegionAgg$YieldDet,na.rm=T)
          yield <- mean(dfProductionRegionAgg$Yield,na.rm=T)
          areaHarvested <- mean(dfProductionRegionAgg$AreaHarvested)
          data.frame(stability,yield,areaHarvested)
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
  dfTime$Level <- lev
  dfTime
})
dfAll <- do.call(rbind,lsAll)
head(dfAll)
# only keep unique entries
dfAll <- unique(dfAll)
write.csv(dfAll, "datasetsDerived/dataScales_global.csv",row.names=F)


############# European level
#### load releant data

load("datasetsDerived/dfProduction_europe.RData")

## summarize per time frame 
vecLevelFinal <- as.character(unique(dfProduction$Level))
lsAllEurope <- lapply(vecLevelFinal,function(lev){
  # detrend yields
  show(lev)
  lsTime <- lapply(c(1978,1988,1998,2008),function(yearStart){
    
    dfProductionWorldTime <- dfProduction[which(dfProduction$Year>=yearStart&dfProduction$Year<=(yearStart+9)),]
    if (sum(dfProductionWorldTime$Level==lev)>0) {  
      vecItem <- unique(dfProductionWorldTime[which(dfProductionWorldTime$Level==lev),"Item"])
      vecLevelItem <- as.character(unique(dfProductionWorldTime[which(dfProductionWorldTime$Item%in%vecItem),"Level"]))
      
      lsScales <- lapply(seq(0.1,1,0.1),function(p){
        lsProp <- lapply(1:5,function(s){
          randomCountries <- c(lev,sample(vecLevelItem[-which(vecLevelItem==lev)],p*(length(vecLevelItem)-1)))
          dfProductionRegion <- dfProductionWorldTime[which(dfProductionWorldTime$Level%in%randomCountries&dfProductionWorldTime$Item%in%vecItem),]
          dfProductionRegionAgg <- aggregate(cbind(Production,AreaHarvested)~Year,dfProductionRegion,sum)
          dfProductionRegionAgg$Yield <- dfProductionRegionAgg$Production/dfProductionRegionAgg$AreaHarvested
          dfProductionRegionAgg$YieldDet <-resid(lm(Yield ~ Year^2,data=dfProductionRegionAgg)) 
          stability <- mean(dfProductionRegionAgg$Yield,na.rm=T)/sd(dfProductionRegionAgg$YieldDet,na.rm=T)
          yield <- mean(dfProductionRegionAgg$Yield,na.rm=T)
          areaHarvested <- mean(dfProductionRegionAgg$AreaHarvested)
          data.frame(stability,yield,areaHarvested)
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
  dfTime$Level <- lev
  dfTime
})
dfAllEurope <- do.call(rbind,lsAllEurope)
head(dfAllEurope)
# only keep unique entries
dfAllEurope <- unique(dfAllEurope)
write.csv(dfAllEurope, "datasetsDerived/dataScales_europe.csv",row.names=F)


############# Farm level
#### load releant data

load("P:/dfProduction_farm.RData")


## summarize per time frame 
vecLevelFinal <- unique(dfProduction$Level)
lsAllFarm <- lapply(sample(vecLevelFinal,1000),function(lev){
  # detrend yields
  show(lev)
  lsTime <- lapply(c(1998,2008),function(yearStart){
    
    dfProductionWorldTime <- dfProduction[which(dfProduction$Year>=yearStart&dfProduction$Year<=(yearStart+9)),]
    if (sum(dfProductionWorldTime$Level==lev)>0) {  
      vecItem <- unique(dfProductionWorldTime[which(dfProductionWorldTime$Level==lev),"Item"])
      vecLevelItem <- as.character(unique(dfProductionWorldTime[which(dfProductionWorldTime$Item%in%vecItem),"Level"]))
      
      lsScales <- lapply(seq(0.1,1,0.1),function(p){
        lsProp <- lapply(1:5,function(s){
          randomCountries <- c(lev,sample(vecLevelItem[-which(vecLevelItem==lev)],p*(length(vecLevelItem)-1)))
          dfProductionRegion <- dfProductionWorldTime[which(dfProductionWorldTime$Level%in%randomCountries&dfProductionWorldTime$Item%in%vecItem),]
          dfProductionRegionAgg <- aggregate(cbind(Production,AreaHarvested)~Year,dfProductionRegion,sum)
          dfProductionRegionAgg$Yield <- dfProductionRegionAgg$Production/dfProductionRegionAgg$AreaHarvested
          dfProductionRegionAgg$YieldDet <-resid(lm(Yield ~ Year^2,data=dfProductionRegionAgg)) 
          stability <- mean(dfProductionRegionAgg$Yield,na.rm=T)/sd(dfProductionRegionAgg$YieldDet,na.rm=T)
          yield <- mean(dfProductionRegionAgg$Yield,na.rm=T)
          areaHarvested <- mean(dfProductionRegionAgg$AreaHarvested)
          data.frame(stability,yield,areaHarvested)
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
  dfTime$Level <- lev
  dfTime
})
dfAllFarm <- do.call(rbind,lsAllFarm)
head(dfAllFarm)

# only keep unique entries
dfAllFarm <- unique(dfAllFarm)
write.csv(dfAllFarm, "P:/dataScales_farm.csv",row.names=F)


rm(list=ls())


