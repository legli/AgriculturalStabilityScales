library(ncdf4)
library(raster)
library(rgdal)
library(countrycode)
library(tidyr)

# library(varhandle)



## Coordinates
dfID <- data.frame(x=rep(seq(-179.75,179.75,0.5),360),y=as.numeric(sapply(seq(89.75,-89.75,-0.5),function(i){rep(i,720)})))

## Shapes
mapCountry <- readOGR("spatial/countries_global.shp")
mapCountry$Country <-  countrycode(mapCountry$Area, 'country.name', 'iso3c')


#### create spatial mask
dfCropland <- as.data.frame(raster::aggregate(raster("datasets/cropland/Cropland2000_5m.tif"),fact=6,fun=mean,na.rm=T))
dfCropland <- cbind(dfID,dfCropland)
head(dfCropland)
names(dfCropland)[3] <- "cropland"
dfCropland[which(dfCropland$cropland>0.01),"cropland"] <- 1
dfCropland[which(dfCropland$cropland<=0.01),"cropland"] <- NA


#### create temporal mask

wd <- "datasets/ALL_CROPS_ArcINFO_0.5deg_filled"
strFolders <- dir(wd)

lsCrops <- lapply(strFolders, function(folder){
  crop <- strsplit(folder,".",fixed=T)[[1]][1]
  print(crop)
  plantAgg=as.data.frame(raster(paste0(wd,"/",folder,"/plant.start.asc")))  # aggregate to resolution of climate data
  totAgg=as.data.frame(raster(paste0(wd,"/",folder,"/tot.days.asc"))) # aggregate to resolution of climate data
  dfGrowing <- cbind(plantAgg,totAgg)
  names(dfGrowing) <- paste0(names(dfGrowing),crop)
  dfGrowing
})
dfCrops <- do.call(cbind,lsCrops)
dfCrops <- cbind(dfID,dfCrops)
head(dfCrops)
names(dfCrops)

# get months for which any crop can be grown
vecYear1 <- c(31,59,90,120,151,181,212,243,273,304,334,365)
vecYear2 <- c(365,396,424,455,485,516,546,577,608,638,669,699)
m <- 0 
repeat{
  m <- m+1
  print(m)
  dfCrops[,paste0("month",m)] <- apply(dfCrops,1,function(r){
    res <- NA
    if (sum(r[seq(3,52,2)]<=vecYear1[m],na.rm=T)>0|sum((r[seq(3,52,2)]+r[seq(4,52,2)])>vecYear2[m],na.rm=T)>0)
    {res <- 1}
    res
  })
  if (m==12)
    break
}
sapply(53:64,function(i){table(dfCrops[,i])}) 

names(dfCrops)
dfCrops <- dfCrops[,c(1:2,53:64)]



#### extract climate data by spatial and temporal mask
## oben ncdf-files
ncTempAbs <- nc_open("datasets/climate/air.mon.mean.v501.nc")
ncPrecAbs <- nc_open("datasets/climate/precip.mon.total.v501.nc")

## create dataframes with correct coordinate (start with x = 0.25 to correct for origin)
dfClimateXY <- data.frame(x=rep(c(seq(0.25,179.75,0.5),seq(-179.75,-0.25,0.5)),360),y=as.numeric(sapply(seq(89.75,-89.75,-0.5),function(i){rep(i,720)})))


stackTemp <- stack(lapply(61:117,function(y){
  print(y)
  dfTemp <- dfClimateXY
  for(m in 1:12){
    temp <- ncvar_get(ncTempAbs, attributes(ncTempAbs$var)$names[1],start=c(1,1,(y*12+m)),count=c(720,360,1))
    dfTemp[,paste0("m",m)] <- as.numeric(temp)
  }
  dfTemp <- merge(dfTemp,dfCropland,by=c("x","y"))
  dfTemp <- merge(dfTemp,dfCrops,by=c("x","y"))
  dfTemp[,3:14] <- dfTemp[,3:14]*dfTemp$cropland*dfTemp[,16:27]
  dfTemp[,paste0("temp",1900+y)] <- rowMeans(dfTemp[,3:14],na.rm=T)
  
  spg <- dfTemp[,c("x","y",paste0("temp",1900+y))]
  coordinates(spg) <- ~ x + y  
  gridded(spg) <- TRUE
  raster(spg)
}))
plot(stackTemp[[57]])
writeRaster(stackTemp,"datasets/stackTemperatureCropland.tif", format="GTiff")
stackTemp <- stack("datasets/stackTemperatureCropland.tif")

dfCountryTemp <- raster::extract(stackTemp,mapCountry,fun=mean,na.rm=TRUE,weights=TRUE,normalizeWeights=TRUE,sp=TRUE)@data
names(dfCountryTemp)[3:59] <- 1961:2017
dfCountryTempFinal <- dfCountryTemp %>% gather(Year, TempMean, names(dfCountryTemp)[3:59])
dfCountryTempFinal <- na.omit(dfCountryTempFinal)
write.csv(dfCountryTempFinal,"datasets/temperatureMean_global.csv",row.names = F)

dfCountryTempSD <- raster::extract(stackTemp,mapCountry,fun=sd,na.rm=TRUE,sp=TRUE)@data
names(dfCountryTempSD)[3:59] <- 1961:2017
dfCountryTempSDFinal <- dfCountryTempSD %>% gather(Year, TempSDMean, names(dfCountryTempSD)[3:59])
dfCountryTempSDFinal <- na.omit(dfCountryTempSDFinal)
write.csv(dfCountryTempSDFinal,"datasets/temperatureSD_global.csv",row.names = F)


stackPrec <- stack(lapply(61:117,function(y){
  print(y)
  dfPrec <- dfClimateXY
  for(m in 1:12){
    prec <- ncvar_get(ncPrecAbs, attributes(ncPrecAbs$var)$names[1],start=c(1,1,(y*12+m)),count=c(720,360,1))
    dfPrec[,paste0("m",m)] <- as.numeric(prec)
  }
  dfPrec <- merge(dfPrec,dfCropland,by=c("x","y"))
  dfPrec <- merge(dfPrec,dfCrops,by=c("x","y"))
  dfPrec[,3:14] <- dfPrec[,3:14]*dfPrec$cropland*dfPrec[,16:27]
  dfPrec[,paste0("prec",1900+y)] <- rowMeans(dfPrec[,3:14],na.rm=T)
  
  spg <- dfPrec[,c("x","y",paste0("prec",1900+y))]
  coordinates(spg) <- ~ x + y  
  gridded(spg) <- TRUE
  raster(spg)
}))
plot(stackPrec[[2]])
writeRaster(stackPrec,"datasets/stackPrecipitationCropland.tif", format="GTiff")
stackPrec <- stack("datasets/stackPrecipitationCropland.tif")

dfCountryPrec <- raster::extract(stackPrec,mapCountry,fun=mean,na.rm=TRUE,weights=TRUE,normalizeWeights=TRUE,sp=TRUE)@data
names(dfCountryPrec)[3:59] <- 1961:2017
dfCountryPrecFinal <- dfCountryPrec %>% gather(Year, PrecMean, names(dfCountryPrec)[3:59])
dfCountryPrecFinal <- na.omit(dfCountryPrecFinal)
write.csv(dfCountryPrecFinal,"datasets/precipitationMean_global.csv",row.names = F)

dfCountryPrecSD <- raster::extract(stackPrec,mapCountry,fun=sd,na.rm=TRUE,sp=TRUE)@data
names(dfCountryPrecSD)[3:59] <- 1961:2017
dfCountryPrecSDFinal <- dfCountryPrecSD %>% gather(Year, PrecSDMean, names(dfCountryPrecSD)[3:59])
dfCountryPrecSDFinal <- na.omit(dfCountryPrecSDFinal)
write.csv(dfCountryPrecSDFinal,"datasets/precipitationSD_global.csv",row.names = F)



rm(list=ls())
