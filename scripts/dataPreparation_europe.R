## libraries
library(eurostat)

#### get production file 
dfAgricultureOld <- get_eurostat("apro_cpnhr_h",time_format = "raw")
head(dfAgricultureOld)
unique(dfAgricultureOld$strucpro)
dfAgricultureNew <- get_eurostat("apro_cpnhr",time_format = "raw")
head(dfAgricultureNew)
unique(dfAgricultureNew$strucpro)

dfAgriculture <- rbind(dfAgricultureOld,dfAgricultureNew)

head(dfAgriculture)
head(dfAgriculture)
unique(dfAgriculture$crops)
unique(dfAgriculture$strucpro)
unique(dfAgriculture$geo)
unique(dfAgriculture$time)
dfAgriculture$time <- as.numeric(dfAgriculture$time) # change year to numeric

# only include harvested area (AR; 1000ha) and production 
dfAgriculture <- dfAgriculture[which(dfAgriculture$strucpro%in%c("AR","PR")&
                                       dfAgriculture$time>=1978&dfAgriculture$time<=2017),]
head(dfAgriculture)

# remove NA
dfAgriculture <- dfAgriculture[which(!is.na(dfAgriculture$values)),]
write.csv(dfAgriculture,"C:/Users/egli/Nextcloud/Cloud/PhD_Leipzig/Publications/StabilityScales/datasets/agriculturalProduction_europe.csv",row.names = F)


### irrigation
dfIrrigation <- get_eurostat("aei_ef_ir",time_format = "raw")
head(dfIrrigation)
unique(dfIrrigation$crops)
unique(dfIrrigation$time)
dfIrrigation <- dfIrrigation[which(dfIrrigation$crops=="UAAIT"),] # focus on actually irrigated area
head(dfIrrigation)
write.csv(dfIrrigation,"C:/Users/egli/Nextcloud/Cloud/PhD_Leipzig/Publications/StabilityScales/datasets/irrigation_europe.csv",row.names = F)



rm(list=ls())

