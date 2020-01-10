library(lme4)
library(ggplot2)
library(MuMIn)
library(RColorBrewer)
library(ggpubr)

## globals

vecColors <- brewer.pal(5,"PuBu")
vecColors2 <- c(brewer.pal(11,"PuOr")[c(1,4,10)],"darkgreen")

lev <- c("Diversity","Asynchrony","Combined")
myColors <- c("#4daf4a",vecColors[5], "#ff7f00")
names(myColors) <- factor(lev,levels=lev)



###### Global
dfGlobal <- read.csv("datasetsDerived/dataFinal_global_v3.csv")
sum(dfGlobal$benefitStabilityG=="winner")/nrow(dfGlobal)
sum(dfGlobal$benefitYieldG=="winner")/nrow(dfGlobal)


# scale differences
boxplot(dfGlobal[,c("ratioStabilityC","ratioStabilityG")])
boxplot(dfGlobal[,c("ratioYieldC","ratioYieldG")])


# get prop winner 
vecCountry <- unique(dfGlobal$Area)
lsBenefit <- lapply(vecCountry,function(c){
  dfSummary <- data.frame(Area=as.character(c),propWinner=sum(dfGlobal$Area==c&dfGlobal$benefitStabilityG=="winner")/sum(dfGlobal$Area==c))
  dfSummary
})
dfBenefit <- do.call(rbind,lsBenefit)
head(dfBenefit)
hist(dfBenefit$propWinner)

dfBenefit2 <- aggregate(cbind(ratioStabilityG,ratioYieldG)~Area,dfGlobal,mean)
dfBenefit2[order(dfBenefit2$ratioStabilityG,decreasing = T),]
head(dfBenefit2)
sum(dfBenefit2$ratioStabilityG>1&dfBenefit2$ratioYieldG>1)/nrow(dfBenefit)
sum(dfBenefit2$ratioStabilityG>1&dfBenefit2$ratioYieldG<1)/nrow(dfBenefit)
sum(dfBenefit2$ratioStabilityG<1&dfBenefit2$ratioYieldG>1)/nrow(dfBenefit)
sum(dfBenefit2$ratioStabilityG<1&dfBenefit2$ratioYieldG<1)/nrow(dfBenefit)


## trade-off between yield benefit and stability benefit
cor(dfGlobal$ratioStabilityG,dfGlobal$ratioYieldG)
cor(dfGlobal$ratioStabilityC,dfGlobal$ratioYieldC)
plot(dfGlobal$ratioStabilityG,dfGlobal$ratioYieldG,xlim=c(0,5),ylim=c(0,5))




# modRaw <- lm(ratioCV~asynchrony+meanNitrogen_t_ha+meanIrrigation_share+meanWarfare+instabilityTemp+instabilityPrec+timePeriod,data=dfGlobal)
# summary(modRaw)

dfLogGlobal=with(dfGlobal,data.frame(Country,
                                  ratioStabilityG = log(ratioStabilityG),
                                  ratioYieldG = log(ratioYieldG),
                                  asynchrony,diversity, 
                                  irrigation=sqrt(meanIrrigation_share),
                                  fertilizer=sqrt(meanNitrogen_t_ha),
                                  instabilityTemp,instabilityPrec,
                                  timePeriod
))
names(dfLogGlobal)
head(dfLogGlobal)

## scale predictors for standardized regression
dfPredictorsGlobal=sapply(dfLogGlobal[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterGlobal=data.frame(Country=dfLogGlobal[,1],ratioStabilityG=dfLogGlobal[,2],ratioYieldG=dfLogGlobal[,3],dfPredictorsGlobal)
head(dfCenterGlobal)


## regression models
modAsynchronyStabilityGlobal <- lm(ratioStabilityG~asynchrony+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterGlobal)
summary(modAsynchronyStabilityGlobal)
modDiversityStabilityGlobal <- lm(ratioStabilityG~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterGlobal)
summary(modDiversityStabilityGlobal)

modAsynchronyYieldGlobal <- lm(ratioYieldG~asynchrony+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterGlobal)
summary(modAsynchronyYieldGlobal)
modDiversityYieldGlobal <- lm(ratioYieldG~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterGlobal)
summary(modDiversityYieldGlobal)

# modStabilityG0 <- lm(ratioStabilityG~1,data=dfCenterGlobal)
# 
# modStabilityG2 <- lm(ratioStabilityG~asynchrony+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterGlobal)
# 
# modStabilityGFull <- lm(ratioStabilityG~(asynchrony+fertilizer+irrigation+meanWarfare+instabilityTemp+instabilityPrec+timePeriod)^2,data=dfCenterGlobal)
# 
# modCV_reduced=stepAIC(modStabilityGFull,k=log(nrow(dfCenterGlobal)))
# summary(modCV_reduced)
# summary(modStabilityG)
# 
# 
# plot(allEffects(modCV_reduced),multiline=T)
# 
# AIC(modStabilityG0)
# 
# AIC(modStabilityG2)




######## EUROPE
dfEurope <- read.csv("datasetsDerived/dataFinal_europe_v3.csv")
dfEurope <- merge(dfEurope,dfGlobal[,c("Country","timePeriod","meanNitrogen_t_ha","meanIrrigation_share")],by=c("Country","timePeriod"))

sum(dfEurope$benefitStabilityG=="winner")/nrow(dfEurope)
sum(dfEurope$benefitYieldG=="winner")/nrow(dfEurope)


dfLogEurope=with(dfEurope,data.frame(RegionCode,
                                  ratioStabilityG = log(ratioStabilityG),
                                  ratioYieldG = log(ratioYieldG),
                                  asynchrony,diversity, 
                                  irrigation=sqrt(meanIrrigation_share),
                                  fertilizer=sqrt(meanNitrogen_t_ha),                                  
                                  instabilityTemp,instabilityPrec,
                                  timePeriod
))
names(dfLogEurope)
head(dfLogEurope)

## scale predictors for standardized regression
dfPredictorsEurope=sapply(dfLogEurope[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterEurope=data.frame(Area=dfLogEurope[,1],ratioStabilityG=dfLogEurope[,2],ratioYieldG=dfLogEurope[,3],dfPredictorsEurope)
head(dfCenterEurope)



## regression models
modAsynchronyStabilityEurope <- lm(ratioStabilityG~asynchrony+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterEurope)
summary(modAsynchronyStabilityEurope)
modDiversityStabilityEurope <- lm(ratioStabilityG~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterEurope)
summary(modDiversityStabilityEurope)

modAsynchronyYieldEurope <- lm(ratioYieldG~asynchrony+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterEurope)
summary(modAsynchronyYieldEurope)
modDiversityYieldEurope <- lm(ratioYieldG~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterEurope)
summary(modDiversityYieldEurope)


####### FARMLEVEL

dfFarmer <- read.csv("C:/Users/egli/Nextcloud/Cloud/PhD_Leipzig/Publications/Globalization/datasetsDerived/dataFinal_farmlevel.csv")

dfLogFarmer=with(dfFarmer,data.frame(id,
                                  ratioStabilityG = log(ratioStabilityG),
                                  ratioYieldG = log(ratioYieldG),
                                  asynchrony,diversity,
                                  irrigation=sqrt(meanIrrigation),
                                  fertilizer=sqrt(meanFertilizer_ha),
                                  instabilityTemp,instabilityPrec,
                                  timePeriod
))
names(dfLogFarmer)
head(dfLogFarmer)

## scale predictors for standardized regression
dfPredictorsFarmer=sapply(dfLogFarmer[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterFarmer=data.frame(Area=dfLogFarmer[,1],ratioStabilityG=dfLogFarmer[,2],ratioYieldG=dfLogFarmer[,3],dfPredictorsFarmer)
head(dfCenterFarmer)


## regression models
modAsynchronyStabilityFarmer <- lm(ratioStabilityG~asynchrony+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterFarmer)
summary(modAsynchronyStabilityFarmer)
modDiversityStabilityFarmer <- lm(ratioStabilityG~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterFarmer)
summary(modDiversityStabilityFarmer)

modAsynchronyYieldFarmer <- lm(ratioYieldG~asynchrony+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterFarmer)
summary(modAsynchronyYieldFarmer)
modDiversityYieldFarmer <- lm(ratioYieldG~diversity+fertilizer+irrigation+instabilityTemp+instabilityPrec+timePeriod,data=dfCenterFarmer)
summary(modDiversityYieldFarmer)
























## read DF


length(unique(dfGlobal$Area))
names(dfGlobal)
table(dfGlobal$IncomeGroup)

#### 1: explore realtionship between diversity and asynchrony
dfDiversityAsynchronyGlobal <- dfGlobal[,c("Area","timePeriod","IncomeGroup","asynchrony","diversity")]
dfDiversityAsynchronyGlobal$timePeriod <- factor(dfDiversityAsynchronyGlobal$timePeriod,levels = c(1968,1978,1988,1998,2008))
dfDiversityAsynchronyGlobal$IncomeGroup <- factor(dfDiversityAsynchronyGlobal$IncomeGroup,levels = c("High income","Upper middle income","Lower middle income","Low income"))

cor.test(dfDiversityAsynchronyGlobal$diversity,dfDiversityAsynchronyGlobal$asynchrony,method='s')

# test if correlation between diversity and asynchrony decreases over time
modDiversityTimeGlobal <- lmer(asynchrony ~ diversity + (1+diversity|timePeriod), data = dfDiversityAsynchronyGlobal)
modDiversityLMGlobal <- lm(asynchrony ~ diversity,data = dfDiversityAsynchronyGlobal)
summary(modDiversityLMGlobal)
anova(modDiversityTimeGlobal,modDiversityLMGlobal) 

modDiversityTimeFixedGlobal=fixef(modDiversityTimeGlobal)
r.squaredGLMM(modDiversityTimeGlobal) 
modDiversityTimeGroupGlobal <-   coef(modDiversityTimeGlobal)$timePeriod

## plot
a1 <- ggplot(dfDiversityAsynchronyGlobal, aes(x=diversity, y=asynchrony, color = timePeriod)) +
  geom_point(size=0.7) +
  scale_colour_manual(name="Time interval",values = vecColors, labels = c("1961-1970","1971-1980","1981-1990","1991-2000","2001-2010")) +
  scale_radius(range = c(2,12)) +
  geom_abline(intercept = modDiversityTimeGroupGlobal[,1],slope = modDiversityTimeGroupGlobal[,2],color=vecColors)+
  geom_abline(intercept = summary(modDiversityLMGlobal)$coefficients[1,1],slope = summary(modDiversityLMGlobal)$coefficients[2,1],color="black",linetype=3)+
  theme_classic() +  
  xlab("") +
  ylab("Asynchrony") + 
  theme(axis.title.x = element_text(size=8)) +  
  theme(axis.text.x = element_text(size=8)) +
  theme(axis.title.y = element_text(size=8)) +    
  theme(axis.text.y = element_text(size=8)) +
  theme(legend.position = c(0.8, 0.17))+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  theme(legend.key.size = unit(0.2,"cm")) + 
  theme(plot.margin = unit(c(0.2,0.3,0.2,0.2), "cm")) 


## test if correlation between diversity and asynchrony differs between income group
modDiversityIncomeGlobal <- lmer(asynchrony ~ diversity + (1+diversity|IncomeGroup), data = dfDiversityAsynchronyGlobal)

anova(modDiversityIncomeGlobal,modDiversityLMGlobal) 

modDiversityIncomeFixedGlobal=fixef(modDiversityIncomeGlobal)
r.squaredGLMM(modDiversityIncomeGlobal) 
modDiversityIncomeGroupGlobal <-   coef(modDiversityIncomeGlobal)$IncomeGroup

## plot
c1 <- ggplot(dfDiversityAsynchronyGlobal, aes(x=diversity, y=asynchrony, color = IncomeGroup)) +
  geom_point(size=0.7) +
  scale_colour_manual(name="Income group",values = vecColors2, labels = c("High income","Upper middle income","Lower middle income","Low income")) +
  scale_radius(range = c(2,12)) +
  geom_abline(intercept = modDiversityIncomeGroupGlobal[,1],slope = modDiversityIncomeGroupGlobal[,2],color=vecColors2)+
  geom_abline(intercept = summary(modDiversityLMGlobal)$coefficients[1,1],slope = summary(modDiversityLMGlobal)$coefficients[2,1],color="black",linetype=3)+
  theme_classic() +  
  xlab("Diversity") +
  ylab("Asynchrony") + 
  theme(axis.title.x = element_text(size=8)) +  
  theme(axis.text.x = element_text(size=8)) +
  theme(axis.title.y = element_text(size=8)) +    
  theme(axis.text.y = element_text(size=8)) +
  theme(legend.position = c(0.8, 0.17))+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  theme(legend.key.size = unit(0.2,"cm")) +
  theme(plot.margin = unit(c(0.2,0.3,0.2,0.2), "cm")) 



#### 2: regression analyses
# transform variables
dfLogGlobal=with(dfGlobal,data.frame(Area,
                                     stability = log(stability),
                                     diversity,
                                     asynchrony, 
                                     meanIrrigation_share=sqrt(meanIrrigation_share),
                                     meanNitrogen_t_ha=sqrt(meanNitrogen_t_ha),
                                     instabilityTemp,instabilityPrec,
                                     meanWarfare,
                                     timePeriod
))
names(dfLogGlobal)
head(dfLogGlobal)

## scale predictors for standardized regression
dfPredictorsGlobal=sapply(dfLogGlobal[,-c(1:2)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterGlobal=data.frame(Area=dfLogGlobal[,1],stability=dfLogGlobal[,2],dfPredictorsGlobal)
head(dfCenterGlobal)

cor(dfCenterGlobal$diversity,dfCenterGlobal$asynchrony)

# models
modAsynchronyGlobal <- lm(stability~asynchrony+meanIrrigation_share+meanNitrogen_t_ha+meanWarfare+timePeriod+instabilityTemp+instabilityPrec,data=dfCenterGlobal)
summary(modAsynchronyGlobal)

modDiversityGlobal <- lm(stability~diversity+meanIrrigation_share+meanNitrogen_t_ha+meanWarfare+timePeriod+instabilityTemp+instabilityPrec,data=dfCenterGlobal)
summary(modDiversityGlobal)

modTotalGlobal <- lm(stability~diversity+asynchrony+meanIrrigation_share+meanNitrogen_t_ha+meanWarfare+timePeriod+instabilityTemp+instabilityPrec,data=dfCenterGlobal)
summary(modTotalGlobal)

# barplot of effects: combine coefficents of both modesl
dfDiversity <- data.frame(summary(modDiversityGlobal)$coefficients)[2:8,c(1,2,4)]
names(dfDiversity) <- c("Effect","SE","pVal")
colnames(dfDiversity)
dfDiversity$nam <- c("Diversity","sqrt(Irigation)","sqrt(N use intensity)","Warfare","Time","Temperature instability","Precipitation instability")
dfDiversity <- rbind(dfDiversity[1,],data.frame(Effect=0,SE=0,pVal=NA,nam="Asynchrony"),dfDiversity[2:7,])
dfDiversity$Model <- "Diversity"

dfAsynchrony <- data.frame(summary(modAsynchronyGlobal)$coefficients)[2:8,c(1,2,4)]
names(dfAsynchrony) <- c("Effect","SE","pVal")
colnames(dfAsynchrony)
dfAsynchrony$nam <- c("Asynchrony","sqrt(Irigation)","sqrt(N use intensity)","Warfare","Time","Temperature instability","Precipitation instability")
dfAsynchrony <- rbind(data.frame(Effect=0,SE=0,pVal=NA,nam="Diversity"),dfAsynchrony)
dfAsynchrony$Model <-  "Asynchrony"

dfTotal <- data.frame(summary(modTotalGlobal)$coefficients)[2:9,c(1,2,4)]
names(dfTotal) <- c("Effect","SE","pVal")
colnames(dfTotal)
dfTotal$nam <- c("Diversity","Asynchrony","sqrt(Irigation)","sqrt(N use intensity)","Warfare","Time","Temperature instability","Precipitation instability")
dfTotal$Model <- "Combined"

dfCombined <- rbind(dfDiversity,dfAsynchrony,dfTotal)
dfCombined$Model <- factor(dfCombined$Model, levels = unique(dfCombined$Model))
dfCombined$nam <- factor(dfCombined$nam, levels = unique(dfCombined$nam))
dfCombined$labHeight <- dfCombined$Effect + 0.05
dfCombined[which(dfCombined$Effect<0),"labHeight"] <- dfCombined[which(dfCombined$Effect<0),"Effect"] - 0.05
dfCombined$lab <- ""
dfCombined[which(dfCombined$pVal<0.05),"lab"] <- "*"
dfCombined[which(dfCombined$pVal<0.01),"lab"] <- "**"
dfCombined[which(dfCombined$pVal<0.001),"lab"] <- "***"
dfCombined[which(dfCombined$pVal>=0.05),"lab"] <- "NS"
dfCombined$lab <- factor(dfCombined$lab, levels = unique(dfCombined$lab))

dfCombined <- dfCombined[unlist(lapply(1:8,function(i)seq(i,24,8))),]

dfText <- data.frame(xpos=sort(c(1:8-0.3,1:8,1:8+0.3)),ypos=dfCombined$labHeight,lab=dfCombined$lab,Model=dfCombined$Model)

a2 <- ggplot(data=dfCombined, aes(x=nam, y=Effect, fill=Model)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=Effect-SE, ymax=Effect+SE), width=.1,
                position=position_dodge(.9)) +
  geom_text(data=dfText,aes(x=xpos,y=ypos,label=lab),size=2)+  
  theme_classic() +  
  xlab("") +
  scale_y_continuous(breaks = round(seq(-0.6,0.6, by = 0.1),1),limits=c(-0.6,0.6)) +
  # scale_y_discrete("Standardized regression coefficient", seq(-0.3,0.3,0.1))+
  ylab("Standardized regression coefficient") +
  theme(axis.title.y=element_text(size=8)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8))+
  theme(axis.text.y = element_text(size=8))+
  scale_fill_manual(name = "Model",values = myColors)+
  geom_hline(yintercept=0)+
  theme(legend.position = c(0.9, 0.8))+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))+
  theme(legend.key.size = unit(0.2,"cm")) +
  theme(plot.margin = unit(c(0.2,0.3,-0.5,0.2), "cm")) 


##### Europe

## read DF
dfEurope <- read.csv("datasetsDerived/dataFinal_europe.csv")
head(dfEurope)



#### 1: explore relationship between diversity and asynchrony
dfDiversityAsynchronyEurope <- dfEurope[,c("NUTS_ID","timePeriod","Member","asynchrony","diversity")]
dfDiversityAsynchronyEurope$timePeriod <- factor(dfDiversityAsynchronyEurope$timePeriod,levels = c(1978,1988,1998,2008))
dfDiversityAsynchronyEurope$Member <- factor(dfDiversityAsynchronyEurope$Member,levels = c("old","new","not"))
cor.test(dfDiversityAsynchronyEurope$diversity,dfDiversityAsynchronyEurope$asynchrony,method='s')

## test if correlation between diversity and asynchrony decreases over time
modDiversityTimeEurope <- lmer(asynchrony ~ diversity + (1+diversity|timePeriod), data = dfDiversityAsynchronyEurope)
modDiversityLMEurope <- lm(asynchrony ~ diversity,data = dfDiversityAsynchronyEurope)
summary(modDiversityLMEurope)
anova(modDiversityTimeEurope,modDiversityLMEurope) 

modDiversityTimeFixedEurope=fixef(modDiversityTimeEurope)
r.squaredGLMM(modDiversityTimeEurope) 
modDiversityTimeGroupEurope <-   coef(modDiversityTimeEurope)$timePeriod

## plot
b1 <- ggplot(dfDiversityAsynchronyEurope, aes(x=diversity, y=asynchrony, color = timePeriod)) +
  geom_point(size=0.7) +
  scale_colour_manual(name="Time interval",values = vecColors[2:5], labels = c("1978-1987","1988-1997","1998-2007","2008-2017")) +
  scale_radius(range = c(2,12)) +
  geom_abline(intercept = modDiversityTimeGroupEurope[,1],slope = modDiversityTimeGroupEurope[,2],color=vecColors[2:5])+
  geom_abline(intercept = summary(modDiversityLMEurope)$coefficients[1,1],slope = summary(modDiversityLMEurope)$coefficients[2,1],color="black",linetype=3)+
  theme_classic() +  
  xlab("") +
  ylab("") + 
  theme(axis.title.x = element_text(size=8)) +  
  theme(axis.text.x = element_text(size=8)) +
  theme(axis.title.y = element_text(size=8)) +    
  theme(axis.text.y = element_text(size=8)) +
  # theme(legend.position = c(0.8, 0.17))+
  # theme(legend.title = element_text(size = 8),
  #       legend.text = element_text(size = 8)) +
  # theme(legend.key.size = unit(0.2,"cm")) +
  theme(legend.position = "none") +
  theme(plot.margin = unit(c(0.2,0.3,0.2,0.2), "cm")) 

## test if correlation between diversity and asynchrony differs between duration of EU membership
modDiversityMemberEurope <- lmer(asynchrony ~ diversity + (1+diversity|Member), data = dfDiversityAsynchronyEurope)
anova(modDiversityMemberEurope,modDiversityLMEurope) 

modDiversityMemberFixedEurope=fixef(modDiversityMemberEurope)
r.squaredGLMM(modDiversityMemberEurope) 
modDiversityMemberGroupEurope <-   coef(modDiversityMemberEurope)$Member

## plot
d1 <- ggplot(dfDiversityAsynchronyEurope, aes(x=diversity, y=asynchrony, color = Member)) +
  geom_point(size=0.7) +
  scale_colour_manual(name="Membership EU",values = vecColors2[1:3], labels = c("Old","New","NA")) +
  scale_radius(range = c(2,12)) +
  geom_abline(intercept = modDiversityMemberGroupEurope[,1],slope = modDiversityMemberGroupEurope[,2],color=vecColors2[1:3])+
  geom_abline(intercept = summary(modDiversityLMEurope)$coefficients[1,1],slope = summary(modDiversityLMEurope)$coefficients[2,1],color="black",linetype=3)+
  theme_classic() +  
  xlab("Diversity") +
  ylab("") + 
  theme(axis.title.x = element_text(size=8)) +  
  theme(axis.text.x = element_text(size=8)) +
  theme(axis.title.y = element_text(size=8)) +    
  theme(axis.text.y = element_text(size=8)) +
  theme(legend.position = c(0.8, 0.17))+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  theme(legend.key.size = unit(0.2,"cm")) +
  theme(plot.margin = unit(c(0.2,0.3,0.2,0.2), "cm")) 

#### 2: regression analyses

# transform variables
dfLogEurope=with(dfEurope,data.frame(NUTS_ID,
                                     stability = log(stability),
                                     diversity,asynchrony, 
                                     instabilityTemp,instabilityPrec,
                                     timePeriod
))
names(dfLogEurope)
head(dfLogEurope)

## scale predictors for standardized regression
dfPredictorsEurope=sapply(dfLogEurope[,-c(1:2)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterEurope=data.frame(Area=dfLogEurope[,1],stability=dfLogEurope[,2],dfPredictorsEurope)
head(dfCenterEurope)

cor(dfCenterEurope$diversity,dfCenterEurope$asynchrony)
cor(dfCenterEurope$stability,dfCenterEurope$asynchrony)

# models
modAsynchronyEurope <- lm(stability~asynchrony+timePeriod+instabilityTemp+instabilityPrec,data=dfCenterEurope)
summary(modAsynchronyEurope)

modDiversityEurope <- lm(stability~diversity+timePeriod+instabilityTemp+instabilityPrec,data=dfCenterEurope)
summary(modDiversityEurope)

modTotalEurope <- lm(stability~diversity+asynchrony+timePeriod+instabilityTemp+instabilityPrec,data=dfCenterEurope)
summary(modTotalEurope)

# barplot of effects: combine coefficents of both modesl
dfDiversityEurope <- data.frame(summary(modDiversityEurope)$coefficients)[2:5,c(1,2,4)]
names(dfDiversityEurope) <- c("Effect","SE","pVal")
colnames(dfDiversityEurope)
dfDiversityEurope$nam <- c("Diversity","Time","Temperature instability","Precipitation instability")
dfDiversityEurope <- rbind(dfDiversityEurope[1,],data.frame(Effect=0,SE=0,pVal=NA,nam="Asynchrony"),dfDiversityEurope[2:4,])
dfDiversityEurope$Model <- "Diversity"

dfAsynchronyEurope <- data.frame(summary(modAsynchronyEurope)$coefficients)[2:5,c(1,2,4)]
names(dfAsynchronyEurope) <- c("Effect","SE","pVal")
colnames(dfAsynchronyEurope)
dfAsynchronyEurope$nam <- c("Asynchrony","Time","Temperature instability","Precipitation instability")
dfAsynchronyEurope <- rbind(data.frame(Effect=0,SE=0,pVal=NA,nam="Diversity"),dfAsynchronyEurope)
dfAsynchronyEurope$Model <-  "Asynchrony"

dfTotalEurope <- data.frame(summary(modTotalEurope)$coefficients)[2:6,c(1,2,4)]
names(dfTotalEurope) <- c("Effect","SE","pVal")
colnames(dfTotalEurope)
dfTotalEurope$nam <- c("Diversity","Asynchrony","Time","Temperature instability","Precipitation instability")
dfTotalEurope$Model <- "Combined"


dfCombinedEurope <- rbind(dfDiversityEurope,dfAsynchronyEurope,dfTotalEurope)
dfCombinedEurope$Model <- factor(dfCombinedEurope$Model, levels = unique(dfCombinedEurope$Model))
dfCombinedEurope$nam <- factor(dfCombinedEurope$nam, levels = unique(dfCombinedEurope$nam))
dfCombinedEurope$labHeight <- dfCombinedEurope$Effect + 0.05
dfCombinedEurope[which(dfCombinedEurope$Effect<0),"labHeight"] <- dfCombinedEurope[which(dfCombinedEurope$Effect<0),"Effect"] - 0.05
dfCombinedEurope$lab <- ""
dfCombinedEurope[which(dfCombinedEurope$pVal<0.05),"lab"] <- "*"
dfCombinedEurope[which(dfCombinedEurope$pVal<0.01),"lab"] <- "**"
dfCombinedEurope[which(dfCombinedEurope$pVal<0.001),"lab"] <- "***"
dfCombinedEurope[which(dfCombinedEurope$pVal>=0.05),"lab"] <- "NS"
dfCombinedEurope$lab <- factor(dfCombinedEurope$lab, levels = unique(dfCombinedEurope$lab))

dfCombinedEurope <- dfCombinedEurope[unlist(lapply(1:5,function(i)seq(i,15,5))),]

dfTextEurope <- data.frame(xpos=sort(c(1:5-0.3,1:5,1:5+0.3)),ypos=dfCombinedEurope$labHeight,lab=dfCombinedEurope$lab,Model=dfCombinedEurope$Model)

b2 <- ggplot(data=dfCombinedEurope, aes(x=nam, y=Effect, fill=Model)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=Effect-SE, ymax=Effect+SE), width=.1,
                position=position_dodge(.9)) +
  geom_text(data=dfTextEurope,aes(x=xpos,y=ypos,label=lab),size=2)+  
  theme_classic() +  
  xlab("") +
  scale_y_continuous(breaks = round(seq(-0.6,0.6, by = 0.1),1),limits=c(-0.6,0.6)) +
  # scale_y_discrete("Standardized regression coefficient", seq(-0.3,0.3,0.1))+
  ylab("Standardized regression coefficient") +
  theme(axis.title.y=element_text(size=8)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8))+
  theme(axis.text.y = element_text(size=8))+
  scale_fill_manual(name = "Model",values = myColors)+
  geom_hline(yintercept=0)+
  # theme(legend.position = c(0.9, 0.8))+
  # theme(legend.title = element_text(size = 8),
  #       legend.text = element_text(size = 8))+
  # theme(legend.key.size = unit(0.2,"cm")) +
  theme(legend.position = "none") +
  theme(plot.margin = unit(c(0.2,0.3,-0.5,0.2), "cm")) 




###### SAVE FIGURES

## Fig 1
jpeg("results/Fig1.jpeg", width = 16.9, height = 16.9, units = 'cm', res = 600)

ggarrange(a1,b1,c1,d1,
          labels = letters[1:4],font.label=list(size=8),
          ncol = 2, nrow = 2,widths =  c(1,1))

dev.off()

## Fig2
jpeg("results/Fig2.jpeg", width = 16.9, height = 16.9*2/3, units = 'cm', res = 600)

ggarrange(a2,b2,
          labels = letters[1:2],font.label=list(size=8),
          ncol = 2, nrow = 1,widths =  c(1.5,1))

dev.off()


rm(list=ls())

