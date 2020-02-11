
funCombinePlot <- function(mod1,mod2,namPred,tit,color,yLength,yname){
  
  if (!is.null(mod2))
  {
    dfCombined <- data.frame(rbind(
      summary(mod1)$coefficients[2:7,c(1,2,4)],
      summary(mod2)$coefficients[2,c(1,2,4)]
    ))
    vecName <- c("Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time","Area harvested") 
  }

  if (is.null(mod2))
  {
    dfCombined <- data.frame(
      summary(mod1)$coefficients[2:7,c(1,2,4)]
      )
    vecName <- c("Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time") 
  }
  
  names(dfCombined) <- c("Effect","SE","pVal")
  dfCombined$nam <- vecName 

  dfCombined$nam <- factor(dfCombined$nam, levels = unique(dfCombined$nam))
  dfCombined$labHeight <- dfCombined$Effect + dfCombined$SE + 0.04
  dfCombined[which(dfCombined$Effect<0),"labHeight"] <- dfCombined[which(dfCombined$Effect<0),"Effect"]- dfCombined[which(dfCombined$Effect<0),"SE"] - 0.04
  dfCombined$lab <- ""
  dfCombined[which(dfCombined$pVal<0.05),"lab"] <- "*"
  dfCombined[which(dfCombined$pVal<0.01),"lab"] <- "**"
  dfCombined[which(dfCombined$pVal<0.001),"lab"] <- "***"
  dfCombined[which(dfCombined$pVal>=0.05),"lab"] <- "NS"
  dfCombined$lab <- factor(dfCombined$lab, levels = unique(dfCombined$lab))
  dfText <- data.frame(xpos=1:length(vecName),ypos=dfCombined$labHeight,lab=dfCombined$lab)
  
  
  fig <- ggplot(data=dfCombined, aes(x=nam, y=Effect)) +
    geom_bar(stat="identity", position=position_dodge(), fill=color)+
    geom_errorbar(aes(ymin=Effect-SE, ymax=Effect+SE), width=.1,
                  position=position_dodge(.9)) +
    geom_text(data=dfText,aes(x=xpos,y=ypos,label=lab),size=1)+  
    theme_classic() +  
    xlab("") +
    scale_y_continuous(breaks = round(seq(-yLength,yLength, by = 0.2),1),limits=c(-yLength,yLength)) +
    ylab(yname) +
    theme(axis.title.y=element_text(size=8)) +
    theme(axis.text.y = element_text(size=8))+
    geom_hline(yintercept=0,size=0)+
    theme(plot.margin = unit(c(0.2,0.3,-0.5,0.2), "cm"))+
    theme(legend.position = "none")+
    ggtitle(tit)+
    theme(plot.title = element_text(size=8))
  if (namPred)
  {fig <- fig + theme(axis.text.x = element_text(angle = 45, hjust = 1,size=6))}
  if (!namPred)
  {fig <- fig + theme(axis.text.x=element_blank())}
  if (!is.null(mod2))
  {
    fig <- fig + geom_vline(xintercept=6.5,size=0)
  }
  fig
}


funCombineAll <- function(df1,df2,df3){

  dfCombined <- rbind(df1,df2,df3)
  dfCombined$Level <- factor(dfCombined$Level, levels = c("National","Subnational","Farm"))
  dfCombined$nam <- factor(dfCombined$nam, levels = unique(dfCombined$nam))
  dfCombined$labHeight <- dfCombined$Effect + dfCombined$SE + 0.03
  dfCombined[which(dfCombined$Effect<0),"labHeight"] <- dfCombined[which(dfCombined$Effect<0),"Effect"]- dfCombined[which(dfCombined$Effect<0),"SE"] - 0.03
  dfCombined$lab <- ""
  dfCombined[which(dfCombined$pVal<0.05),"lab"] <- "*"
  dfCombined[which(dfCombined$pVal<0.01),"lab"] <- "**"
  dfCombined[which(dfCombined$pVal<0.001),"lab"] <- "***"
  dfCombined[which(dfCombined$pVal>=0.05),"lab"] <- "NS"
  dfCombined$lab <- factor(dfCombined$lab, levels = unique(dfCombined$lab))
  dfCombined
}


funPlot <- function(df,dfText,yLength,yname,Legend){
  
  fig <- ggplot(data=df, aes(x=nam, y=Effect, fill=Level)) +
    geom_bar(stat="identity", position=position_dodge())+
    geom_errorbar(aes(ymin=Effect-SE, ymax=Effect+SE), width=.1,
                  position=position_dodge(.9)) +
    geom_text(data=dfText,aes(x=xpos,y=ypos,label=lab),size=2)+  
    theme_classic() +  
    xlab("") +
    scale_y_continuous(breaks = round(seq(-yLength,yLength, by = 0.1),1),limits=c(-yLength,yLength)) +
    ylab(yname) +
    theme(axis.title.y=element_text(size=8)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8))+
    theme(axis.text.y = element_text(size=8))+
    scale_fill_manual(name = "Level",values = myColors)+
    geom_hline(yintercept=0)+
    theme(plot.margin = unit(c(0.2,0.3,-0.5,0.2), "cm")) 
  if (Legend)
  {
    fig <- fig + theme(legend.position = c(0.9, 0.8))+
      theme(legend.title = element_text(size = 8),
            legend.text = element_text(size = 8))+
      theme(legend.key.size = unit(0.2,"cm")) 
  }

  if (!Legend)
  {
    fig <- fig + theme(legend.position = "none")
  }  
  fig
}




funPred <- function(dfPredict,dfCenter,dfLog,mod,variable,level)
{
  dfPredict[,variable] <-  seq(min(dfCenter[,variable]), max(dfCenter[,variable]), length.out = 1e3)
  
  # combined model
  pred <- exp(data.frame(predict(mod,newdata = dfPredict, 
                                          interval = "confidence")))    
  pred$variable <- dfPredict[,variable]*sd(dfLog[,variable])+mean(dfLog[,variable])
  pred$Level <- factor(level,levels=lev)
  pred
}

funPlotPred <- function(dfPred,yLength,labX,labY,Legend)
{
  fig <- ggplot(data = dfPred, aes(x = variable, y = stability, color=Level)) +
    # geom_point() +
    geom_line(data = dfPred, aes(y = fit,color=Level),size=0.5)+
    geom_ribbon(data = dfPred, aes(y = fit, ymin = lwr, ymax = upr, fill = Level), alpha = 0.5,colour=NA) +
    theme_classic() +
    theme(axis.title.x=element_text(size=8)) +
    theme(axis.title.y=element_text(size=8)) +
    xlab(labX)+
    ylab(labY)+
    ylim(0,yLength)+
    scale_colour_manual(name = "Level",values = myColors)+
    scale_fill_manual(name = "Level",values = myColors)
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"))

  if (Legend)
  {
    fig <- fig + theme(legend.position = c(0.9, 0.8))+
      theme(legend.title = element_text(size = 8),
            legend.text = element_text(size = 8))+
      theme(legend.key.size = unit(0.2,"cm")) 
  }
  
  if (!Legend)
  {
    fig <- fig + theme(legend.position = "none")
  }  
  fig
}  


funTables <- function(modN,modS,modF,r,nam)
{
  dfNationalTab <- data.frame(summary(modN)$coefficients)
  indLow <- which(dfNationalTab$Pr...t..<0.0001)
  dfNationalTab <- round(dfNationalTab,r)
  row.names(dfNationalTab) <- nam
  dfNationalTab$Estimate <- paste0(dfNationalTab$Estimate," (",dfNationalTab$Std..Error,")")
  dfNationalTab[indLow,4] <- "<0.0001"
  dfNationalTab <- dfNationalTab[,c(1,3,4)]
  colnames(dfNationalTab) <- c("Estimate (SE)","T","p-value") 
  dfNationalTab2 <- data.frame(est=c(summary(modN)$r.squared,AIC(modN)),tVal=c(NA,NA),pVal=c(NA,NA))
  dfNationalTab2$est <- round(dfNationalTab2$est,r)
  rownames(dfNationalTab2) <- c("R2","AIC")
  colnames(dfNationalTab2) <- colnames(dfNationalTab)
  dfNationalTab <- rbind(dfNationalTab,dfNationalTab2)
  
  dfSubnationalTab <- data.frame(summary(modS)$coefficients)
  indLow <- which(dfSubnationalTab$Pr...t..<0.0001)
  dfSubnationalTab <- round(dfSubnationalTab,r)
  row.names(dfSubnationalTab) <- NULL
  dfSubnationalTab$Estimate <- paste0(dfSubnationalTab$Estimate," (",dfSubnationalTab$Std..Error,")")
  dfSubnationalTab[indLow,4] <- "<0.0001"
  dfSubnationalTab <- dfSubnationalTab[,c(1,3,4)]
  colnames(dfSubnationalTab) <- c("Estimate (SE)","T","p-value") 
  dfSubnationalTab2 <- data.frame(est=c(summary(modS)$r.squared,AIC(modS)),tVal=c(NA,NA),pVal=c(NA,NA))
  dfSubnationalTab2$est <- round(dfSubnationalTab2$est,r)
  rownames(dfSubnationalTab2) <- c("R2","AIC")
  colnames(dfSubnationalTab2) <- colnames(dfSubnationalTab)
  dfSubnationalTab <- rbind(dfSubnationalTab,dfSubnationalTab2)
  
  dfFarmTab <- data.frame(summary(modF)$coefficients)
  indLow <- which(dfFarmTab$Pr...t..<0.0001)
  dfFarmTab <- round(dfFarmTab,r)
  row.names(dfFarmTab) <- NULL
  dfFarmTab$Estimate <- paste0(dfFarmTab$Estimate," (",dfFarmTab$Std..Error,")")
  dfFarmTab[indLow,4] <- "<0.0001"
  dfFarmTab <- dfFarmTab[,c(1,3,4)]
  colnames(dfFarmTab) <- c("Estimate (SE)","T","p-value") 
  dfFarmTab2 <- data.frame(est=c(summary(modF)$r.squared,AIC(modF)),tVal=c(NA,NA),pVal=c(NA,NA))
  dfFarmTab2$est <- round(dfFarmTab2$est,r)
  rownames(dfFarmTab2) <- c("R2","AIC")
  colnames(dfFarmTab2) <- colnames(dfFarmTab)
  dfFarmTab <- rbind(dfFarmTab,dfFarmTab2)  
  
  cbind(dfNationalTab,dfSubnationalTab,dfFarmTab)
  
}

funPredictCountry <- function(dfPred,mod,slope)
{
  dfPredictDiversity <- dfPred
  maxDiversity <- max(dfPredictDiversity$diversity)
  dfPredictDiversity[which(dfPredictDiversity$diversity<maxDiversity*0.5),"diversity"] <- maxDiversity*0.5
  dfPredictDiversity$fitDiversity1 <-  exp(as.numeric(predict(mod,newdata = dfPredictDiversity[,2:7])))
  dfPredictDiversity[which(dfPredictDiversity$diversity<maxDiversity*0.75),"diversity"] <- maxDiversity*0.75
  dfPredictDiversity$fitDiversity2 <-  exp(as.numeric(predict(mod,newdata = dfPredictDiversity[,2:7])))
  dfPredictDiversity[which(dfPredictDiversity$diversity<maxDiversity),"diversity"] <- maxDiversity
  dfPredictDiversity$fitDiversity3 <-  exp(as.numeric(predict(mod,newdata = dfPredictDiversity[,2:7])))  
  
  dfPredictFinal <- merge(dfNationalCombined[which(dfNationalCombined$timePeriod==2008),c("Country",slope)],dfPredictDiversity[,c("Country","fitDiversity1","fitDiversity2","fitDiversity3")])
  
  dfPredictFinal$dim1 <-car::recode(dfPredictFinal[,slope],"0:0.25=1; 0.25:0.5=2; 0.5:0.75=3;0.75:0.95=4;0.95:1.05=5;1.05:1.3333333=6;1.3333333:2=7;2:4=8;4:100=9;")
  dfPredictFinal$dim1 <- vecColMap[dfPredictFinal$dim1]
  dfPredictFinal$dim2 <-car::recode(dfPredictFinal$fitDiversity1,"0:0.25=1; 0.25:0.5=2; 0.5:0.75=3;0.75:0.95=4;0.95:1.05=5;1.05:1.3333333=6;1.3333333:2=7;2:4=8;4:100=9;")
  dfPredictFinal$dim2 <- vecColMap[dfPredictFinal$dim2]
  dfPredictFinal$dim3 <-car::recode(dfPredictFinal$fitDiversity2,"0:0.25=1; 0.25:0.5=2; 0.5:0.75=3;0.75:0.95=4;0.95:1.05=5;1.05:1.3333333=6;1.3333333:2=7;2:4=8;4:100=9;")
  dfPredictFinal$dim3 <- vecColMap[dfPredictFinal$dim3]
  dfPredictFinal$dim4 <-car::recode(dfPredictFinal$fitDiversity3,"0:0.25=1; 0.25:0.5=2; 0.5:0.75=3;0.75:0.95=4;0.95:1.05=5;1.05:1.3333333=6;1.3333333:2=7;2:4=8;4:100=9;")
  dfPredictFinal$dim4 <- vecColMap[dfPredictFinal$dim4]
  dfPredictFinal$id <- factor(dfPredictFinal$Country)
  
  dfPredictFinal
}

# funPredictCountry <- function(dfPred,mod,slope)
# {
#   dfPredictDiversity <- dfPred
#   maxDiversity <- max(dfPredictDiversity$diversity)
#   dfPredictDiversity[which(dfPredictDiversity$diversity<maxDiversity*0.75),"diversity"] <- maxDiversity*0.75
#   dfPredictDiversity <-  cbind(dfPredictDiversity,exp(data.frame(predict(mod,newdata = dfPredictDiversity[,2:7],interval = "confidence"))))
#   names(dfPredictDiversity)[8] <- "fitDiversity"
#   
#   dfPredictFertilizer <- dfPred
#   maxFertilizer <- max(dfPredictFertilizer$fertilizer)
#   dfPredictFertilizer[which(dfPredictFertilizer$diversity<maxFertilizer*0.75),"fertilizer"]  <- maxFertilizer*0.75
#   dfPredictFertilizer <-  cbind(dfPredictFertilizer,exp(data.frame(predict(mod,newdata = dfPredictFertilizer[,2:7],interval = "confidence"))))
#   names(dfPredictFertilizer)[8] <- "fitFertilizer"
#   
#   dfPredictIrrigation <- dfPred
#   maxIrrigation <- max(dfPredictIrrigation$irrigation)
#   dfPredictIrrigation[which(dfPredictIrrigation$irrigation<maxIrrigation*0.75),"irrigation"]  <- maxIrrigation*0.75
#   dfPredictIrrigation <-  cbind(dfPredictIrrigation,exp(data.frame(predict(mod,newdata = dfPredictIrrigation[,2:7],interval = "confidence"))))
#   names(dfPredictIrrigation)[8] <- "fitIrrigation"
#   
#   dfPredictAll <- dfPred
#   dfPredictAll[which(dfPredictAll$diversity<maxDiversity*0.75),"diversity"] <- maxDiversity*0.75
#   dfPredictAll[which(dfPredictAll$diversity<maxFertilizer*0.75),"fertilizer"]  <- maxFertilizer*0.75
#   dfPredictAll[which(dfPredictAll$irrigation<maxIrrigation*0.75),"irrigation"]  <- maxIrrigation*0.75
#   dfPredictAll <-  cbind(dfPredictAll,exp(data.frame(predict(mod,newdata = dfPredictAll[,2:7],interval = "confidence"))))
#   names(dfPredictAll)[8] <- "fitAll"
#   
#   dfPredictFinal <- merge(dfPredictDiversity[,c("Country","fitDiversity")],dfPredictFertilizer[,c("Country","fitFertilizer")],by="Country")
#   dfPredictFinal <- merge(dfPredictFinal,dfPredictIrrigation[,c("Country","fitIrrigation")],by="Country")
#   dfPredictFinal <- merge(dfPredictFinal,dfPredictAll[,c("Country","fitAll")],by="Country")
#   dfPredictFinal <- merge(dfPredictFinal, dfNationalCombined[which(dfNationalCombined$timePeriod==2008),c("Country",slope)])
#   
#   dfPredictFinal$dim1 <-car::recode(dfPredictFinal[,slope],"0:0.25=1; 0.25:0.5=2; 0.5:0.75=3;0.75:0.95=4;0.95:1.05=5;1.05:1.3333333=6;1.3333333:2=7;2:4=8;4:100=9;")
#   dfPredictFinal$dim1 <- vecColMap[dfPredictFinal$dim1]
#   dfPredictFinal$dim2 <-car::recode(dfPredictFinal$fitDiversity,"0:0.25=1; 0.25:0.5=2; 0.5:0.75=3;0.75:0.95=4;0.95:1.05=5;1.05:1.3333333=6;1.3333333:2=7;2:4=8;4:100=9;")
#   dfPredictFinal$dim2 <- vecColMap[dfPredictFinal$dim2]
#   dfPredictFinal$dim3 <-car::recode(dfPredictFinal$fitFertilizer,"0:0.25=1; 0.25:0.5=2; 0.5:0.75=3;0.75:0.95=4;0.95:1.05=5;1.05:1.3333333=6;1.3333333:2=7;2:4=8;4:100=9;")
#   dfPredictFinal$dim3 <- vecColMap[dfPredictFinal$dim3]
#   dfPredictFinal$dim4 <-car::recode(dfPredictFinal$fitIrrigation,"0:0.25=1; 0.25:0.5=2; 0.5:0.75=3;0.75:0.95=4;0.95:1.05=5;1.05:1.3333333=6;1.3333333:2=7;2:4=8;4:100=9;")
#   dfPredictFinal$dim4 <- vecColMap[dfPredictFinal$dim4]
#   dfPredictFinal$dim5 <-car::recode(dfPredictFinal$fitAll,"0:0.25=1; 0.25:0.5=2; 0.5:0.75=3;0.75:0.95=4;0.95:1.05=5;1.05:1.3333333=6;1.3333333:2=7;2:4=8;4:100=9;")
#   dfPredictFinal$dim5 <- vecColMap[dfPredictFinal$dim5]
#   dfPredictFinal$id <- factor(dfPredictFinal$Country)
#   dfPredictFinal
# }

funMapPlot <- function(map,dim)
{
  ggplot() +
  geom_map(data = map, map = map,
           aes(x = long, y = lat,  map_id=id, fill=dim),
           colour = "#7f7f7f", size=0.05) +
  scale_fill_identity()+
  theme_void()
}

funLegend <- function(title)
{
  df <- data.frame(x=1,y=1:9,col=factor(vecColMap))
  g.legend <- ggplot(df, aes(x,y,fill=factor(1:9)))+
    geom_tile()+
    scale_fill_manual(values=vecColMap)+
    theme(axis.line.x=element_blank(),
          axis.text.x=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank(),
          axis.title = element_text(size=8),axis.text = element_text(size=8))+
    xlim(0,2)+
    scale_y_continuous(breaks=c(1,3,5,7,9),labels=c("0", "0.5","1", "2",">4"),limits = c(-2,11))+
    labs(y=title,x="") 
  g.legend
}


funFig1 <- function(df,response,effect,colorVector,Legend)
{
  fig <- ggplot(df, aes(x=areaHarvested, y=response, color = factor(timePeriod))) +
    geom_point(size=0.3,alpha=0.2) +
    scale_colour_manual(name="Time interval",values = colorVector, labels = c("1968-1977","1978-1987","1988-1997","1998-2007","2008-2017")) +
    scale_radius(range = c(2,12)) +
    geom_abline(intercept = effect[,1],slope = effect[,2],color=colorVector)+
    theme_classic() +
    xlab("Area harvested (Mio. ha)") +
    ylab("log(Stability)") +
    theme(axis.title.x = element_text(size=8)) +
    theme(axis.text.x = element_text(size=8)) +
    theme(axis.title.y = element_text(size=8)) +
    theme(axis.text.y = element_text(size=8)) 

  
  if (Legend)
  {
    fig <- fig +  theme(legend.position = c(0.8, 0.17))+
      theme(legend.title = element_text(size = 8),
            legend.text = element_text(size = 8)) +
      theme(plot.margin = unit(c(0.2,0.2,1.5,0.2), "cm")) +
      theme(legend.key.size = unit(0.2,"cm"))
  }
  
  if (!Legend)
  {
    fig <- fig + theme(legend.position = "none")
  }  
  fig  
  
}



## bivariate maps
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



funFig2 <- function(dfTarget,variable,map,level)
{
  
  ## extract most recent period
  # dfGroups <- dfTarget[which(dfTarget$timePeriod==2008),]

  ## calculate quantiles

  
  trintVariableX <- as.numeric(quantile(dfTarget[,c(paste0(variable,".x"))],probs=seq(0,1,length.out = 4)))
  trintVariableY <- as.numeric(quantile(dfTarget[,c(paste0(variable,".y"))],probs=seq(0,1,length.out = 4)))
  # trintVariable <- c(0,30,60,1000)
  
  dfTarget$dim1 <-car::recode(dfTarget[,paste0(variable,".x")],"trintVariableX[1]:trintVariableX[2]=1; trintVariableX[2]:trintVariableX[3]=2; trintVariableX[3]:trintVariableX[4]=3;")
  dfTarget$dim2 <-car::recode(dfTarget[,paste0(variable,".y")],"trintVariableY[1]:trintVariableY[2]=1; trintVariableY[2]:trintVariableY[3]=2; trintVariableY[3]:trintVariableY[4]=3;")
  
  ## join to map
  dfTargetFinal <- merge(dfTarget[,c(level, "dim1","dim2")],grd)
  head(dfTargetFinal)
  dfTargetFinal$id <- as.character(dfTargetFinal[,level])
  mapsBivariate <- fortify(map,region=level)
  mapsBivariate = join(mapsBivariate, dfTargetFinal[,c("id","color")], by="id")
  
  fig <- ggplot() +
    geom_map(data = mapsBivariate, map = mapsBivariate,
             aes(x = long, y = lat,  map_id=id, fill=factor(color)),
             colour = "#7f7f7f", size=0.05) +
    scale_fill_identity()+
    theme_void()+
    theme(legend.position="none")
  
  fig
  
}
