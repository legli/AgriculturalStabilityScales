
funCombinePlot <- function(mod1,mod2,namPred,tit,color,yLength,yname){
  

  if (mod2=="all")
  {
    dfCombined <- data.frame(
      summary(mod1)$coefficients[2:7,c(1,2,4)]
      )
    # names(dfCombined) <- c("Effect","SE","pVal")
    vecName <- c("Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability","Precipitation instability","Time") 
  }

  if (mod2 == "farm")
  {
    dfCombined <- data.frame(
      summary(mod1)$coefficients[2:5,c(1,2,4)]
    )
    # names(dfCombined) <- c("Effect","SE","pVal")
    # dfCombined <- rbind(dfCombined,data.frame(Effect=c(0,0),SE=c(0,0),pVal=c(9999,9999)))
    vecName <- c("Diversity","sqrt(Fertilizer)","sqrt(Irigation)","Temperature instability") 
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
    theme(axis.title.y=element_text(size=6)) +
    theme(axis.text.y = element_text(size=6))+
    geom_hline(yintercept=0,size=0)+
    theme(plot.margin = unit(c(0.2,0.3,-0.5,0.2), "cm"))+
    theme(legend.position = "none")+
    ggtitle(tit)+
    theme(plot.title = element_text(size=8))
  if (namPred)
  {fig <- fig + theme(axis.text.x = element_text(angle = 45, hjust = 1,size=6))}
  if (!namPred)
  {fig <- fig + theme(axis.text.x=element_blank())}

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
  dfFarmTabEmpty <- data.frame(e=c(NA,NA),T=c(NA,NA),p=c(NA,NA))
  colnames(dfFarmTabEmpty)<- c("Estimate (SE)","T","p-value") 
  dfFarmTab <- rbind(dfFarmTab,dfFarmTabEmpty)
  dfFarmTab2 <- data.frame(est=c(summary(modF)$r.squared,AIC(modF)),tVal=c(NA,NA),pVal=c(NA,NA))
  dfFarmTab2$est <- round(dfFarmTab2$est,r)
  rownames(dfFarmTab2) <- c("R2","AIC")
  colnames(dfFarmTab2) <- colnames(dfFarmTab)
  dfFarmTab <- rbind(dfFarmTab,dfFarmTab2)  
  
  cbind(dfNationalTab,dfSubnationalTab,dfFarmTab)
  
}


funPredictChange <- function(ar,dfPred,mod,factor,lev)
{
  dfPredict <- dfPred
  reg <- dfPredict[,ar]
  dfPredict$fitOriginal <-  exp(as.numeric(predict(mod,newdata = dfPredict[,2:7])))
  dfPredict$diversityOld <- dfPredict$diversity
  dfPredict$fertilizerOld <- dfPredict$fertilizer
  dfPredict$irrigationOld <- dfPredict$irrigation

  maxDiversity <- max(dfPredict$diversity)*factor
  dfPredict[which(dfPredict$diversity<=maxDiversity),"diversity"] <- maxDiversity
  dfPredict$fitDiversity <-  exp(as.numeric(predict(mod,newdata = dfPredict[,2:7])))
  dfPredict$diversity <- dfPredict$diversityOld
  
  maxFertilizer <- max(dfPredict$fertilizer)*factor
  dfPredict[which(dfPredict$fertilizer<=maxFertilizer),"fertilizer"] <- maxFertilizer
  dfPredict$fitFertilizer <-  exp(as.numeric(predict(mod,newdata = dfPredict[,2:7])))
  dfPredict$fertilizer <- dfPredict$fertilizerOld
  
  maxIrrigation <- max(dfPredict$irrigation)*factor
  dfPredict[which(dfPredict$irrigation<=maxIrrigation),"irrigation"] <- maxIrrigation
  dfPredict$fitIrrigation <-  exp(as.numeric(predict(mod,newdata = dfPredict[,2:7])))
  dfPredict$irrigation <- dfPredict$irrigationOld
  
  dfPredict[which(dfPredict$diversity<=maxDiversity),"diversity"] <- maxDiversity
  dfPredict[which(dfPredict$fertilizer<=maxFertilizer),"fertilizer"] <- maxFertilizer
  dfPredict[which(dfPredict$irrigation<=maxIrrigation),"irrigation"] <- maxIrrigation
  
  dfPredict$fitAll <-  exp(as.numeric(predict(mod,newdata = dfPredict[,2:7])))
  
  dfFinal = rbind(
    data.frame(Area=reg,slope=dfPredict$fitOriginal,scenario="Original",plot="Diversity",level=lev),
    data.frame(Area=reg,slope=dfPredict$fitOriginal,scenario="Original",plot="Fertilizer",level=lev),
    data.frame(Area=reg,slope=dfPredict$fitOriginal,scenario="Original",plot="Irrigation",level=lev),
    data.frame(Area=reg,slope=dfPredict$fitOriginal,scenario="Original",plot="All",level=lev),
    data.frame(Area=reg,slope=dfPredict$fitDiversity,scenario="Scenario",plot="Diversity",level=lev),
    data.frame(Area=reg,slope=dfPredict$fitFertilizer,scenario="Scenario",plot="Fertilizer",level=lev),
    data.frame(Area=reg,slope=dfPredict$fitIrrigation,scenario="Scenario",plot="Irrigation",level=lev),
    data.frame(Area=reg,slope=dfPredict$fitAll,scenario="Scenario",plot="All",level=lev))
  
  
  # dfPredict$dim1 <-car::recode(dfPredict$fitDiversity,"0:0.25=1; 0.25:0.5=2; 0.5:0.75=3;0.75:0.95=4;0.95:1.05=5;1.05:1.3333333=6;1.3333333:2=7;2:4=8;4:100=9;")
  # dfPredict$dim1 <- vecColMap[dfPredict$dim1]
  # dfPredict$dim2 <-car::recode(dfPredict$fitFertilizer,"0:0.25=1; 0.25:0.5=2; 0.5:0.75=3;0.75:0.95=4;0.95:1.05=5;1.05:1.3333333=6;1.3333333:2=7;2:4=8;4:100=9;")
  # dfPredict$dim2 <- vecColMap[dfPredict$dim2]
  # dfPredict$dim3 <-car::recode(dfPredict$fitIrrigation,"0:0.25=1; 0.25:0.5=2; 0.5:0.75=3;0.75:0.95=4;0.95:1.05=5;1.05:1.3333333=6;1.3333333:2=7;2:4=8;4:100=9;")
  # dfPredict$dim3 <- vecColMap[dfPredict$dim3]
  # dfPredict$dim4 <-car::recode(dfPredict$fitAll,"0:0.25=1; 0.25:0.5=2; 0.5:0.75=3;0.75:0.95=4;0.95:1.05=5;1.05:1.3333333=6;1.3333333:2=7;2:4=8;4:100=9;")
  # dfPredict$dim4 <- vecColMap[dfPredict$dim4]
  
  
  # dfPredictDiversity[which(dfPredictDiversity$diversity<maxDiversity*0.75),"diversity"] <- maxDiversity*0.75
  # dfPredictDiversity$fitDiversity2 <-  exp(as.numeric(predict(mod,newdata = dfPredictDiversity[,2:7])))
  # dfPredictDiversity[which(dfPredictDiversity$diversity<maxDiversity),"diversity"] <- maxDiversity
  # dfPredictDiversity$fitDiversity3 <-  exp(as.numeric(predict(mod,newdata = dfPredictDiversity[,2:7])))  
  # 
  # dfPredictDiversity$dim1 <-car::recode(dfPredictDiversity$fitDiversity1,"0:0.25=1; 0.25:0.5=2; 0.5:0.75=3;0.75:0.95=4;0.95:1.05=5;1.05:1.3333333=6;1.3333333:2=7;2:4=8;4:100=9;")
  # dfPredictDiversity$dim1 <- vecColMap[dfPredictDiversity$dim1]
  # dfPredictDiversity$dim2 <-car::recode(dfPredictDiversity$fitDiversity2,"0:0.25=1; 0.25:0.5=2; 0.5:0.75=3;0.75:0.95=4;0.95:1.05=5;1.05:1.3333333=6;1.3333333:2=7;2:4=8;4:100=9;")
  # dfPredictDiversity$dim2 <- vecColMap[dfPredictDiversity$dim2]
  # dfPredictDiversity$dim3 <-car::recode(dfPredictDiversity$fitDiversity3,"0:0.25=1; 0.25:0.5=2; 0.5:0.75=3;0.75:0.95=4;0.95:1.05=5;1.05:1.3333333=6;1.3333333:2=7;2:4=8;4:100=9;")
  # dfPredictDiversity$dim3 <- vecColMap[dfPredictDiversity$dim3]

  dfFinal
}


funMapPlot <- function(map,dim,lab)
{
  # ggplot() +
  # geom_map(data = map, map = map,
  #          aes(x = long, y = lat,  map_id=id, fill=dim),
  #          colour = "#7f7f7f", size=0.05) +
  # scale_fill_identity()+
  # theme_void()
  
  ggplot() + 
    geom_sf(data = st_as_sf(mapCountry), fill = "white",size=0.1) +
    geom_sf(data = map, aes(fill = factor(dim)),size=0.1) +
    scale_fill_identity()+
    theme_void() +
    theme(legend.position="none") +
    coord_sf(xlim = extent(mapsBivariate)[1:2],
             ylim = extent(mapsBivariate)[3:4])+
    labs(title = lab)+
    theme(title = element_text(hjust = 0, face= "bold",size=8))
  
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



funFig2 <- function(dfInput,variable,map,level,lab)
{
  
  ## extract most recent period
  # dfGroups <- dfTarget[which(dfTarget$timePeriod==2008),]

  ## calculate quantiles
  dfTarget <- dfInput
  
  trintVariableX <- as.numeric(quantile(dfTarget[,variable],probs=seq(0,1,length.out = 4)))
  trintVariableY <- as.numeric(quantile(dfTarget[,paste0(variable,"Overall")],probs=seq(0,1,length.out = 4)))
  # trintVariable <- c(0,30,60,1000)
  
  dfTarget$dim1 <- 1
  dfTarget[which(dfTarget[,variable]>trintVariableX[2]&dfTarget[,variable]<=trintVariableX[3]),"dim1"] <- 2
  dfTarget[which(dfTarget[,variable]>trintVariableX[3]),"dim1"] <- 3
  dfTarget$dim2 <- 1
  dfTarget[which(dfTarget[,paste0(variable,"Overall")]>trintVariableY[2]&dfTarget[,variable]<=trintVariableY[3]),"dim2"] <- 2
  dfTarget[which(dfTarget[,paste0(variable,"Overall")]>trintVariableY[3]),"dim2"] <- 3  

  ## join to map
  dfTargetFinal <- merge(dfTarget[,c(level, "dim1","dim2")],grd)
  head(dfTargetFinal)
  mapsBivariate <- fortify(map,region=level)
  mapsBivariate = merge(mapsBivariate, dfTargetFinal[,c(level,"color")], by=level)
  
  
  fig = ggplot() + 
    geom_sf(data = st_as_sf(mapCountry), fill = "white",size=0.1) +
    geom_sf(data = mapsBivariate, aes(fill = factor(color)),size=0.1) +
    scale_fill_identity()+
    theme_void() +
    theme(legend.position="none") +
    coord_sf(xlim = extent(mapsBivariate)[1:2],
             ylim = extent(mapsBivariate)[3:4])+
    labs(title = lab)+
    theme(title = element_text(hjust = 0, face= "bold",size=8))
    
  fig
}

funPredRange <- function(predictor,dfPredict,dfCenter,dfLog,dfOriginal,modS,modY,trans,xlabel,ylabel,posX,posY,tStability,tYield){
  
  dfPredictNew <- dfPredict
  dfPredictNew[,predictor] <-  seq(min(dfCenter[,predictor]), max(dfCenter[,predictor]), length.out = 1e3)
  
  # combined model
  pred <- exp(data.frame(predict(modS,newdata = dfPredictNew, 
                                 interval = "confidence")))    
  
  if (trans==""){
    pred$variable <- dfPredictNew[,predictor]*sd(dfLog[,predictor])+mean(dfLog[,predictor])
    
  }
  if (trans=="sqrt"){
    pred$variable <- (dfPredictNew[,predictor]*sd(dfLog[,predictor])+mean(dfLog[,predictor]))^2
  }    
  
  pred$Model <- factor("Stability",levels=c("Stability","Yield"))
  
  
  predYield <- exp(data.frame(predict(modY,newdata = dfPredictNew, 
                                      interval = "confidence")))    
  
  if (trans==""){
    predYield$variable <- dfPredictNew[,predictor]*sd(dfLog[,predictor])+mean(dfLog[,predictor])
    
  }
  if (trans=="sqrt"){
    predYield$variable <- (dfPredictNew[,predictor]*sd(dfLog[,predictor])+mean(dfLog[,predictor]))^2
  }    
  
  predYield$Model <- factor("Yield",levels=c("Stability","Yield"))
  pred <- rbind(pred,predYield)
  
  
  dfOrig <- data.frame(response=c(min(dfOriginal[,c("slopeStability","slopeYield")]),max(dfOriginal[,c("slopeStability","slopeYield")])))
  dfOrig$variable <- c(min(dfOriginal[,predictor]),max(dfOriginal[,predictor]))
  dfOrig$Model <- ""
  ggplot(data = dfOrig, aes(x = variable, y = response, color=Model)) +
    # geom_point() +
    geom_line(data = pred, aes(y = fit,color=Model),size=0.5)+
    geom_ribbon(data = pred, aes(y = fit, ymin = lwr, ymax = upr, fill = Model), alpha = 0.5,colour=NA) +
    theme_classic() +
    theme(axis.title=element_text(size=6),axis.text=element_text(size=6)) +
    xlab(xlabel)+
    ylab(ylabel)+
    ylim(0,5)+
    scale_colour_manual(name = "Model",values = myColorsModel)+
    scale_fill_manual(name = "Model",values = myColorsModel)+
    theme(legend.position = c(posX, posY))+
    theme(legend.title = element_text(size = 6),
          legend.text = element_text(size = 6))+    
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"))+
    annotate("text", -Inf, Inf, label = tStability,hjust=-0.4,vjust=1.2,color="#4daf4a")+
    annotate("text", -Inf, Inf, label = tYield,hjust=-2,vjust=1.2,color="#045A8D")
}