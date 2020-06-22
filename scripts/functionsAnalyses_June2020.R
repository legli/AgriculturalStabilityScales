
############### Figure 1, 3, 5

funMaps <- function(dfInput,variable,map,level,b,colVec,tit,lab,legend)
{
  
  ## get df
  dfTarget <- dfInput
  
  labScale <- b
  if (max(dfTarget[,variable])>max(b))
  {
    labScale[length(labScale)] <- paste0(">",max(b))
  }
  
  if (min(dfTarget[,variable])<min(b))
  {
    labScale[1] <- paste0("<",min(b))
  } 
  
  # restrict df (for right colors)
  dfTarget[which(dfTarget[,variable]>max(b)),variable] <- max(b)  
  dfTarget[which(dfTarget[,variable]<min(b)),variable] <- min(b)
  dfTarget$dim <- dfTarget[,variable]
 

  ## join to map
  mapsBivariate <- fortify(map,region=level)
  mapsBivariate = merge(mapsBivariate, dfTarget[,c(level,"dim")], by=level)
  
  ## plot
  fig = ggplot() + 
    geom_sf(data = st_as_sf(mapCountry), fill = "white",size=0.1) +
    geom_sf(data = mapsBivariate, aes(fill = dim),size=0.1) +
    scale_fill_gradientn(colours=colVec,
                         values=rescale(b),
                         na.value="white", guide="colourbar",
                         name=tit,limits=c(min(b),max(b)),breaks=b, 
                         labels=labScale)+
    theme(legend.position = c(0.85, 0.5)) +
    theme_void() +
    coord_sf(xlim = extent(mapsBivariate)[1:2],
             ylim = extent(mapsBivariate)[3:4]) +
    guides(fill = guide_colorbar(barheight = 8,barwidth = 0.5))+
    theme(legend.title = element_text(size=6),legend.text = element_text(size=6))+
    labs(title = lab)+
    theme(title = element_text(hjust = 0, face= "bold",size=8))
  
  if (!legend)
  {
    fig <- fig + theme(legend.position = "none")
  }
  
  fig
}

############### Figure 2

funBoxplot <- function(dfInput,color,yname,tit){
  
  fig <- ggplot(data=dfInput, aes(x=Scale, y=Stability)) +
    geom_boxplot(color=color, fill="white")+
    theme_classic() +  
    ylim(0,8.1)+
    xlab("") +
    # scale_y_continuous(breaks = round(seq(-yLength,yLength, by = b),rnd),limits=c(-yLength,yLength)) +
    ylab(yname) +
    theme(axis.title.y=element_text(size=8)) +
    theme(axis.text.y = element_text(size=8))+
    theme(plot.margin = unit(c(0.2,0.3,-0.5,0.2), "cm"))+
    theme(legend.position = "none")+
    ggtitle(tit)+
    theme(plot.title = element_text(size=8))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size=6))
  
  fig
}

############### Figure 3

funEffect <- function(mod1,noPred,namPred,tit,color,yLength,b,rnd,yname,textPos,textNeg){
 
  dfCombined <-  data.frame(summary(mod1)$tTable[2:noPred,c(1,2,5)])
  names(dfCombined) <- c("Effect","SE","pVal")
  dfCombined$nam  <- namPred
  
  dfCombined$nam <- factor(dfCombined$nam, levels = unique(dfCombined$nam))
  dfCombined$labHeight <- dfCombined$Effect + dfCombined$SE + textPos
  dfCombined[which(dfCombined$Effect<0),"labHeight"] <- dfCombined[which(dfCombined$Effect<0),"Effect"]- dfCombined[which(dfCombined$Effect<0),"SE"] - textNeg
  dfCombined$lab <- ""
  dfCombined[which(dfCombined$pVal<0.05),"lab"] <- "*"
  dfCombined[which(dfCombined$pVal<0.01),"lab"] <- "**"
  dfCombined[which(dfCombined$pVal<0.001),"lab"] <- "***"
  dfCombined[which(dfCombined$pVal>=0.05),"lab"] <- "NS"
  dfCombined$lab <- factor(dfCombined$lab, levels = unique(dfCombined$lab))
  dfText <- data.frame(xpos=1:length(namPred),ypos=dfCombined$labHeight,lab=dfCombined$lab)
  
  
  fig <- ggplot(data=dfCombined, aes(x=nam, y=Effect)) +
    geom_bar(stat="identity", position=position_dodge(), fill=color)+
    geom_errorbar(aes(ymin=Effect-SE, ymax=Effect+SE), width=.1,
                  position=position_dodge(.9)) +
    geom_text(data=dfText,aes(x=xpos,y=ypos,label=lab),size=2)+  
    theme_classic() +  
    xlab("") +
    scale_y_continuous(breaks = round(seq(-yLength,yLength, by = b),rnd),limits=c(-yLength,yLength)) +
    ylab(yname) +
    theme(axis.title.y=element_text(size=8)) +
    theme(axis.text.y = element_text(size=8))+
    geom_hline(yintercept=0,size=0)+
    theme(plot.margin = unit(c(0.2,0.3,-0.5,0.2), "cm"))+
    theme(legend.position = "none")+
    ggtitle(tit)+
    theme(plot.title = element_text(size=8))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size=6))

  fig
}


############### Figures 4-6
funInteraction <- function(dfPredict,dfCenter,dfLog,moderator,modS,xlabel,ylabel,modLabel,yVal1,yVal2){
  
  dfPredictNew <- dfPredict
  dfPredictNew[,"diversity"] <-  seq(min(dfCenter[,"diversity"]), max(dfCenter[,"diversity"]), length.out = 1e3)
  
  # combined model
  dfPredictNew$Mean <- predict(modS,newdata = dfPredictNew,level=0)    
  dfPredictNew[,moderator] <-  1
  dfPredictNew$High <- predict(modS,newdata = dfPredictNew,level=0)    
  dfPredictNew[,moderator] <-  -1
  dfPredictNew$Low <- predict(modS,newdata = dfPredictNew,level=0)    
  
  dfPredictNew$Mean <- exp(dfPredictNew$Mean)
  dfPredictNew$High <- exp(dfPredictNew$High)
  dfPredictNew$Low <- exp(dfPredictNew$Low)
  
  dfPredictNew[,"diversity"] <- dfPredictNew[,"diversity"]*sd(dfLog[,"diversity"])+mean(dfLog[,"diversity"])  
  
  dfFinal <- dfPredictNew[,c("diversity","Mean","High","Low")]  %>% gather(Moderator, Stability, "Mean":"Low")
  dfFinal$Moderator <- factor(dfFinal$Moderator,levels = c("Low","Mean","High"))
  ggplot(data = dfFinal, aes(x = diversity, y = Stability,color=Moderator)) +
    # geom_point() +
    geom_line(size=0.5)+
    theme_classic() +
    theme(axis.title=element_text(size=8),axis.text=element_text(size=6)) +
    xlab(xlabel)+
    ylab(ylabel)+
    ylim(yVal1,yVal2)+
    theme(legend.position = c(0.8,0.2),legend.title = element_text(size = 6),legend.text = element_text(size = 6),legend.key.size = unit(0.5, "lines"))+
    guides(shape = guide_legend(override.aes = list(size = 0.5)),color = guide_legend(override.aes = list(size = 0.5)))+
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"))+
    scale_color_manual(name=modLabel,values = c("#E69F00","#999999","#56B4E9"))
}

############### Figures S4-S6

funPredRange <- function(predictor,dfPredict,dfCenter,dfLog,dfOriginal,modS,transResponse, trans,xlabel,ylabel,yVal1,yVal2,col){
  
  dfPredictNew <- dfPredict
  dfPredictNew[,predictor] <-  seq(min(dfCenter[,predictor]), max(dfCenter[,predictor]), length.out = 1e3)
  
  # combined model
  pred <- data.frame(predict(modS,newdata = dfPredictNew, 
                             interval = "confidence"))    
  
  if (trans==""){
    pred$variable <- dfPredictNew[,predictor]*sd(dfLog[,predictor])+mean(dfLog[,predictor])
    
  }
  if (trans=="sqrt"){
    pred$variable <- (dfPredictNew[,predictor]*sd(dfLog[,predictor])+mean(dfLog[,predictor]))^2
  }    
  
  if (trans=="log"){
    pred$variable <- exp(dfPredictNew[,predictor]*sd(dfLog[,predictor])+mean(dfLog[,predictor]))
  }       
  
  if (transResponse=="log"){
    pred$fit <- exp(pred$fit)
    pred$lwr <- exp(pred$lwr)
    pred$upr <- exp(pred$upr)
    
    
  }  
  
  dfOrig <- data.frame(response=c(min(dfOriginal[,c("stability","yield")]),max(dfOriginal[,c("stability","yield")])))
  dfOrig$variable <- c(min(dfOriginal[,predictor]),max(dfOriginal[,predictor]))
  dfOrig$Model <- ""
  ggplot(data = dfOrig, aes(x = variable, y = response)) +
    # geom_point() +
    geom_line(data = pred, aes(y = fit),size=0.5,color=col)+
    geom_ribbon(data = pred, aes(y = fit, ymin = lwr, ymax = upr),fill=col,alpha = 0.5,colour=NA) +
    theme_classic() +
    theme(axis.title=element_text(size=8),axis.text=element_text(size=6)) +
    xlab(xlabel)+
    ylab(ylabel)+
    ylim(yVal1,yVal2)+
    theme(legend.position = "none")+
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"))
}



############### Table S1


funTables <- function(modN,modS,modF,r,namCountry,namRegion,namFarm)
{
  dfNationalTab <- data.frame(summary(modN)$tTable[1:16,c(1,2,4,5)])
  # dfNationalTab <- data.frame(summary(modN)$coefficients)
  indLow <- which(dfNationalTab$p.value<0.0001)
  dfNationalTab <- round(dfNationalTab,r)
  row.names(dfNationalTab) <- namCountry
  dfNationalTab$Value <- paste0(dfNationalTab$Value," (",dfNationalTab$Std.Error,")")
  dfNationalTab[indLow,4] <- "<0.0001"
  dfNationalTab <- dfNationalTab[,c(1,3,4)]
  colnames(dfNationalTab) <- c("Estimate (SE)","T","p-value") 
  dfNationalTab$nam <- namCountry
  dfNationalTab2 <- data.frame(est=c(r.squaredGLMM(modN),AIC(modN)),tVal=c(NA,NA,NA),pVal=c(NA,NA,NA),nam=c("R2m","R2c","AIC"))
  dfNationalTab2$est <- round(dfNationalTab2$est,r)
  rownames(dfNationalTab2) <- c("R2m","R2c","AIC")
  colnames(dfNationalTab2) <- colnames(dfNationalTab)
  dfNationalTab <- rbind(dfNationalTab,dfNationalTab2)
  
  dfSubnationalTab <- data.frame(summary(modS)$tTable[1:16,c(1,2,4,5)])
  # dfSubnationalTab <- data.frame(summary(modS)$coefficients)
  indLow <- which(dfSubnationalTab$p.value<0.0001)
  dfSubnationalTab <- round(dfSubnationalTab,r)
  row.names(dfSubnationalTab) <- namRegion
  dfSubnationalTab$Value <- paste0(dfSubnationalTab$Value," (",dfSubnationalTab$Std.Error,")")
  dfSubnationalTab[indLow,4] <- "<0.0001"
  dfSubnationalTab <- dfSubnationalTab[,c(1,3,4)]
  colnames(dfSubnationalTab) <- c("Estimate (SE)","T","p-value") 
  dfSubnationalTab$nam <- namRegion
  dfSubnationalTab2 <- data.frame(est=c(r.squaredGLMM(modS),AIC(modS)),tVal=c(NA,NA,NA),pVal=c(NA,NA,NA),nam=c("R2m","R2c","AIC"))
  dfSubnationalTab2$est <- round(dfSubnationalTab2$est,r)
  rownames(dfSubnationalTab2) <- c("R2m","R2c","AIC")
  colnames(dfSubnationalTab2) <- colnames(dfSubnationalTab)
  dfSubnationalTab <- rbind(dfSubnationalTab,dfSubnationalTab2)
  
  dfFarmTab <- data.frame(summary(modF)$tTable[1:10,c(1,2,4,5)])
  # dfFarmTab <- data.frame(summary(modF)$coefficients)
  indLow <- which(dfFarmTab$p.value<0.0001)
  dfFarmTab <- round(dfFarmTab,r)
  # row.names(dfFarmTab) <- namFarm
  dfFarmTab$Value <- paste0(dfFarmTab$Value," (",dfFarmTab$Std.Error,")")
  dfFarmTab[indLow,4] <- "<0.0001"
  dfFarmTab <- dfFarmTab[,c(1,3,4)]
  colnames(dfFarmTab) <- c("Estimate (SE)","T","p-value") 
  dfFarmTab$nam <- namFarm
  # dfFarmTabEmpty <- data.frame(e=c(NA,NA),T=c(NA,NA),p=c(NA,NA))
  # colnames(dfFarmTabEmpty)<- c("Estimate (SE)","T","p-value") 
  # dfFarmTab <- rbind(dfFarmTab,dfFarmTabEmpty)
  dfFarmTab2 <- data.frame(est=c(r.squaredGLMM(modF),AIC(modF)),tVal=c(NA,NA,NA),pVal=c(NA,NA,NA),nam=c("R2m","R2c","AIC"))
  dfFarmTab2$est <- round(dfFarmTab2$est,r)
  rownames(dfFarmTab2) <- c("R2m","R2c","AIC")
  colnames(dfFarmTab2) <- colnames(dfFarmTab)
  dfFarmTab <- rbind(dfFarmTab,dfFarmTab2)  
  
  dfFinal <- merge(dfNationalTab,dfSubnationalTab,by="nam",all=T)
  dfFinal <- merge(dfFinal,dfFarmTab,by="nam",all=T)
  colnames(dfFinal) <- c("Name","Estimate (SE)","T","p-value","Estimate (SE)","T","p-value","Estimate (SE)","T","p-value")
  dfFinal
  
}


