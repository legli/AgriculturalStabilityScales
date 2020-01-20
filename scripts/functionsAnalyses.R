
funCombine <- function(df1,df2,df3){

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
