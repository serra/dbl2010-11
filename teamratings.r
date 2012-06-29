library(ggplot2)
library(reshape2)
library(lattice)
library(gplots)

# assumes that the gmStats data frame is initialized

######################################################################
#
# Utility functions
#
######################################################################

PageWithTrendAndBoxPlot <- function (df, title, medianForComp, yLim) {
  p <- ggplot(df, aes(x=game, y=value)) +
    opts(title=title)  +
    geom_hline(yintercept=medianForComp, linetype="dotted") +
    ylim(yLim)
  
  ptrend <- p +
    stat_smooth(aes(fill = variable, colour=variable), size=1) +
    geom_point(aes(shape=opponent, colour=variable)) + 
    scale_shape_manual(values=as.numeric(df$opponent)) 
  
  pboxplot <- p + 
    geom_boxplot(aes(x=variable, fill=variable))
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 4)))   
  print(ptrend, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:3))         
  print(pboxplot, vp = viewport(layout.pos.row = 1, layout.pos.col = 4))
}

######################################################################
#
# Output
#
######################################################################

pdf("output/dlb2010-11regseason.pdf", paper="a4r", width=12)

# Offensive and Defensive Ratings - Competition
ortgByTeamPlot <- ggplot(gmStats, aes(plg_ShortName, Ortg)) + 
                  geom_boxplot(aes(fill=plg_ShortName)) +
                  geom_hline(aes(yintercept=median(Ortg)), linetype="dotted") +
                  opts(title ="Offensive Rating") +
                  xlab("") + 
                  ylab("Points per 100 possessions")    
print(ortgByTeamPlot)

drtgByTeamPlot <- ggplot(gmStats, aes(plg_ShortName, Drtg)) + 
  geom_boxplot(aes(fill=plg_ShortName)) +
  geom_hline(aes(yintercept=median(Drtg)), linetype="dotted") +
  opts(title ="Defensive Rating by Team") +
  xlab("") + 
  ylab("Points per 100 possessions")    
print(drtgByTeamPlot)

nrtgByTeamPlot <- ggplot(gmStats, aes(plg_ShortName, Nrtg)) + 
  geom_boxplot(aes(fill=plg_ShortName)) +
  geom_hline(aes(yintercept=median(Nrtg)), linetype="dotted") +
  opts(title ="Net Rating by Team") +
  xlab("") + 
  ylab("Points Difference per 100 possessions")    
print(nrtgByTeamPlot)

ptsByTeamPlot <- ggplot(gmStats, aes(plg_ShortName, pts)) + 
  geom_boxplot(aes(fill=plg_ShortName)) +
  geom_hline(aes(yintercept=median(pts)), linetype="dotted") +
  opts(title ="Points") +
  xlab("") + 
  ylab("Points")    
print(ptsByTeamPlot)

ptsAllowedByTeamPlot <- ggplot(gmStats, aes(plg_ShortName, opp_pts)) + 
  geom_boxplot(aes(fill=plg_ShortName)) +
  geom_hline(aes(yintercept=median(opp_pts)), linetype="dotted") +
  opts(title ="Points Allowed") +
  xlab("") + 
  ylab("Points")    
print(ptsAllowedByTeamPlot)

ptsDiffByTeamPlot <- ggplot(gmStats, aes(plg_ShortName, (pts-opp_pts))) + 
  geom_boxplot(aes(fill=plg_ShortName)) +
  geom_hline(aes(yintercept=0), linetype="dotted") +
  geom_hline(aes(yintercept=-5), linetype="dotted") +
  geom_hline(aes(yintercept=5), linetype="dotted") +
  opts(title ="Points Difference") +
  xlab("") + 
  ylab("Points") 
                            
print(ptsDiffByTeamPlot)

# Game pace
p <- ggplot(gmStats, aes(plg_ShortName, avgps)) + 
  geom_boxplot(aes(fill=plg_ShortName)) +
  geom_hline(aes(yintercept=median(avgps)), linetype="dotted") +
  opts(title ="Game Pace") +
  xlab("") + 
  ylab("#Possessions")    
print(p)

# Performance Indicators - Competition

efgPctPlot <- ggplot(gmStats, aes(plg_ShortName, EFGpct)) + 
  geom_boxplot(aes(fill=plg_ShortName)) +
  geom_hline(aes(yintercept=median(EFGpct)), linetype="dotted") +
  opts(title ="Effective Field Goal % (EFG%)") +
  xlab("") + 
  ylim(c(0.2,0.8)) +
  ylab("EFG%")    
print(efgPctPlot)

orPctPlot <- ggplot(gmStats, aes(plg_ShortName, ORpct)) + 
  geom_boxplot(aes(fill=plg_ShortName)) +
  geom_hline(aes(yintercept=median(ORpct)), linetype="dotted") +
  opts(title ="Offensive Rebound % (OR%)") +
  xlab("") + 
  ylim(c(0.0,0.7)) +
  ylab("OR%")    
print(orPctPlot)

toPctPlot <- ggplot(gmStats, aes(plg_ShortName, TOpct)) + 
  geom_boxplot(aes(fill=plg_ShortName)) +
  geom_hline(aes(yintercept=median(TOpct)), linetype="dotted") +
  opts(title ="Turnovers per Possession") +
  xlab("") + 
  ylim(c(0.0,0.4)) +
  ylab("FTT%")    
print(toPctPlot)

fttPctPlot <- ggplot(gmStats, aes(plg_ShortName, FTTpct)) + 
  geom_boxplot(aes(fill=plg_ShortName)) +
  geom_hline(aes(yintercept=median(FTTpct)), linetype="dotted") +
  opts(title ="Free Throw Trips per Shooting Possession") +
  xlab("") + 
  ylim(c(0.0,0.3)) +
  ylab("FTT%")    
print(fttPctPlot)

# correlation of performance indicators

d = data.frame(gmStats$Nrtg, 
              gmStats$EFGpct, gmStats$ORpct, 
              gmStats$TOpct, gmStats$FTTpct
               # when evaluating the competion, 
               # it does not make sense to include opponent stats
               #gmStats$opp_EFGpct, gmStats$opp_ORpct, 
               #gmStats$opp_TOpct, gmStats$opp_FTTpct
              )
names(d) <- sub("^gmStats.", "", names(d))
corComp <- cor(d)

print(corComp,2)

corComp.m <- melt(corComp)
corPlot <- ggplot(corComp.m, aes(Var1, Var2, fill = value)) + 
            geom_tile() + 
            scale_fill_gradient2(low = "red",  high = "blue") +
            opts(title="Correlation matrix for complete competition")
print(corPlot)

forPlot <- gmStats[c("wed_ID","Nrtg","EFGpct","ORpct","TOpct","FTTpct",
                     "plg_ShortName","Home")] 
forPlot.m <- melt(forPlot, id=c("wed_ID", "plg_ShortName", "Home","Nrtg"))

p <- ggplot(forPlot.m, aes(value, Nrtg)) +
  geom_point(aes(shape=plg_ShortName, colour=plg_ShortName)) + 
  scale_shape_manual(values=as.numeric(forPlot.m$plg_ShortName)) +
  stat_smooth(method="lm") +
  facet_wrap(~variable,scales="free")

print(p)

# Offensive and Defensive Ratings - by team

medianRatingCompetion <- median(gmStats$Ortg)
yLim <- c(60, 170)

for(i in 1:10){
  plgID <- teams[i,1]
  plgName <- teams[i,2]
  forPlot <- gmStats[which(gmStats$plg_ID==plgID),]
  
  forPlot <- forPlot[c("Drtg","Ortg","opp_plg_ShortName","Home")] 
  forPlot$game = c(1:length(forPlot$Ortg))
  forPlot <- rename.vars(forPlot, c("opp_plg_ShortName"), c("opponent"))
  
  forPlot.m <- melt(forPlot, id=c("game", "opponent", "Home"))
  
  PageWithTrendAndBoxPlot(forPlot.m, plgName, medianRatingCompetion, yLim)
  
}

# battle of ratio's per team

for(i in 1:10){
  plgID <- teams[i,1]
  plgName <- teams[i,2]
  forPlot <- gmStats[which(gmStats$plg_ID==plgID),]
  d = data.frame(forPlot$Nrtg, forPlot$Ortg, forPlot$Drtg,
                 forPlot$EFGpct, forPlot$ORpct, 
                 forPlot$TOpct, forPlot$FTTpct,
                 forPlot$opp_EFGpct, forPlot$opp_ORpct, 
                 forPlot$opp_TOpct, forPlot$opp_FTTpct
                 )
  names(d) <- sub("^forPlot.", "", names(d))
  corTeam = cor(d)
  
  p <- levelplot(corTeam, main=paste("Performance correlation matrix for ",plgName), 
                 panel=function(...) {
                  arg <- list(...)
                  panel.levelplot(...)
                  panel.text(arg$x, arg$y, round(arg$z,2))})
  
  print(p)
}

# Details

yLim <- c(0, 0.8)

for(i in 1:10){
  plgID <- teams[i,1]
  plgName <- teams[i,2]
  forPlot <- gmStats[which(gmStats$plg_ID==plgID),]
    
  forPlot <- forPlot[c("opp_plg_ShortName","Home",
                       "EFGpct","ORpct","TOpct","FTTpct",
                       "opp_EFGpct","opp_ORpct","opp_TOpct","opp_FTTpct")] 
  forPlot$game = c(1:length(forPlot$EFGpct))
  forPlot <- rename.vars(forPlot, c("opp_plg_ShortName"), c("opponent"))
  
  PageWithTrendAndBoxPlot(melt(forPlot, measure=c("EFGpct", "opp_EFGpct")), 
                          plgName, median(gmStats$EFGpct), yLim)
  PageWithTrendAndBoxPlot(melt(forPlot, measure=c("ORpct", "opp_ORpct")), 
                          plgName, median(gmStats$ORpct), yLim)
  PageWithTrendAndBoxPlot(melt(forPlot, measure=c("TOpct", "opp_TOpct")), 
                          plgName, median(gmStats$TOpct), yLim)
  PageWithTrendAndBoxPlot(melt(forPlot, measure=c("FTTpct", "opp_FTTpct")), 
                          plgName, median(gmStats$FTTpct), yLim)

#   forCor <- data.frame(forPlot$Nrtg, forPlot$EFGpct,
#                        forPlot$ORpct, forPlot$TOpct, 
#                        forPlot$FTTpct)
#   forCorOpp <- data.frame(forPlot$Nrtg, forPlot$opp_EFGpct,
#                           forPlot$opp_ORpct, forPlot$opp_TOpct, 
#                           forPlot$opp_FTTpct)
  
}

# Shooting plays (2/3/FT)

layout(matrix(c(1,2,3,4,5,2,3,4), 2, 4, byrow=TRUE), widths=c(5,1,1,1))

yLim <- c(0, 60)

for(i in 1:10){
  plgID <- teams[i,1]
  plgName <- teams[i,2]
  forPlot <- gmStats[which(gmStats$plg_ID==plgID),]
  gameNrs <- c(1:36)
  
  # absolute
  plot(gameNrs, forPlot$FGA, 
       type="o", pch=1, lty=1, col="blue", 
       xlab=plgName, ylab="#Shots",
       ylim=yLim)
  lines(gameNrs, forPlot$FG3A, 
        type="o", pch=1, lty=1, col="purple", 
        xlab=plgName, 
        ylim=yLim)
  lines(gameNrs, forPlot$FTtrip, 
        type="o", pch=1, lty=1, col="red", 
        xlab=plgName, 
        ylim=yLim)
  
  abline(h=mean(forPlot$FGA), lty=3, col="blue")
  abline(h=mean(forPlot$FG3A), lty=3, col="purple")
  abline(h=mean(forPlot$FTtrip), lty=3, col="red")
  
  boxplot((forPlot$FGApct), data=forPlot, 
          xlab="2FGA", col="blue", 
          ylim=c(0.0, 1.0) )
  abline(h=median(gmStats$FGApct), lty=3)
  
  boxplot(forPlot$FGA3pct, data=forPlot, 
          xlab="3FGA", col="purple",
          ylim=c(0.0, 1.0) )
  abline(h=median(gmStats$FGA3pct), lty=3)
  
  boxplot(forPlot$FTTpct, data=forPlot, 
          xlab="FT trips", col="red",
          ylim=c(0.0, 1.0) )
  abline(h=median(gmStats$FTTpct), lty=3)
  
  # relative
  plot(gameNrs, forPlot$FGApct, 
       type="o", pch=1, lty=1, col="blue", 
       xlab=plgName, ylab="Shot Selection Ratio",
       ylim=c(0.0, 1.0))
  lines(gameNrs, forPlot$FGA3pct, 
        type="o", pch=1, lty=1, col="purple", 
        xlab=plgName, 
        ylim=c(0.0, 1.0))
  lines(gameNrs, forPlot$FTTpct, 
        type="o", pch=1, lty=1, col="red", 
        xlab=plgName, 
        ylim=c(0.0, 1.0))
  
#   # create a bar plot using ggplot   
#   fields <- c("FGA", "FG3A", "FTtrips", "TO")
#   plays <- forPlot[fields]
#   plays["gameNrs"] <- gameNrs
#   meltedPlays <- melt(plays, id=c('gameNrs'))
#   qplot(factor(gameNrs), data=meltedPlays, 
#         geom="bar", fill=variable, weight=value)
  
}

dev.off()



# print some table to screen

ratingTable <- sprintf("\n\n %-30s %5s %5s %5s %5s %5s \n", 
                       "Team",
                       "pts",
                       "opp",
                       "Ortg",
                       "Drtg",
                       "Nrtg")

for(i in 1:10){
  plgID <- teams[i,1]
  plgName <- teams[i,2]
  forPlot <- gmStats[which(gmStats$plg_ID==plgID),]
  gameNrs = c(1:36)
  
  row <- sprintf("%-30s %5.1f %5.1f %5.1f %5.1f %5.1f \n", 
                 plgName,
                 mean(forPlot$pts),
                 mean(forPlot$opp_pts),
                 mean(forPlot$Ortg),
                 mean(forPlot$Drtg),
                 mean(forPlot$Nrtg))
  ratingTable <- paste (ratingTable, row)
}

cat(ratingTable)
                 