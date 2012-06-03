# todo:
#  - configure limits for performace indicators
#  - output per team?
#  - legends on all charts

library(sqldf)
library(ggplot2)
library(reshape2)
library(lattice)
library(gplots)

regSeasonID <- 421
playOffID <- 627

sts <- read.csv2("./sources/heren_2010_2011.csv")
sts <- sts[which(sts$cmp_ID==regSeasonID),]

psData <- data.frame(sts$wed_ID, sts$plg_ID, sts$wed_UitPloeg, sts$wed_ThuisPloeg, 
                                sts$scu_FTA, sts$scu_FTM, sts$scu_FGA, sts$scu_FGM, sts$scu_3PM,  sts$scu_3PA, 
                                sts$scu_OffRebounds, sts$scu_DefRebounds, sts$scu_TurnOvers )

# prettify
names(psData) <- sub("^sts.", "", names(psData))        
names(psData) <- sub("scu_", "", names(psData))
names(psData) <- sub("OffRebounds", "OR", names(psData))
names(psData) <- sub("DefRebounds", "DR", names(psData))
names(psData) <- sub("TurnOvers", "TO", names(psData))
names(psData) <- sub("3P", "FG3", names(psData))
                 
games <- sqldf("select wed_ID from sts group by wed_ID")
teams <- sqldf(paste("select plg_ID, thuis_club as plg_Name from sts",
                     "where plg_ID = wed_ThuisPloeg",
                     "group by plg_ID, thuis_club"))

sqlThuis <- paste("select wed_ID, plg_ID, wed_UitPloeg, wed_ThuisPloeg, ", 
                  "max(wed_TeamOffRebThuis) as [OR], ",
                  "max(wed_TeamDefRebThuis) as DR, ", 
                  "max(wed_TeamTurnOverThuis) as [TO] ",
                  "from sts where plg_Id=wed_ThuisPloeg ",
                  "group by wed_Id, plg_ID, wed_UitPloeg, wed_ThuisPloeg")
stsThuis <- sqldf(sqlThuis)


# add zeros for missing columns
missingCols <- setdiff(names(psData), names(stsThuis))  # get missing cols
stsThuis[missingCols] <- 0                              # add to stsThuis, fill with 0 

sqlUit <- gsub("Thuis", "Uit", sqlThuis)
sqlUit <- gsub(", wed_UitPloeg, wed_UitPloeg,", ", wed_UitPloeg, wed_ThuisPloeg,", sqlThuis)  # bug fix :)
stsUit <- sqldf(sqlUit)
stsUit[missingCols] <- 0 

# merge the data frames to obtain a frame we can aggregate on by wed_ID and plg_ID
psData <- rbind(psData, stsThuis, stsUit)

# aggregate by game and team
agg <- aggregate(psData[5:13] , by=list(wed_ID=psData$wed_ID, plg_ID=psData$plg_ID, wed_UitPloeg=psData$wed_UitPloeg, wed_ThuisPloeg=psData$wed_ThuisPloeg), FUN=sum)

# add team name
agg <- sqldf("select agg.*, teams.plg_Name from agg inner join teams on agg.plg_ID=teams.plg_ID")

agg <- transform(agg, 
                 plg_ShortName = substr(plg_Name,0,8))

# now we join the tables, so that we have opposing numbers on the same game line
sqlGameLine = paste("select * from agg ",
                    "inner join agg opp on ",
                    "agg.wed_ID=opp.wed_ID and (",
                    "(agg.plg_ID = agg.wed_ThuisPloeg and opp.plg_ID = opp.wed_UitPloeg) or ",
                    "(agg.plg_ID = agg.wed_UitPloeg and opp.plg_ID = opp.wed_ThuisPloeg) ",
                    ")",
                    "",
                    "order by wed_ID"
                    )

gmStats <- sqldf(sqlGameLine) 

# pretify columns; opponents columns are prefixed with "opp_"
nrCols <- dim(gmStats)[2]/2
oppCols <- paste("opp", names(gmStats)[nrCols+1:nrCols], sep="_")
names(gmStats)[nrCols+1:nrCols] <- oppCols

######################################################################
#
# Calculate performance indicators
#
######################################################################

gmStats <- transform(gmStats,
                     FTtrips = 0.4*FTA,
                     opp_FTtrips =  0.4*opp_FTA)

gmStats <- transform(gmStats, 
                   pts = FTM + 2*FGM + 3*FG3M,
                   opp_pts =  opp_FTM + 2*opp_FGM + 3*opp_FG3M,
                   ps = TO + FTtrips + (FGA + FG3A) - 1.07 * (FGA + FG3A - FGM - FG3M) * OR / (OR + opp_DR),
                   opp_ps = opp_TO + 0.4*opp_FTA + (opp_FGA + opp_FG3A) - 1.07 * (opp_FGA + opp_FG3A - opp_FGM - opp_FG3M) * opp_OR / (opp_OR + DR)
                   )

gmStats <- transform(gmStats,
                   avgps = round((ps + opp_ps) / 2),
                   WARNING = abs(ps-opp_ps) > 4.0)

gmStats <- transform(gmStats,
                     Ortg = 100 * pts / avgps,
                     Drtg = 100 * opp_pts / avgps,
                     Home =  plg_ID == wed_ThuisPloeg)

gmStats <- transform(gmStats,
                     Nrtg = Ortg - Drtg)

gmStats <- transform(gmStats,
                     EFGpct = (FGM+1.5*FG3M)/(FGA+FG3A),
                     ORpct = OR / (OR + opp_DR),
                     TOpct = TO / avgps
                     )

gmStats <- transform(gmStats,
                     opp_EFGpct = (opp_FGM+1.5*opp_FG3M)/(opp_FGA+opp_FG3A),
                     opp_ORpct = opp_OR / (opp_OR + DR),
                     opp_TOpct = opp_TO / avgps,
                     opp_FTTpct = opp_FTtrips / (opp_FGA+opp_FG3A)
                     )

gmStats <- transform(gmStats,
                     FGApct = FGA / (FGA + FG3A + FTtrips),
                     FGA3pct = FG3A / (FGA + FG3A + FTtrips),
                     FTTpct = FTtrips / (FGA + FG3A + FTtrips)
                     )

######################################################################
#
# Utility functions
#
######################################################################

multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
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
  
  p <- ggplot(forPlot.m, aes(x=game, y=value)) +
    opts(title=plgName)  +
    geom_hline(yintercept=medianRatingCompetion, linetype="dotted") +
    ylim(yLim)

  ptrend <- p +
    stat_smooth(aes(fill = variable, colour=variable), size=1) +
    geom_point(aes(shape=opponent, colour=variable)) + 
    scale_shape_manual(values=as.numeric(forPlot.m$opponent)) 
  
  pboxplot <- p + 
    geom_boxplot(aes(x=variable, fill=variable))

  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 4)))   
  print(ptrend, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:3))         
  print(pboxplot, vp = viewport(layout.pos.row = 1, layout.pos.col = 4))
  
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
  gameNrs = c(1:36)
  
  plot(gameNrs, forPlot$EFGpct, 
       type="o", pch=1, lty=1, col="blue", 
       xlab=plgName, ylab="Ratio",
       ylim=yLim)
  lines(gameNrs, forPlot$opp_EFGpct, 
        pch=2, lty=1, col="red", type="o")
  
  boxplot(forPlot$EFGpct, data=forPlot, xlab="EFG%", col="blue", ylim=yLim )
  abline(h=median(gmStats$EFGpct), lty=3)
  boxplot(forPlot$opp_EFGpct, data=forPlot, xlab="Opp EFG%", col="red", ylim=yLim)
  abline(h=median(gmStats$EFGpct), lty=3)
  
  plot(gameNrs, forPlot$FTTpct, 
       type="o", pch=1, lty=1, col="blue", 
       xlab=plgName, ylab="Ratio",
       ylim=yLim)
  lines(gameNrs, forPlot$opp_FTTpct, 
        pch=2, lty=1, col="red", type="o")
  
  boxplot(forPlot$FTTpct, data=forPlot, xlab="FT trip %", col="blue", ylim=yLim )
  abline(h=median(gmStats$FTTpct), lty=3)
  boxplot(forPlot$opp_FTTpct, data=forPlot, xlab="Opp FT trip %", col="red", ylim=yLim)
  abline(h=median(gmStats$opp_FTTpct), lty=3)
  
  plot(gameNrs, forPlot$ORpct, 
       type="o", pch=1, lty=1, col="blue", 
       xlab=plgName, ylab="OR Ratio",
       ylim=yLim)
  lines(gameNrs, forPlot$opp_ORpct, 
        pch=2, lty=1, col="red", type="o")
  
  boxplot(forPlot$ORpct, data=forPlot, xlab="OR%", col="blue", ylim=yLim )
  abline(h=median(gmStats$ORpct), lty=3)
  boxplot(forPlot$opp_ORpct, data=forPlot, xlab="Opp OR%", col="red", ylim=yLim)
  abline(h=median(gmStats$opp_ORpct), lty=3)
  
  plot(gameNrs, forPlot$TOpct, 
       type="o", pch=1, lty=1, col="blue", 
       xlab=plgName, ylab="TO%",
       ylim=yLim)
  lines(gameNrs, forPlot$opp_TOpct, 
        pch=2, lty=1, col="red", type="o")
  
  boxplot(forPlot$TOpct, data=forPlot, xlab="TO%", col="blue", ylim=yLim )
  abline(h=median(gmStats$TOpct), lty=3)
  boxplot(forPlot$opp_TOpct, data=forPlot, xlab="Opp TO %", col="red", ylim=yLim)
  abline(h=median(gmStats$opp_TOpct), lty=3)

  
#   forCor <- data.frame(forPlot$Nrtg, forPlot$EFGpct,
#                        forPlot$ORpct, forPlot$TOpct, 
#                        forPlot$FTTpct)
#   forCorOpp <- data.frame(forPlot$Nrtg, forPlot$opp_EFGpct,
#                           forPlot$opp_ORpct, forPlot$opp_TOpct, 
#                           forPlot$opp_FTTpct)
  
}

# Game pace

boxplot(avgps ~ plg_ShortName, data=gmStats, 
        ylab="#Possessions")
abline(h=median(gmStats$avgps), lty=3)
title(main="Game Pace by team")

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
                 