# todo:
#  - configure limits for performace indicators
#  - output per team?
#  - legends on all charts


library(sqldf)
library(ggplot2)
library(reshape2)

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
# Output
#
######################################################################

pdf("output/dlb2010-11regseason.pdf", paper="a4r", width=12)
opar <- par(no.readonly=TRUE)

# Offensive and Defensive Ratings - Competition

boxplot(Ortg ~ plg_ShortName, data=gmStats, xlab="Team", ylab="Ortg (Offensive Rating)")
abline(h=median(gmStats$Ortg), lty=3)

boxplot(Drtg ~ plg_ShortName, data=gmStats, xlab="Team", ylab="Drtg (Defensive Rating)")
abline(h=median(gmStats$Drtg), lty=3)

boxplot(Nrtg ~ plg_ShortName, data=gmStats, xlab="Team", ylab="Nrtg (Net Rating, Ort-Drtg)")
abline(h=median(gmStats$Nrtg), lty=3)

# Performance Indicators - Competition

layout(matrix(c(1), 1, 1, byrow=TRUE))

boxplot(EFGpct ~ plg_ShortName, data=gmStats, 
        ylab="EFG%",
        ylim=c(0.2,0.8))
abline(h=median(gmStats$EFGpct), lty=3)
title(main="Effective Fieldgoal Percentage (EFG%) by team")

boxplot(ORpct ~ plg_ShortName, data=gmStats, 
        ylab="OR%", ylim=c(0,0.7))
abline(h=median(gmStats$ORpct), lty=3)
title(main="Offensive Rebound Percentage (OR%) by team: (OR/(OR+DR_opponent))")

boxplot(FTTpct ~ plg_ShortName, data=gmStats, 
        ylab="FTT%", ylim=c(0,0.3))
abline(h=median(gmStats$FTTpct), lty=3)
title(main="Number of Free Throw trips per Field Goal Attempt (FTT%) by team: FT trips / (FGA + 3FGA)")

boxplot(TOpct ~ plg_ShortName, data=gmStats, 
        ylab="TO%", 
        ylim=c(0,0.4))
abline(h=median(gmStats$TOpct), lty=3)
title(main="Turnover Percentage (TO%) by team: (TO/#possessions)")

# Offensive and Defensive Ratings - by team

layout(matrix(c(1,2,3,4), 1, 4, byrow=TRUE), widths=c(5,1,1,1))

yLim <- c(60, 170)

for(i in 1:10){
  plgID <- teams[i,1]
  plgName <- teams[i,2]
  forPlot <- gmStats[which(gmStats$plg_ID==plgID),]
  gameNrs = c(1:36)
  
  plot(gameNrs, forPlot$Ortg, 
       type="o", pch=1, lty=1, col="blue", 
       ylab="Rating",
       ylim=yLim)
  lines(gameNrs, forPlot$Drtg, 
        pch=2, lty=2, col="red", type="o")
  title(main=paste(plgName, " - Offensive and Defensive Ratings"))
  
  abline(h=median(forPlot$Ortg), lty=1, col="blue")
  abline(h=median(forPlot$Drtg), lty=2, col="red")
  # plot a line for the league average; 
  # which is the same for off and def rating:
  abline(h=median(gmStats$Drtg), lty=3)
  
  legend("topleft", inset=.05, title="Legend", c("Ortg","Drtg", "League Avg"),
         lty=c(1, 2, 3), col=c("blue", "red", "black"))
  
  boxplot(forPlot$Ortg, data=forPlot, xlab="Ortg", col="blue", ylim=yLim )
  abline(h=median(gmStats$Ortg), lty=3)
  boxplot(forPlot$Drtg, data=forPlot, xlab="Drtg", col="red", ylim=yLim)
  abline(h=median(gmStats$Drtg), lty=3)
  boxplot(forPlot$Nrtg, data=forPlot, xlab="Nrtg", ylim=c(-70,70))
  abline(h=median(gmStats$Nrtg), lty=3)
  
}

par(opar)

# battle of ratio's per team

layout(matrix(c(1,2,3,4,5,6), 2, 3, byrow=TRUE), widths=c(5,1,1))

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
}

par(opar)

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

par(opar)

dev.off()



                 