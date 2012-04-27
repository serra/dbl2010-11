# summary I keep for when doin a fully automated run;
# right now I'm only working interactively

#clear workspace?

library(sqldf)

regSeasonID <- 421
playOffID <- 627

sts <- read.csv2("./sources/heren_2010_2011.csv")
sts <- sts[which(sts$cmp_ID==regSeasonID),]

psData <- data.frame(sts$wed_ID, sts$plg_ID, sts$wed_UitPloeg, sts$wed_ThuisPloeg, 
                                sts$scu_FTA, sts$scu_FTM, sts$scu_FGA, sts$scu_FGM, sts$scu_3PM,  sts$scu_3PA, 
                                sts$scu_OffRebounds, sts$scu_DefRebounds, sts$scu_TurnOvers )

# pretify
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

# teamById <- function (origId) {
#    return(sqldf(paste("select plg_Name from teams where plg_ID=",origId))[1])
# }

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
                     TOpct = TO / avgps,
                     FTpct = FTtrips/(FGA+FG3A)
                     )

gmStats <- transform(gmStats,
                     opp_EFGpct = (opp_FGM+1.5*opp_FG3M)/(opp_FGA+opp_FG3A),
                     opp_ORpct = opp_OR / (opp_OR + DR),
                     opp_TOpct = opp_TO / avgps,
                     opp_FTpct = opp_FTtrips / (opp_FGA+opp_FG3A)
                     )

######################################################################
#
# Output
#
######################################################################

#pdf("output/dlb2011-12regseason.pdf", paper="a4r", width=12)

# RATINGS - all teams

boxplot(Ortg ~ plg_ShortName, data=gmStats, xlab="Team", ylab="Ortg (Offensive Rating)")
abline(h=median(gmStats$Ortg), lty=3)

boxplot(Drtg ~ plg_ShortName, data=gmStats, xlab="Team", ylab="Drtg (Defensive Rating)")
abline(h=median(gmStats$Drtg), lty=3)

boxplot(Nrtg ~ plg_ShortName, data=gmStats, xlab="Team", ylab="Nrtg (Net Rating, Ort-Drtg)")
abline(h=median(gmStats$Nrtg), lty=3)

# Ratings - by team

opar <- par(no.readonly=TRUE)

layout(matrix(c(1,2,3,4,5,6,7,8), 2, 4, byrow=TRUE), widths=c(5,1,1,1))

yLim <- c(60, 170)

for(i in 1:10){
  plgID <- teams[i,1]
  plgName <- teams[i,2]
  forPlot <- gmStats[which(gmStats$plg_ID==plgID),]
  
  plot(forPlot$wed_ID, forPlot$Ortg, 
       type="o", pch=1, lty=1, col="blue", 
       xlab=plgName, ylab="Rating",
       ylim=yLim,
       xaxt="n")
  lines(forPlot$wed_ID, forPlot$Drtg, 
        pch=2, lty=2, col="red", type="o")
  abline(h=mean(forPlot$Ortg), lty=3, col="blue")
  abline(h=mean(forPlot$Drtg), lty=3, col="red")
  #   lines(forPlot$wed_ID, mean(forPlot$Drtg), 
#         pch=2, lty=1, col="red", type="o")
  
  legend("topleft", inset=.05, title="Legend", c("Ortg","Drtg"),
         lty=c(1, 2), pch=c(1, 2), col=c("blue", "red"))
  
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

yLim <- c(0, 1)

for(i in 1:10){
  plgID <- teams[i,1]
  plgName <- teams[i,2]
  forPlot <- gmStats[which(gmStats$plg_ID==plgID),]
  
  plot(forPlot$wed_ID, forPlot$EFGpct, 
       type="o", pch=1, lty=1, col="blue", 
       xlab=plgName, ylab="Ratio",
       ylim=yLim,
       xaxt="n")
  lines(forPlot$wed_ID, forPlot$opp_EFGpct, 
        pch=2, lty=1, col="red", type="o")
  
  boxplot(forPlot$EFGpct, data=forPlot, xlab="EFG%", col="blue", ylim=yLim )
  boxplot(forPlot$opp_EFGpct, data=forPlot, xlab="Opp EFG%", col="red", ylim=yLim)
  
  plot(forPlot$wed_ID, forPlot$FTpct, 
       type="o", pch=1, lty=1, col="blue", 
       xlab=plgName, ylab="Ratio",
       ylim=yLim,
       xaxt="n")
  lines(forPlot$wed_ID, forPlot$opp_FTpct, 
        pch=2, lty=1, col="red", type="o")
  
  boxplot(forPlot$FTpct, data=forPlot, xlab="FT trip %", col="blue", ylim=yLim )
  boxplot(forPlot$opp_FTpct, data=forPlot, xlab="Opp FT trip %", col="red", ylim=yLim)
  
}

# Game pace

boxplot(avgps ~ plg_ShortName, data=gmStats, xlab="Team", ylab="Possessions")





#dev.off()



                 