# summary I keep for when doin a fully automated run;
# right now I'm only working interactively

#clear workspace?

library(sqldf)
sts <- read.csv2("./sources/heren_2010_2011.csv")

psData <- data.frame(sts$wed_ID, sts$plg_ID, sts$wed_UitPloeg, sts$wed_ThuisPloeg, 
                                sts$scu_FTA, sts$scu_FTM, sts$scu_FGA, sts$scu_FGM, sts$scu_3PM,  sts$scu_3PA, 
                                sts$scu_OffRebounds, sts$scu_DefRebounds, sts$scu_TurnOvers )
names(psData) <- sub("^sts.", "", names(psData))        # hack - remove prefixes to maintain identical column names for merge
                 
games <- sqldf("select wed_ID from sts group by wed_ID")
teams <- sqldf(paste("select plg_ID, thuis_club from sts",
                     "where plg_ID = wed_ThuisPloeg",
                     "group by plg_ID, thuis_club"))

# order statstsraw
#sts <- sts[order(sts$wed_ID, statlinesraw$wed_ThuisPloeg, statlinesraw$wed_UitPloeg),]

sqlThuis <- paste("select wed_ID, plg_ID, wed_UitPloeg, wed_ThuisPloeg, ", 
                  "max(wed_TeamOffRebThuis) as scu_OffRebounds, ",
                  "max(wed_TeamDefRebThuis) as scu_DefRebounds, ", 
                  "max(wed_TeamTurnOverThuis) as scu_TurnOvers ",
                  "from sts where plg_Id=wed_ThuisPloeg group by wed_Id, plg_ID, wed_UitPloeg, wed_ThuisPloeg")
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

sqlGameLine = paste("select * from agg ",
                    "inner join agg opp on ",
                    "agg.wed_ID=opp.wed_ID and (",
                    "(agg.plg_ID = agg.wed_ThuisPloeg and opp.plg_ID = opp.wed_UitPloeg) or ",
                    "(agg.plg_ID = agg.wed_UitPloeg and opp.plg_ID = opp.wed_ThuisPloeg) ",
                    ")",
                    "order by wed_ID"
                    )

# pretify columns
gmStats <- sqldf(sqlGameLine)
nrCols <- dim(gmStats)[2]/2
oppCols <- paste("opp", names(gmStats)[nrCols+1:nrCols], sep="_")
names(gmStats)[nrCols+1:nrCols] <- oppCols
names(gmStats) <- sub("scu_", "", names(gmStats))
names(gmStats) <- sub("OffRebounds", "OR", names(gmStats))
names(gmStats) <- sub("DefRebounds", "DR", names(gmStats))
names(gmStats) <- sub("TurnOvers", "TO", names(gmStats))
names(gmStats) <- sub("3P", "FG3", names(gmStats))

gmStats <- transform(gmStats, 
                   pts = FTM + 2*FGM + 3*FG3M,
                   opp_pts =  opp_FTM + 2*opp_FGM + 3*opp_FG3M,
                   ps = TO + 0.4*FTA + (FGA + FG3A) - 1.07 * (FGA + FG3A - FGM - FG3M) * OR / (OR + opp_DR),
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

pdf("ratings.pdf")

boxplot(Ortg ~ plg_ID, data=gmStats, xlab="Team", ylab="Ortg (Offensive Rating)")
boxplot(Drtg ~ plg_ID, data=gmStats, xlab="Team", ylab="Drtg (Defensive Rating)")
boxplot(Nrtg ~ plg_ID, data=gmStats, xlab="Team", ylab="Nrtg (Net Rating, Ort-Drtg)")

dev.off()

pdf("streak.pdf")

plot(Ortg ~ wed_ID, data=gmStats, xlab="Team", ylab="Ortg (Offensive Rating)")

dev.off()



                 