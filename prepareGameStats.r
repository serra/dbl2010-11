#
# Takes a csv file and calculates the teamStats data frame
#

library(sqldf)

###################
#
# Constants
# 
###################

regSeasonID <- 421
playOffID <- 627

ftaFactor <- 0.4     # Estimate percentage of free throws that end a possession.
                     # Originally estimated by Oliver at 0.4,
                     # but nowadays values of 0.44 (NBA) and 0.47 (EuroLeague)
                     # are used. To be investigated.

secondChanceFactor <- 1.07

################################
#
# Prepare teamStats data frame
#
################################

inputFile <- "./sources/heren_2010_2011.csv"
advancedTeamsStatsOutputFile <- "./output/heren_2010_2011_advanced_team_stats.csv"
advancedPlayerStatsOutputFile <- "./output/heren_2010_2011_advanced_player_stats.csv"



sts <- read.csv2(inputFile)
sts <- sts[which(sts$cmp_ID==regSeasonID),]
sts <- transform(sts, spl_ID = paste(plg_ID,  
                                      spl_Voornaam, 
                                      spl_tussen,
                                      spl_Achternaam))

psData <- data.frame(sts$wed_ID, sts$plg_ID, sts$wed_UitPloeg, sts$wed_ThuisPloeg, 
                     sts$scu_FTA, sts$scu_FTM, sts$scu_FGA, sts$scu_FGM, sts$scu_3PM,  
                     sts$scu_3PA, 
                     sts$scu_OffRebounds, sts$scu_DefRebounds, sts$scu_TurnOvers,
                     sts$scu_Minuten)

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
agg <- aggregate(psData[5:14] , by=list(wed_ID=psData$wed_ID, plg_ID=psData$plg_ID, wed_UitPloeg=psData$wed_UitPloeg, wed_ThuisPloeg=psData$wed_ThuisPloeg), FUN=sum)

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

teamStats <- sqldf(sqlGameLine) 

# pretify columns; opponents columns are prefixed with "opp_"
nrCols <- dim(teamStats)[2]/2
oppCols <- paste("opp", names(teamStats)[nrCols+1:nrCols], sep="_")
names(teamStats)[nrCols+1:nrCols] <- oppCols

# sanity checks ...

minutesNotEqual <- sqldf(paste("select wed_ID, plg_name, wed_ThuisPloeg, wed_UitPloeg, Minuten, opp_minuten",
                               "from teamStats ",
                               "where Minuten <> opp_minuten"))

nrGamesWithUnEqualMinutes <- (nrow(minutesNotEqual) / 2)
if(nrGamesWithUnEqualMinutes > 0) {
  warning(sprintf("%i game(s) with unequal minutes per team:", nrGamesWithUnEqualMinutes))
  print(minutesNotEqual)
}
#######################################################################
#
# Calculate performance indicators and add them to the teamStats frame
#
#######################################################################

teamStats <- transform(teamStats,
                     FTtrips = ftaFactor*FTA,
                     opp_FTtrips =  ftaFactor*opp_FTA)

teamStats <- transform(teamStats, 
                     pts = FTM + 2*FGM + 3*FG3M,
                     opp_pts =  opp_FTM + 2*opp_FGM + 3*opp_FG3M,
                     ps = TO + FTtrips + (FGA + FG3A) - secondChanceFactor * (FGA + FG3A - FGM - FG3M) * OR / (OR + opp_DR),
                     opp_ps = opp_TO + ftaFactor*opp_FTA + (opp_FGA + opp_FG3A) - secondChanceFactor * (opp_FGA + opp_FG3A - opp_FGM - opp_FG3M) * opp_OR / (opp_OR + DR)
                     )

teamStats <- transform(teamStats,
                     avgps = round((ps + opp_ps) / 2),
                     WARNING = abs(ps-opp_ps) > 4.0)

teamStats <- transform(teamStats,
                     Ortg = 100 * pts / avgps,
                     Drtg = 100 * opp_pts / avgps,
                     Home =  plg_ID == wed_ThuisPloeg)

teamStats <- transform(teamStats,
                     Nrtg = Ortg - Drtg)

teamStats <- transform(teamStats,
                     EFGpct = (FGM+1.5*FG3M)/(FGA+FG3A),
                     ORpct = OR / (OR + opp_DR),
                     TOpct = TO / avgps
                     )

teamStats <- transform(teamStats,
                     opp_EFGpct = (opp_FGM+1.5*opp_FG3M)/(opp_FGA+opp_FG3A),
                     opp_ORpct = opp_OR / (opp_OR + DR),
                     opp_TOpct = opp_TO / avgps,
                     opp_FTTpct = opp_FTtrips / (opp_FGA+opp_FG3A)
                     )

# shooting distribution
teamStats <- transform(teamStats,
                     FGApct = FGA / (FGA + FG3A + FTtrips),
                     FGA3pct = FG3A / (FGA + FG3A + FTtrips),
                     FTTpct = FTtrips / (FGA + FG3A + FTtrips)
                     )

# shooting percentages
teamStats <- transform(teamStats,
                     FG2pct = FGM / FGA,
                     FG3pct = FG3M/ FG3A,
                     FTpct = FTM / FTA
                     )

# # point by category
# teamStats <- transform(teamStats,
#                      FG2pts = FGM*2,
#                      FG3pts = FG3M*3,
#                      FTpts = FTM
#                      )
# 
# # point contributions
# teamStats <- transform(teamStats,
#                      ContrFG2pts = FG2pts/pts,
#                      ContrFG3pts = FG3pts/pts,
#                      ContrFTpts = FTpts/pts
#                      )

#######################################################################
#
# Calculate player performanceindicators by game
# and join this frame on the teamStats
#
#######################################################################

playerStats <- data.frame(sts$wed_ID, sts$plg_ID, sts$scu_Minuten,
                         sts$scu_FTA, sts$scu_FTM, sts$scu_FGA, sts$scu_FGM, sts$scu_3PM,  
                         sts$scu_3PA, 
                         sts$scu_OffRebounds, sts$scu_DefRebounds, sts$scu_TurnOvers,
                         sts$spl_ID)

# prettify
names(playerStats) <- sub("^sts.", "", names(playerStats))        
names(playerStats) <- sub("scu_", "spl_", names(playerStats))
names(playerStats) <- sub("OffRebounds", "OR", names(playerStats))
names(playerStats) <- sub("DefRebounds", "DR", names(playerStats))
names(playerStats) <- sub("TurnOvers", "TO", names(playerStats))
names(playerStats) <- sub("3P", "FG3", names(playerStats))

sqlPlayersJoinTeam <- paste("select * from teamStats tm ",
                            "inner join playerStats ply on ",
                            "tm.wed_ID = ply.wed_ID and tm.plg_ID = ply.plg_ID ", 
                            "order by wed_ID")

playerStats <- sqldf(sqlPlayersJoinTeam)

# shooting indicators:
playerStats <- transform(playerStats,
                        spl_PTS = spl_FTM + 2*spl_FGM + 3*spl_FG3M)
                        

playerStats <- transform(playerStats,
                        spl_FTperFG = spl_FTA / (spl_FGA+spl_FG3A),
                        spl_FG3AperFG =  spl_FG3A / (spl_FGA+spl_FG3A),
                        spl_EFGpct = (1.5*spl_FG3M + spl_FGM) / (spl_FGA+spl_FG3A),
                        spl_TSpct = (spl_PTS / (2 * (spl_FGA + spl_FG3A + ftaFactor * spl_FTA)))
                        )

#########
#
# output
#
#########

write.csv2(teamStats, advancedTeamsStatsOutputFile)
write.csv2(teamStats, advancedPlayerStatsOutputFile)
