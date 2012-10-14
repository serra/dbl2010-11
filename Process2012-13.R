# Processes all files in ./input/ and outputs csv files with advanced stats
#
# Optionally print additional team ratings report, buth this is commented out,
# because it take too much time.

source("CalculateAdvancedGameStats.r")
source("ReportTeamRatings.r")
source("download.r")

season <- "2012-2013" 

message("downloading ", season, " ...")
fileName <- downloadseason(season)

message("saved as ", fileName)
  
CreateAdvancedStatsFiles(fileName)

regseasTeam <- read.csv2(sprintf("./output/heren_%s_regseas_advanced_team_stats.csv", season))
regseasPlyr <- read.csv2(sprintf("./output/heren_%s_regseas_advanced_player_stats.csv", season))

#PrintTeamRatings(regseas, sprintf("./output/%s_regseas_advanced_teamRatings.pdf", season))
#PrintTeamRatings(playoffs, sprintf("./output/%s_playoffs_advanced_teamRatings.pdf", season))
