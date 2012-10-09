# Processes all files in ./input/ and outputs csv files with advanced stats
#
# Optionally print additional team ratings report, buth this is commented out,
# because it take too much time.

source("CaluclateAdvancedGameStats.r")
source("ReportTeamRatings.r")

inputFiles <- list.files("./input/", full.names="true")

for(fileName in inputFiles) {
  message("processing ", fileName, " ...")
  season <- substr(fileName,9,23)
  
  CreateAdvancedStatsFiles(fileName)
  
  regseas <- read.csv2(sprintf("./output/%s_regseas_advanced_team_stats.csv", season))
  playoffs <- read.csv2(sprintf("./output/%s_playoffs_advanced_team_stats.csv", season))
  
  #PrintTeamRatings(regseas, sprintf("./output/%s_regseas_advanced_teamRatings.pdf", season))
  #PrintTeamRatings(playoffs, sprintf("./output/%s_playoffs_advanced_teamRatings.pdf", season))
}