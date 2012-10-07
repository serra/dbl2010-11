fileName <- "./input/heren_2010_2011.csv"
season <- substr(fileName,9,23)

CreateAdvancedStatsFiles(fileName)

regseas <- read.csv2(sprintf("./output/%s_regseas_advanced_team_stats.csv", season))
PrintTeamRatings(regseas, sprintf("./output/%s_regseas_advanced_teamRatings.pdf", season))

playoffs <- read.csv2(sprintf("./output/%s_playoffs_advanced_team_stats.csv", season))
PrintTeamRatings(playoffs, sprintf("./output/%s_playoffs_advanced_teamRatings.pdf", season))
