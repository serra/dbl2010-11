CreateAdvancedStatsFiles("./sources/heren_2010_2011.csv")

regseas <- read.csv2("./output/heren_2010_2011_regseas_advanced_team_stats.csv")
PrintTeamRatings(regseas, "./output/heren_2010_2011_regseas_advanced_teamRatings.pdf")

playoffs <- read.csv2("./output/heren_2010_2011_playoffs_advanced_team_stats.csv")
PrintTeamRatings(playoffs, "./output/heren_2010_2011_playoffs_advanced_teamRatings.pdf")