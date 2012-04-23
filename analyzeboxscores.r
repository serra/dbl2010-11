# summary I keep for when doin a fully automated run;
# right now I'm only working interactively

#read csv file into statlinesraw

# order statlinesraw
lines <- statlinesraw[order(statlinesraw$wed_ID, statlinesraw$wed_ThuisPloeg, statlinesraw$wed_UitPloeg),]

sqlThuis <- "select wed_ID, plg_ID, max(wed_TeamOffRebThuis), max(wed_TeamDefRebThuis), max(wed_TeamStealThuis), max(wed_TeamTurnOverThuis) from lines where plg_Id=wed_ThuisPloeg group by wed_Id, plg_ID"
linesThuis <- sqldf(sqlThuis)

sqlUit <- gsub("Thuis", "Uit", sqlThuis)
linesUit=sqldf(sqlUit)