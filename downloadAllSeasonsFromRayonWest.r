# example: http://west.basketball.nl/db/overzicht/heren_ere_stats.pl?seizoen=2008-2009
downloadseason <- function(seasonId) {
  baseUrl <- "http://west.basketball.nl/db/overzicht/heren_ere_stats.pl?seizoen="
  url <- paste(baseUrl, seasonId, sep="")
  dest <- paste("./sources/raw/heren_", seasonId, ".csv", sep ="")
  download.file(url, dest, "auto")
}

seasonStarts <- 2000:2012
allSeasons <- paste(seasonStarts, seasonStarts+1, sep="-")

for(s in allSeasons) {
  downloadseason(s)
}
