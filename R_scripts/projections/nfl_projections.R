# TO DO: scrape each page - only have QB right now
# #     figure out multiplier
#       standardize feature names
#       standardize names

library("XML")
library("stringr")
library("plyr")

nfl_qb <- readHTMLTable("http://fantasy.nfl.com/research/projections?position=O&statCategory=projectedStats&statSeason=2015&statType=weekProjectedStats&statWeek=5")$'NULL'

colnames(nfl_qb) <- c("Player", "Opp", "Pass_Yds", "Pass_TD", "Int", "Rush_Yds", "Rush_TD", "Reception_Yds", "Reception_TD", "FumTD", "2PT", "Fumble", "Points")

nfl_qb <- select(nfl_qb, Player, Pass_Yds, Pass_TD, Int, Rush_Yds, Rush_TD, Reception_Yds, Reception_TD, Fumble)

# Extract position and team

nfl_qb$Player <- gsub(" View Videos", "", nfl_qb$Player)
nfl_qb$Player <- gsub(" View Video", "", nfl_qb$Player)
nfl_qb$Player <- gsub(" View News", "", nfl_qb$Player)


nfl_qb$Team <- lapply(strsplit(as.character(nfl_qb$Player), "-"), "[", 2)
nfl_qb$Team <- str_trim(as.character(nfl_qb$Team), side = "left")
nfl_qb$Team <- gsub(' [A-z ]*', '' , nfl_qb$Team)


nfl_qb$Player <- gsub(' \\- .*$', '', nfl_qb$Player)
nfl_qb$first_name <- lapply(strsplit(as.character(nfl_qb$Player), " "), "[", 1)
nfl_qb$last_name <- lapply(strsplit(as.character(nfl_qb$Player), " "), "[", 2)
nfl_qb$Position <- lapply(strsplit(as.character(nfl_qb$Player), " "), "[", 3)

nfl_qb$first_name <- toupper(nfl_qb$first_name)
nfl_qb$last_name <- toupper(nfl_qb$last_name)
nfl_qb$Player <- paste(nfl_qb$last_name, nfl_qb$first_name, sep = "")

nfl_qb$Player <- gsub("[[:punct:]]", '', nfl_qb$Player)
