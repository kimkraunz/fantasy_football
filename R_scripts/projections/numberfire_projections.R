# Works
# TO DO: match names (remove DST from defense)

library("XML")
library("stringr")
library("dplyr")

week = 7
season = 2015

nf_off <- readHTMLTable("http://www.numberfire.com/nfl/fantasy/fantasy-football-projections", stringsAsFactors = FALSE)$'complete-projection'
nf_off <- select(nf_off, V1, V19, V20)
colnames(nf_off) <- c("Player", "Points", "Cost")

nf_df <- readHTMLTable("http://www.numberfire.com/nfl/fantasy/fantasy-football-projections/d", stringsasFactors = FALSE)$'complete-projection'
nf_df <- select(nf_df, V1, V15, V16)
colnames(nf_df) <- c("Player", "Points", "Cost")

nf_k <- readHTMLTable("http://www.numberfire.com/nfl/fantasy/fantasy-football-projections/k", stringsasFactors = FALSE)$'complete-projection'
nf_k <- select(nf_k, V1, V19, V20)
colnames(nf_k) <- c("Player", "Points", "Cost")

nf <- rbind(nf_off, nf_df, nf_k)


# Clean up player and create Position and Team
nf$Position<- lapply(strsplit(as.character(nf$Player), "\\("), "[", 2)
nf$Team <- lapply(strsplit(as.character(nf$Position), "\\,"), "[", 2)
nf$Team <- gsub("\\)","", as.character(nf$Team)) 

                           
nf$Player<- lapply(strsplit(as.character(nf$Player), "\\("), "[", 1)
                           
nf$Position <- gsub("\\,.*","", nf$Position) 

# Clean up player's names
nf$Player <- gsub("[[:punct:]]", "", nf$Player)                           
nf$first_name <- lapply(strsplit(as.character(nf$Player), " "), "[", 1)
nf$last_name <- lapply(strsplit(as.character(nf$Player), " "), "[", 2)
                           
nf$first_name <- toupper(nf$first_name)
nf$last_name <- toupper(nf$last_name)
nf$Player <- paste(nf$last_name, nf$first_name, sep = "")
nf$Player <- str_replace(nf$Player, "^DST", "")
                           
#Remove special characters($)
nf$Cost <- substring(nf$Cost, 2)
nf <- select(nf, Player, Team, Points, Cost, Position)
                           
# convert to numeric and factor
nf$Points <- as.numeric(nf$Points)
nf$Team <- as.factor(nf$Team)
nf$Cost <- as.numeric(nf$Cost)
nf$Player <- as.character(nf$Player)
nf$Position <- as.factor(nf$Position)

# Remove teams/players with buys
nf <- nf[nf$Cost != 0, ]

#Save file
save(nf, file = paste(getwd(), "/fantasy_football/projections/week", week, "/numberfire.RData", sep=""))
write.csv(nf, file=paste(getwd(), "/fantasy_football/projections/week", week, "/numberfire.csv", sep=""), row.names=FALSE)

