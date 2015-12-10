

library("XML")
library("stringr")
library("dplyr")
library(caret)
library(lpSolve)

qb_fp <- readHTMLTable("http://www.fantasypros.com/nfl/fanduel-cheatsheet.php?position=QB", stringsAsFactors = FALSE)$data
rb_fp <- readHTMLTable("http://www.fantasypros.com/nfl/fanduel-cheatsheet.php?position=RB", stringsAsFactors = FALSE)$data
wr_fp <- readHTMLTable("http://www.fantasypros.com/nfl/fanduel-cheatsheet.php?position=WR", stringsAsFactors = FALSE)$data
te_fp <- readHTMLTable("http://www.fantasypros.com/nfl/fanduel-cheatsheet.php?position=TE", stringsAsFactors = FALSE)$data
k_fp <- readHTMLTable("http://www.fantasypros.com/nfl/fanduel-cheatsheet.php?position=K", stringsAsFactors = FALSE)$data
dst_fp <- readHTMLTable("http://www.fantasypros.com/nfl/fanduel-cheatsheet.php?position=DST", stringsAsFactors = FALSE)$data

# Keep only players that have projected points > 0
qb_fp <- filter(qb_fp, Proj.Points >= 0)
wr_fp <- filter(wr_fp, Proj.Points >= 0)
rb_fp <- filter(rb_fp, Proj.Points >= 0)
te_fp <- filter(te_fp, Proj.Points >= 0)
k_fp <- filter(k_fp, Proj.Points >= 0)
dst_fp <- filter(dst_fp, Proj.Points >= 0)

# Add position
qb_fp <- mutate(qb_fp, pos = "QB")
wr_fp <- mutate(wr_fp, pos = "WR")
rb_fp <- mutate(rb_fp, pos = "RB")
te_fp <- mutate(te_fp, pos = "TE")
k_fp <- mutate(k_fp, pos = "K")
dst_fp <- mutate(dst_fp, pos = "DST")

# Merge position tables into one data frame
projections_fp <- rbind_list(qb_fp, wr_fp, rb_fp, te_fp, k_fp, dst_fp)

# Keep only Player, Proj.Points, Salary, and pos columns
projections_fp <- select(projections_fp, Player, Proj.Points, Salary, pos)

# Extract team from Player
projections_fp$Team<- lapply(strsplit(as.character(projections_fp$Player), "\\(+"), "[", 2)
projections_fp$Team<- lapply(strsplit(as.character(projections_fp$Team), "\\s+"), "[", 1)
projections_fp$Team <- as.factor(unlist(projections_fp$Team))

# Clean up Player
projections_fp$first_name <- lapply(strsplit(as.character(projections_fp$Player), " "), "[", 1)
projections_fp$last_name <- lapply(strsplit(as.character(projections_fp$Player), " "), "[", 2)
projections_fp$first_name <- toupper(projections_fp$first_name)
projections_fp$last_name <- toupper(projections_fp$last_name)

projections_fp$Player <- paste0(projections_fp$last_name, projections_fp$first_name, sep = "")

#Remove special characters
projections_fp$Proj.Points <- gsub(" pts", "", projections_fp$Proj.Points)
projections_fp$Player <- gsub("[[:punct:]]", "", projections_fp$Player)

projections_fp$pos <- as.factor(projections_fp$pos)
projections_fp$Proj.Points <- as.numeric(projections_fp$Proj.Points)

# add source column
projections_fp$proj.source <- "fantasypros"

projections_fp <- select(projections_fp, Player, Proj.Points, Team, pos)
colnames(projections_fp) <- c("Player", "Points", "Team", "Position")

# write projected points to csv
write.csv(projections_fp, file = paste0(getwd(), "/fantasy_football/projections_data/fp_projections_", Sys.Date(), ".csv", sep = ""))
