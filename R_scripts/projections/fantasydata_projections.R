library("XML")
library("stringr")
library("dplyr")
library("data.table")


week = 14
season = 2015

fantasydata_base <- "http://fantasydata.com/nfl-stats/fantasy-football-weekly-projections.aspx?fs=0&stype=0&sn=0&scope=1&w="

# Scraping data and selecting columns
fantasydata_qb <- readHTMLTable(paste0(fantasydata_base, week - 1, "&ew=", week - 1, "&s=&t=0&p=1&st=FantasyPoints&d=1&ls=FantasyPoints&live=false&pid=false&minsnaps=4"), as.data.frame = TRUE, stringsasfactors = FALSE, header = FALSE, skip.rows = 1)$StatsGrid

fantasydata_qb <- select(fantasydata_qb, V2, V3, V5, V21)

fantasydata_rb <- readHTMLTable(paste0(fantasydata_base, week - 1, "&ew=", week - 1, "&s=&t=0&p=2&st=FantasyPoints&d=1&ls=FantasyPoints&live=false&pid=false&minsnaps=4"), as.data.frame = TRUE, stringsasfactors = FALSE, header = FALSE, skip.rows = 1)$StatsGrid
fantasydata_rb <- select(fantasydata_rb, V2, V3, V5, V19)

fantasydata_wr <- readHTMLTable(paste0(fantasydata_base, week - 1, "&ew=", week - 1, "&s=&t=0&p=3&st=FantasyPoints&d=1&ls=FantasyPoints&live=false&pid=false&minsnaps=4"), as.data.frame = TRUE, stringsasfactors = FALSE, header = FALSE, skip.rows = 1)$StatsGrid
fantasydata_wr <- select(fantasydata_wr, V2, V3, V5, V22)

fantasydata_te <- readHTMLTable(paste0(fantasydata_base, week - 1, "&ew=", week - 1, "&s=&t=0&p=4&st=FantasyPoints&d=1&ls=FantasyPoints&live=false&pid=false&minsnaps=4"), as.data.frame = TRUE, stringsasfactors = FALSE, header = FALSE, skip.rows = 1)$StatsGrid
fantasydata_te <- select(fantasydata_te, V2, V3, V5, V22)

fantasydata_k <- readHTMLTable(paste0(fantasydata_base, week - 1, "&ew=", week - 1, "&s=&t=0&p=5&st=FantasyPoints&d=1&ls=FantasyPoints&live=false&pid=false&minsnaps=4"), as.data.frame = TRUE, stringsasfactors = FALSE, header = FALSE, skip.rows = 1)$StatsGrid
fantasydata_k <- select(fantasydata_k, V2, V3, V5, V15)

fantasydata_dst <- readHTMLTable(paste0(fantasydata_base, week - 1, "&ew=", week - 1, "&s=&t=0&p=6&st=FantasyPoints&d=1&ls=FantasyPoints&live=false&pid=false&minsnaps=4"), as.data.frame = TRUE, stringsasfactors = FALSE, header = FALSE, skip.rows = 1)$StatsGrid
fantasydata_dst <- select(fantasydata_dst, V2, V3, V5, V18)

# Defining column names
colnames(fantasydata_qb) <- colnames(fantasydata_rb) <- colnames(fantasydata_wr) <- colnames(fantasydata_te) <- colnames(fantasydata_k) <- colnames(fantasydata_dst) <-c("Player", "Position", "Team", "projected.points")

# Binding position data frames together
projections_fantasydata <- rbind(fantasydata_qb, fantasydata_rb, fantasydata_wr,fantasydata_te,fantasydata_k,fantasydata_dst)

# Adding projection source column
projections_fantasydata$proj.source <- "fantasydata"

# Format names
projections_fantasydata$first_name <- lapply(strsplit(as.character(projections_fantasydata$Player), " "), "[", 1)
projections_fantasydata$last_name <- lapply(strsplit(as.character(projections_fantasydata$Player), " "), "[", 2)

projections_fantasydata$first_name <- toupper(projections_fantasydata$first_name)
projections_fantasydata$last_name <- toupper(projections_fantasydata$last_name)

projections_fantasydata$Player <- paste0(projections_fantasydata$last_name, projections_fantasydata$first_name, "")

projections_fantasydata$Player <- gsub("[[:punct:]]", "", projections_fantasydata$Player)

# Save file to csv
write.csv(projections_fantasydata, file = paste0(getwd(), "/fantasy_football/projections_data/fantasydata_projections_week", week, ".csv", sep = ""))

