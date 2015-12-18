

14&ew=14&s=&t=0&p=2&st=FantasyPointsFanDuel&d=1&ls=FantasyPointsFanDuel&live=false&pid=false&minsnaps=4

library("XML")
library("stringr")
library("dplyr")


week = 14
season = 2015

salaries_base <- "http://fantasydata.com/nfl-stats/daily-fantasy-football-salary-and-projection-tool.aspx?fs=0&stype=0&sn=0&scope=0&w="


# Scraping data and selecting columns
salaries_qb <- readHTMLTable(paste0(salaries_base, week - 1, "&ew=", week - 1, "&s=&t=0&p=2&st=FantasyPointsFanDuel&d=1&ls=&live=false&pid=false&minsnaps=4"), as.data.frame = TRUE, stringsasfactors = FALSE, header = FALSE, skip.rows = 1)$StatsGrid

salaries_qb <- select(salaries_qb, V2, V3, V5, V9)

salaries_rb <- readHTMLTable(paste0(salaries_base, week - 1, "&ew=", week - 1, "&s=&t=0&p=3&st=FantasyPointsFanDuel&d=1&ls=&live=false&pid=false&minsnaps=4"), as.data.frame = TRUE, stringsasfactors = FALSE, header = FALSE, skip.rows = 1)$StatsGrid

salaries_rb <- select(salaries_rb, V2, V3, V5, V9)

salaries_wr <- readHTMLTable(paste0(salaries_base, week - 1, "&ew=", week - 1, "&s=&t=0&p=4&st=FantasyPointsFanDuel&d=1&ls=&live=false&pid=false&minsnaps=4"), as.data.frame = TRUE, stringsasfactors = FALSE, header = FALSE, skip.rows = 1)$StatsGrid

salaries_wr <- select(salaries_wr, V2, V3, V5, V9)

salaries_te <- readHTMLTable(paste0(salaries_base, week - 1, "&ew=", week - 1, "&s=&t=0&p=5&st=FantasyPointsFanDuel&d=1&ls=&live=false&pid=false&minsnaps=4"), as.data.frame = TRUE, stringsasfactors = FALSE, header = FALSE, skip.rows = 1)$StatsGrid

salaries_te <- select(salaries_te, V2, V3, V5, V9)

salaries_k <- readHTMLTable(paste0(salaries_base, week - 1, "&ew=", week - 1, "&s=&t=0&p=6&st=FantasyPointsFanDuel&d=1&ls=&live=false&pid=false&minsnaps=4"), as.data.frame = TRUE, stringsasfactors = FALSE, header = FALSE, skip.rows = 1)$StatsGrid

salaries_k <- select(salaries_k, V2, V3, V5, V9)

salaries_dst <- readHTMLTable(paste0(salaries_base, week - 1, "&ew=", week - 1, "&s=&t=0&p=7&st=FantasyPointsFanDuel&d=1&ls=&live=false&pid=false&minsnaps=4"), as.data.frame = TRUE, stringsasfactors = FALSE, header = FALSE, skip.rows = 1)$StatsGrid

salaries_dst <- select(salaries_dst, V2, V3, V5, V9)



# Binding position data frames together
salaries <- rbind(salaries_qb, salaries_rb, salaries_wr, salaries_te, salaries_k, salaries_dst)

# Defining column names
colnames(salaries) <-c("Player", "Position", "Team", "Salary")


# Format names
salaries$first_name <- lapply(strsplit(as.character(salaries$Player), " "), "[", 1)
salaries$last_name <- lapply(strsplit(as.character(salaries$Player), " "), "[", 2)

salaries$first_name <- toupper(salaries$first_name)
salaries$last_name <- toupper(salaries$last_name)

salaries$Player <- paste0(salaries$last_name, salaries$first_name, "")

salaries$Player <- gsub("[[:punct:]]", "", salaries$Player)

salaries <- select(salaries, Player, Position, Team, Salary)

# Save file to csv
write.csv(projections_fantasydata, file = paste0(getwd(), "/fantasy_football/projections_data/salaries_week", week, ".csv", sep = ""))

