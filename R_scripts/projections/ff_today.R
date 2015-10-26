library("dplyr")
library("XML")
library("stringr")

week <- 2

qb_fftoday <- readHTMLTable(paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=2015&GameWeek=", week, "&PosID=10&LeagueID=174798", sep = ""), stringsasfactors = FALSE, header = TRUE, skip.rows = 1)[11]$"NULL"
rb1_fftoday <- readHTMLTable(paste0("http://www.fftoday.com/rankings/playerwkproj.php?Season=2015&GameWeek=", week, "&PosID=20&LeagueID=174798", sep = ""), stringsasfactors = FALSE, header = TRUE, skip.rows = 1)[11]$"NULL"
rb2_fftoday <- readHTMLTable(paste0("http://www.fftoday.com/rankings/playerwkproj.php?Season=2015&GameWeek=", week, "&PosID=20&LeagueID=174798&order_by=FFPts&sort_order=DESC&cur_page=1", sep = ""), stringsasfactors = FALSE, header = TRUE, skip.rows = 1)[11]$"NULL"
wr1_fftoday <- readHTMLTable(paste0("http://www.fftoday.com/rankings/playerwkproj.php?Season=2015&GameWeek=", week, "&PosID=30&LeagueID=174798", sep = ""), stringsasfactors = FALSE, header = TRUE, skip.rows = 1)[11]$"NULL"
wr2_fftoday <- readHTMLTable(paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=2015&GameWeek=", week, "&PosID=30&LeagueID=174798&order_by=FFPts&sort_order=DESC&cur_page=1", sep = ""), stringsasfactors = FALSE, header = TRUE, skip.rows = 1)[11]$"NULL"
wr3_fftoday <- readHTMLTable(paste0("http://www.fftoday.com/rankings/playerwkproj.php?Season=2015&GameWeek=", week, "&PosID=30&LeagueID=174798&order_by=FFPts&sort_order=DESC&cur_page=2", sep = ""), stringsasfactors = FALSE, header = TRUE, skip.rows = 1)[11]$"NULL"
te_fftoday <- readHTMLTable(paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=2015&GameWeek=", week, "&PosID=40&LeagueID=174798", sep = ""), stringsasfactors = FALSE, header = TRUE, skip.rows = 1)[11]$"NULL"
k_fftoday <- readHTMLTable(paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=2015&GameWeek=", week, "&PosID=80&LeagueID=174798", sep = ""), stringsasfactors = FALSE, header = TRUE, skip.rows = 1)[11]$"NULL"

# create single rb and wr data frames
rb_fftoday <- rbind(rb1_fftoday, rb2_fftoday)
wr_fftoday <- rbind(wr1_fftoday, wr2_fftoday, wr3_fftoday)

# Column names for qb data frame
colnames(qb_fftoday) <- c("chg", "player", "team", "opp", "pass_compl", "pass_att", "pass_yards", "pass_TD", "INT", "rush_att", "rush_yards", "rush_TD", "fd_points")

# Column names for  rb data frame
colnames(rb_fftoday) <- c("chg", "player", "team", "opp", "rush_att", "rush_yards", "rush_TD", "pass_rec", "pass_yards", "pass_TD", "fd_points")

# Column names for  wr data frame
colnames(rb_fftoday) <- c("chg", "player", "team", "opp", "pass_rec", "pass_yards", "pass_TD", "fd_points")

# Column names for  wr data frame
colnames(rb_fftoday) <- c("chg", "player", "team", "opp", "pass_rec", "pass_yards", "pass_TD", "fd_points")

colnames(k_fftoday) <- c("chg", "player", "team", "opp", "fg_made", "fg_miss", "xp_made", "xp_miss", "fd_points")

# Remove columns not needed for FD
k_fftoday <- select(k_fftoday, -fg_miss, -xp_miss)
fftoday <- rbind(qb_fftoday, wr_fftoday, rb_fftoday, te_fftoday, k_fftoday)

# add columns to data frames
dfs <- c("qb_fftoday", "rb_fftoday", "wr_fftoday", "te_fftoday", "k_fftoday")
for 

# Format names
qb_fftoday <- select(qb_fftoday, -chg)
qb_fftoday$player <- substring(qb_fftoday$player, 3)

qb_fftoday$first_name <- lapply(strsplit(as.character(qb_fftoday$player), " "), "[", 1)
trim.leading <- function (x)  sub("^\\s+", "", x)
qb_fftoday$first_name <- trim.leading(qb_fftoday$first_name)

qb_fftoday$last_name <- lapply(strsplit(as.character(qb_fftoday$player), " "), "[", 2)
qb_fftoday$first_name <- toupper(qb_fftoday$first_name)
qb_fftoday$last_name <- toupper(qb_fftoday$last_name)
qb_fftoday$player <- paste(qb_fftoday$last_name, qb_fftoday$first_name, sep = "")

qb_fftoday <- select(qb_fftoday, -first_name, -last_name)
qb_fftoday$opp <- str_replace(qb_fftoday$opp, "@", "")
