# Ready to calculate fantasy points and import salaries

#Load libraries
library("XML")
library("stringr")
library("dplyr")

#Functions
source(paste(getwd(),"/fantasy_football/R_scripts/Functions.R", sep=""))

week <- 7

#Download fantasy football projections from FFtoday.com
qb_fftoday <- readHTMLTable(paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=2015&GameWeek=", week, "&PosID=10&LeagueID=174798", sep = ""), stringsAsFactors = FALSE)[11]$'NULL'
rb1_fftoday <- readHTMLTable(paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=2015&GameWeek=", week, "&PosID=20&LeagueID=174798&order_by=FFPts&sort_order=DESC&cur_page=0", sep = ""), stringsAsFactors = FALSE)[11]$'NULL'
rb2_fftoday <- readHTMLTable(paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=2015&GameWeek=", week, "&PosID=20&LeagueID=174798&order_by=FFPts&sort_order=DESC&cur_page=1", sep = ""), stringsAsFactors = FALSE)[11]$'NULL'
wr1_fftoday <- readHTMLTable(paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=2015&GameWeek=", week, "&PosID=30&LeagueID=174798", sep = ""), stringsAsFactors = FALSE)[11]$'NULL'

wr2_fftoday <- readHTMLTable(paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=2015&GameWeek=", week, "&PosID=30&LeagueID=174798&order_by=FFPts&sort_order=DESC&cur_page=2", sep = ""), stringsAsFactors = FALSE)[11]$'NULL'

te_fftoday <- readHTMLTable(paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=2015&GameWeek=", week, "&PosID=40&LeagueID=174798", sep = ""), stringsAsFactors = FALSE)[11]$'NULL'

k_fftoday <- readHTMLTable(paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=2015&GameWeek=", week, "&PosID=80&LeagueID=174798", sep = ""), stringsAsFactors = FALSE)[11]$'NULL'

#Add variable names for each object
names(qb_fftoday) <- c("star","Player","Team","OPP","passComp","passAtt","PaY","PaTD","I","rushAtt","RuY","RuTD","Points")
names(rb1_fftoday) <- names(rb2_fftoday) <- c("star","Player","Team","OPP","rushAtt","RuY","RuTD","Re","ReY","ReTD","Points")
names(wr1_fftoday) <- names(wr2_fftoday) <- names(te_fftoday) <- c("star","Player","Team","OPP","Re","ReY","ReTD","Points")
names(k_fftoday) <- c("star","Player","Team","OPP","FGu20","fgMiss","PATP","xpMiss", "Points")

#Trim dimensions
qb_fftoday <- qb_fftoday[2:(dim(qb_fftoday)[1]-1),]
rb1_fftoday <- rb1_fftoday[2:(dim(rb1_fftoday)[1]-1),]
rb2_fftoday <- rb2_fftoday[2:(dim(rb2_fftoday)[1]-1),]
wr1_fftoday <- wr1_fftoday[2:(dim(wr1_fftoday)[1]-1),]
wr2_fftoday <- wr2_fftoday[2:(dim(wr2_fftoday)[1]-1),]
te_fftoday <- te_fftoday[2:(dim(te_fftoday)[1]-1),]
k_fftoday <- k_fftoday[2:(dim(k_fftoday)[1]-1),]

#Merge within position

rb_fftoday <- rbind(rb1_fftoday,rb2_fftoday)
wr_fftoday <- rbind(wr1_fftoday,wr2_fftoday)


#Add variable for player position
qb_fftoday$Pos <- as.factor("QB")
rb_fftoday$Pos <- as.factor("RB")
wr_fftoday$Pos <- as.factor("WR")
te_fftoday$Pos <- as.factor("TE")
k_fftoday$Pos <- as.factor("K")

#Merge across positions
projections_fftoday <- rbind.fill(qb_fftoday, rb_fftoday, wr_fftoday, te_fftoday, k_fftoday)

projections_fftoday <- select(projections_fftoday, Player, Team, PaY, PaTD, I, RuY, RuTD, Points, Pos, Re, ReY, ReTD, FGu20, PATP)

#Convert variables from character strings to numeric
projections_fftoday[,c("PaY", "PaTD", "I", "RuY", "RuTD", "Points", "Re", "ReY", "ReTD", "FGu20", "PATP")] <- 
    convert.magic(projections_fftoday[,c("PaY", "PaTD", "I", "RuY", "RuTD", "Points", "Re", "ReY", "ReTD", "FGu20", "PATP")], "numeric")

#Player name, position, and team
projections_fftoday$Player <- str_trim(str_sub(projections_fftoday$Player, start=2))

# Clean up names
projections_fftoday <- convert.names(projections_fftoday)





#Save file
#save(projections_fftoday, file = paste(getwd(), "/Data/FFtoday-Projections.RData", sep=""))
#write.csv(projections_fftoday, file=paste(getwd(), "/Data/FFtoday-Projections.csv", sep=""), row.names=FALSE)

#save(projections_fftoday, file = paste(getwd(), "/Data/Historical Projections/FFtoday-Projections-", season, ".RData", sep=""))
#write.csv(projections_fftoday, file=paste(getwd(), "/Data/Historical Projections/FFtoday-Projections-", season, ".csv", sep=""), row.names=FALSE)