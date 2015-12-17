# TO DO: scrape each page - only have QB right now
# #     figure out multiplier
#       standardize feature names
#       standardize names

library("XML")
library("stringr")
library("dplyr")

week <- 15
season <- 2015

# Create offense urls
nfl_base_url <- paste0("http://fantasy.nfl.com/research/projections?offset=")
nfl_offset <- c("0", "26", "51", "76", "101", "126", "151", "176", "201", "226", "251", "276", "301", "326", "351")
nfl_urls <- paste0(nfl_base_url, nfl_offset, "&position=0&statCategory=projectedStats&statSeason=", season, "&statType=weekProjectedStats&statWeek=", week)


nfl_off_colClasses <- c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
nfl_off <- try(lapply(nfl_urls, function(x) {data.table(readHTMLTable(x, as.data.frame = TRUE, stringsasfactors = FALSE, colClasses = nfl_off_colClasses)$'NULL')})); if(class(nfl) == "try-error") next;
nfl_off_list <- nfl_off

projections_nfl_off <- rbindlist(nfl_off_list, use.names = TRUE, fill = TRUE)
colnames(projections_nfl_off) <- c("Player", "Opp", "PaY", "PaTD", "I", "RuY", "RuTD", "ReY", "ReTD", "FUTD", "PCP2", "FUL", "nfl.projected.points")

# Replace NAs with 0's
miss <- is.na(projections_nfl_off)
projections_nfl_off[miss] <- 0

# Calculate Fanduel projected points
projections_nfl_off <- mutate(projections_nfl_off, projected.points = PaY*pts_perPaY +  PaTD* pts_perPaTD + I*pts_perI +  RuY*pts_perRuY + RuTD*pts_perRuTD + ReY*pts_perReY + ReTD*pts_perReTD + FUTD*pts_perFUTD + PCP2*pts_per2PCP + FUL*pts_perFUL)

# Change projected points to 0 if on IR
projections_nfl_off$projected.points[projections_nfl_off$nfl.projected.points == 0] = 0

# Grab data rows
projections_nfl_off <- select(projections_nfl_off, Player, projected.points)

# Scrape Kicker data
k_colClasses <- c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")

nfl_k1 <- readHTMLTable(paste0("http://fantasy.nfl.com/research/projections?position=7&statCategory=projectedStats&statSeason=", season, "&statType=weekProjectedStats&statWeek=", week), stringsasfactors = FALSE, colClasses = k_colClasses)$'NULL'

nfl_k2 <- readHTMLTable(paste0("http://fantasy.nfl.com/research/projections?offset=26&position=7&statCategory=projectedStats&statSeason=", season, "&statType=weekProjectedStats&statWeek=", week), stringsasfactors = FALSE, colClasses = k_colClasses)$'NULL'
projections_nfl_k <- rbind(nfl_k1, nfl_k2)
colnames(projections_nfl_k) <- c("Player", "Opp", "PATP", "FGu20", "FGu30", "FGu40", "FGu50", "FGo50", "nfl.projected.points")

# Replace NAs with 0's
miss <- is.na(projections_nfl_k)
projections_nfl_k[miss] <- 0

# Calculate fanduel projected points
projections_nfl_k <- mutate(projections_nfl_k, projected.points = PATP * pts_perPATP + FGu20 * pts_perFGu20 + FGu30 * pts_perFGu30 + FGu40 * pts_perFGu40 + FGu50 * pts_perFGu50 + FGo50 * pts_perFGo50)

# Change projected points to 0 if on IR
projections_nfl_k$projected.points[projections_nfl_k$nfl.projected.points == 0] = 0

# Plot nfl projections vs fanduel projections
#plot(projections_nfl_k$nfl.projected.points, projections_nfl_k$projected.points, xlab="NFL projected points", ylab="Fanduel Projected Points", pch=2, cex.main=2, frame.plot=FALSE , col="blue")

# Grab kicker data
projections_nfl_k <- select(projections_nfl_k, Player, projected.points)

# Scrape DST data
dst_colClasses <- c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")

nfl_dst1 <- readHTMLTable(paste0("http://fantasy.nfl.com/research/projections?position=8&statCategory=projectedStats&statSeason=", season, "&statType=weekProjectedStats&statWeek=", week), stringsasfactors = FALSE, colClasses = k_colClasses)$'NULL'

nfl_dst2 <- readHTMLTable(paste0("http://fantasy.nfl.com/research/projections?offset=26&position=8&statCategory=projectedStats&statSeason=", season, "&statType=weekProjectedStats&statWeek=", week), stringsasfactors = FALSE, colClasses = k_colClasses)$'NULL'
projections_nfl_dst <- rbind(nfl_dst1, nfl_dst2)
colnames(projections_nfl_dst) <- c("Player", "Opp", "DES", "DEI", "DEFR", "DESF", "DEFRTD", "DERTD", "PA", "nfl.projected.points")

# Replace NAs with 0's
miss <- is.na(projections_nfl_dst)
projections_nfl_dst[miss] <- 0

# Calculate Fanduel Points for points allowed using nested ifelse
projections_nfl_dst$PA.points <-ifelse(projections_nfl_dst$PA == 0, 10, ifelse (projections_nfl_dst$PA < 7, 7, ifelse(projections_nfl_dst$PA < 14, 4, ifelse (projections_nfl_dst$PA < 21, 1, ifelse(projections_nfl_dst$PA < 28, 0, ifelse(projections_nfl_dst$PA < 35, -1, -4))))))

# Calculate fanduel projected points
projections_nfl_dst <- mutate(projections_nfl_dst, projected.points =  DES*pts_perDES +  DEI*pts_perDEI +  DEFR*pts_perDEFR + DESF*pts_perDESF + DEFRTD*pts_perDEFRTD + DERTD*pts_perDERTD + PA.points)

# Change projected points to 0 if on IR
projections_nfldst$projected.points[projections_nfl_dst$nfl.projected.points == 0] = 0

# Plot nfl projections vs fanduel projections
#plot(projections_nfl_k$nfl.projected.points, projections_nfl_k$projected.points, xlab="NFL projected points", ylab="Fanduel Projected Points", pch=2, cex.main=2, frame.plot=FALSE , col="blue")

# Grab dst data
projections_nfl_dst <- select(projections_nfl_dst, Player, projected.points)

# Combine offense, kicker, and dst data
projections_nfl <- rbind(projections_nfl_off, projections_nfl_k, projections_nfl_dst)

# Delete if projected points equals zero
projections_nfl <- projections_nfl[projections_nfl$projected.points !=0, ]


# Extract position and team

projections_nfl$Player <- gsub(" View Videos", "", projections_nfl$Player)
projections_nfl$Player <- gsub(" View Video", "", projections_nfl$Player)
projections_nfl$Player <- gsub(" View News", "", projections_nfl$Player)


projections_nfl$Team <- lapply(strsplit(as.character(projections_nfl$Player), "-"), "[", 2)
projections_nfl$Team <- str_trim(as.character(projections_nfl$Team), side = "left")
projections_nfl$Team <- gsub(' [A-z ]*', '' , projections_nfl$Team)


projections_nfl$Player <- gsub(' \\- .*$', '', projections_nfl$Player)
projections_nfl$first_name <- lapply(strsplit(as.character(projections_nfl$Player), " "), "[", 1)
projections_nfl$last_name <- lapply(strsplit(as.character(projections_nfl$Player), " "), "[", 2)
projections_nfl$Position <- lapply(strsplit(as.character(projections_nfl$Player), " "), "[", 3)

projections_nfl$first_name <- toupper(projections_nfl$first_name)
projections_nfl$last_name <- toupper(projections_nfl$last_name)
projections_nfl$Player <- paste(projections_nfl$last_name, projections_nfl$first_name, sep = "")

projections_nfl$Player <- gsub("[[:punct:]]", '', projections_nfl$Player)

positions <- c("QB", "TE", "WR", "RB", "K", "DST")

# Correct Team positions
projections_nfl$Position[!(projections_nfl$Position %in% positions)] = "DST"

# Add projection source
projections_nfl$proj.source <- "nfl"

# Select Player, Position, Team, projected.points
projections_nfl <- select(projections_nfl, Player, Position, Team, projected.points, proj.source)
