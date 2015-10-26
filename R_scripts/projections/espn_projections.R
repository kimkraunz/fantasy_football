### NOT WORKING


library("XML")
library("stringr")
library("plyr")
library("data.table")

season = 2015
week = 6

# Create urls
espn_base_url <- paste0("http://games.espn.go.com/ffl/tools/projections?&proTeamId=null&scoringPeriodId=", week, "&seasonId=", season, "&startIndex=")
espn_pos <- list(QB=0, RB=2, WR=4, TE=6, K=17, D=16)
espn_pages <- c("0", "40", "80")
espn_urls <- paste0(espn_base_url, "&slotCategoryId=", rep(espn_pos, each=length(espn_pages)), "&startIndex=", espn_pages)

#Scrape - if error then skips
espn <- try(lapply(espn_urls, function(x) {data.table(readHTMLTable(x, as.data.frame = TRUE, stringsasfactors = FALSE)$playertable_0)})); if(class(espn) == "try-error") next;
espn_list <- espn

# defining column names
qb_colnames <- rb_colnames <- wr_colnames <- te_colnames <- k_colnames <- d_colnames <- c("Player", "OPP", "Time", "Catch_Attempts", "PaY", "PaTD", "I", "RuAttempts", "RuY", "RuTD",  "Re", "ReY", "ReTD", "espn_fantasy_pts")

for(i in 1:length(espn_list)) {
    if(nrow(espn_list[[i]]) > 0){
        #Add position to projection
        espn_list[[i]][,pos := rep(names(espn_pos), each=length(espn_pages))[i]]
        espn_list[[i]][,pos := as.factor(pos)]
        
        #Trim dimensions  
        espn_list[[i]] <- espn_list[[i]][2:nrow(espn_list[[i]])]
        
        #Add variable names
        if(unique(espn_list[[i]][,pos]) == "QB"){
            setnames(espn_list[[i]], c(qb_colnames, "Position"))
        } else if(unique(espn_list[[i]][,pos]) == "RB"){
            setnames(espn_list[[i]], c(rb_colnames, "Position"))
        } else if(unique(espn_list[[i]][,pos]) == "WR"){
            setnames(espn_list[[i]], c(wr_colnames, "Position"))
        } else if(unique(espn_list[[i]][,pos]) == "TE"){
            setnames(espn_list[[i]], c(te_colnames, "Position"))
        } else if(unique(espn_list[[i]][,pos]) == "K"){
            setnames(espn_list[[i]], c(k_colnames, "Position"))
        } else if(unique(espn_list[[i]][,pos]) == "D"){
            setnames(espn_list[[i]], c(d_colnames, "Position"))
        }
    }
}

# creating data frame
projections_espn <- rbindlist(espn_list, use.names = TRUE, fill = TRUE)

#Replace symbols with value of zero
projections_espn[which(Catch_Attempts == "--/--"), Catch_Attempts := "0/0"]
projections_espn[which(pass_yards == "--"), pass_yards := "0"]
projections_espn[which(pass_TDs == "--"), pass_TDs := "0"]
projections_espn[which(int == "--"), int := "0"]
projections_espn[which(rushs == "--"), rushs := "0"]
projections_espn[which(rush_yards == "--"), rush_yards := "0"]
projections_espn[which(rush_TDS == "--"), rush_TDs := "0"]
projections_espn[which(receptions == "--"), receptions := "0"]
projections_espn[which(recept_yds == "--"), recept_yds := "0"]
projections_espn[which(recept_tds == "--"), recept_tds := "0"]
projections_espn[which(pts == "--"), pts := "0"]

# Delete rows with either 0 projected points or NA
projections_espn <- projections_espn[!is.na(projections_espn$pts),]
projections_espn <- projections_espn[projections_espn$pts != 0,]

#Separate pass completions from attempts
projections_espn$attempts <- gsub("\\/.*", "", projections_espn$Catch_Attempts)

# Strip Team from name
projections_espn$Team <- lapply(strsplit(as.character(projections_espn$Player), ", "), "[", 2)

# Remove POS and status from Team
#projections_espn$Team <- lapply(strsplit(as.character(projections_espn$Team), " "), "[", 1)
#projections_espn$Team <- gsub(" ", "", projections_espn$Team, perl = TRUE)

# Remove Team and Pos from Player
projections_espn$Player <- gsub(",.*", "", projections_espn$Player)

# Remove "D/DST" from Defensive Players
projections_espn$Player <- str_replace_all(projections_espn$Player, fixed("D/ST"), "")

projections_espn$first_name <- lapply(strsplit(as.character(projections_espn$Player), " "), "[", 1)
projections_espn$last_name <- lapply(strsplit(as.character(projections_espn$Player), " "), "[", 2)

projections_espn$first_name <- toupper(projections_espn$first_name)
projections_espn$last_name <- toupper(projections_espn$last_name)

projections_espn$Player <- paste0(projections_espn$last_name, projections_espn$first_name, "")

# Remove extra leading whitespace from Player (defense teams)
projections_espn$Player <- str_trim(projections_espn$Player, "left")

# Remove special characters from player names
projections_espn$Player <- gsub("[[:punct:]]", "", projections_espn$Player)
