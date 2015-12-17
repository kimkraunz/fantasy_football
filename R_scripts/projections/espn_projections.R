### TO DO:
### 1. Convert data types (convert.magic?)
### 2. Calculate FD points
### 3. Figure out V1:V14???
### 4. No defense?

library("XML")
library("stringr")
library("plyr")
library("data.table")

season = 2015
week = 14

# Create urls
espn_base_url <- paste0("http://games.espn.go.com/ffl/tools/projections?&proTeamId=null&scoringPeriodId=", week, "&seasonId=", season)
espn_pos <- list(QB=0, RB=2, WR=4, TE=6, K=17, D=16)
espn_pages <- c("0", "40", "80", "120")
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
        } else if(unique(espn_list[[i]][,pos]) == "DST"){
            setnames(espn_list[[i]], c(d_colnames, "Position"))
        }
    }
}

# creating data frame
projections_espn <- rbindlist(espn_list, use.names = TRUE, fill = TRUE)

#Replace symbols with value of zero
projections_espn[which(Catch_Attempts == "--/--"), Catch_Attempts := "0/0"]
projections_espn[which(PaY == "--"), PaY := "0"]
projections_espn[which(PaTD == "--"), PaTD := "0"]
projections_espn[which(I == "--"), I := "0"]
projections_espn[which(RuAttempts == "--"), RuAttempts := "0"]
projections_espn[which(RuY == "--"), RuY := "0"]
projections_espn[which(RuTD == "--"), RuTD := "0"]
projections_espn[which(Re == "--"), Re := "0"]
projections_espn[which(ReY == "--"), ReY := "0"]
projections_espn[which(ReTD == "--"), ReTD := "0"]
projections_espn[which(espn_fantasy_pts == "--"), espn_fantasy_pts := "0"]

# Delete rows with either 0 projected points or NA
projections_espn <- projections_espn[!is.na(projections_espn$espn_fantasy_pts),]
projections_espn <- projections_espn[projections_espn$espn_fantasy_pts != 0,]

# Strip Team from name
projections_espn$Team <- lapply(strsplit(as.character(projections_espn$Player), ", "), "[", 2)



# Remove Team and Pos from Player
projections_espn$Player <- gsub(",.*", "", projections_espn$Player)

# Remove Pos and Status from Team (space is not simply space)
projections_espn$Team <- lapply(strsplit(as.character(projections_espn$Team), "\\s"), "[", 1)

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

# Remove columns from DF
projections_espn <- subset(projections_espn, , -c(OPP, Time, Catch_Attempts, RuAttempts, first_name, last_name, V1:V14, pos))

# Remove position and status from Team names
projections_espn$Team <- lapply(strsplit(as.character(unlist(projections_espn$Team)), " "), "[", 1)

# Convert lower case to upper case
projections_espn$Team <- toupper(projections_espn$Team)

# Change team name for consistency
projections_espn$Team[projections_espn$Team == "WSH"] = "WAS"

# Change data types- Can I use convert magic?
#projections_espn$Team <- as.factor(projections_espn$Team)
#conver.magic(projections_espn$Team, factor())

# Compute Fanduel projected points

# Add projection source
projections_espn$proj.source <- "espn"

# Save file to csv
write.csv(projections_fp, file = paste0(getwd(), "/fantasy_football/projections_data/espn_projections_week", week, ".csv", sep = ""))