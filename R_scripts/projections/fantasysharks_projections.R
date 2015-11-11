

#Load libraries
library(caret)
library(lpSolve)

# Week
week <- 5

#Download fantasy football projections from FantasySharks.com
#Download offense(minus kicker)
projections_fs_of <- read.csv(paste("http://www.fantasysharks.com/apps/bert/forecasts/projections.php?csv=1&Sort=&Segment=53", week + 1, "&Position=97&scoring=17&League=-1&uid=4&uid2=&printable=", sep = ""), stringsAsFactors = FALSE)

#Download kicker
projections_fs_k <- read.csv(paste("http://www.fantasysharks.com/apps/bert/forecasts/projections.php?csv=1&Sort=&Segment=53", week + 1, "&Position=7&scoring=17&League=-1&uid=4&uid2=&printable=", sep = ""), stringsAsFactors = FALSE)

#Download defense
projections_fs_def <- read.csv(paste("http://www.fantasysharks.com/apps/bert/forecasts/projections.php?csv=1&Sort=&Segment=53", week + 1, "&Position=6&scoring=17&League=-1&uid=4&uid2=&printable=", sep = ""), stringsAsFactors = FALSE)

#Read salaries
setwd("/Users/kimkraunz/Documents/fantasy_football")
salaries <- read.csv("Fanduel-NFL-2015-10-11-13183-players-list.csv", stringsAsFactors = FALSE)

# Keep Player, Team, Position, Pts
keep <- c("Player", "Team", "Position", "Pts")
projections_fs_of <- projections_fs_of[, keep]
projections_fs_k <- projections_fs_k[, keep]
projections_fs_def <- projections_fs_def[, keep]
projections_fs <- rbind(projections_fs_k, projections_fs_of, projections_fs_def)

# Standardize team names
if(length(projections_fs[projections_fs$Team == "SFO", "Team"]) > 0){projections_fs[projections_fs$Team == "SFO", "Team"] <- "SF"}
projections_fs$Team<- as.character(projections_fs$Team)
projections_fs$Team[projections_fs$Team == "NEP"] <- "NE"
projections_fs$Team[projections_fs$Team == "GBP"] <- "GB"
projections_fs$Team[projections_fs$Team == "KCC"] <- "KC"
projections_fs$Team[projections_fs$Team == "NOR"] <- "NO"
projections_fs$Team[projections_fs$Team == "SDC"] <- "SD"
projections_fs$Team[projections_fs$Team == "TBB"] <- "TB"
projections_fs$Team[projections_fs$Team == "ARZ"] <- "ARI"



# Convert Team and Position to factors
projections_fs$Position <- as.factor(projections_fs$Position)
projections_fs$Team <- as.factor(projections_fs$Team)
levels_fs_team <- levels(projections_fs$Team)

# Clean up names in projection df
projections_fs$first_name <- lapply(strsplit(as.character(projections_fs$Player), ","), "[", 2)
trim.leading <- function (x)  sub("^\\s+", "", x)
projections_fs$first_name <- trim.leading(projections_fs$first_name)

projections_fs$last_name <- lapply(strsplit(as.character(projections_fs$Player), ","), "[", 1)

# Capitalize names
projections_fs$first_name <- toupper(projections_fs$first_name)
projections_fs$last_name <- toupper(projections_fs$last_name)

# merge names back together
projections_fs$Name <- paste(projections_fs$last_name, projections_fs$first_name, sep = "")

keep <- c("Name", "Team", "Position", "Pts")
projections_fs <- projections_fs[, keep]

# clean up salaries name
salaries$First.Name <- toupper(salaries$First.Name)
salaries$Last.Name <- toupper(salaries$Last.Name)
salaries$Name <- paste(salaries$Last.Name, salaries$First.Name, sep = "")

keep <- c("Name",  "Team", "Position", "Salary")
salaries <- salaries[, keep]

# Standardize player names
projections_fs$Name[projections_fs$Name == "BECKHAMODELL"] <- "BECKHAM JR.ODELL"
projections_fs$Name[projections_fs$Name == "IVORYCHRIS"] <- "IVORYCHRISTOPHER"
projections_fs$Name[projections_fs$Name == "BROWNPHILLY"] <- "BROWNCOREY (PHILLY)"
projections_fs$Name[projections_fs$Name == "KEARSE JERMAINE"] <- "KEARSEJERMAINE"
projections_fs$Name[projections_fs$Name == "MCCOWNJOSHUA"] <- "MCCOWNJOSH"
projections_fs$Name[projections_fs$Name == "WHITTAKERFOZZY"] <- "WHITTAKERFOSWHITT"
projections_fs$Name[projections_fs$Name == "HOUSLERROBERT"] <- "HOUSLERROB"
projections_fs$Name[projections_fs$Name == "HOUSLERROBERT"] <- "HOUSLERROB"
projections_fs$Name[projections_fs$Name == "RAMSST. LOUIS"] <- "RAMSST LOUIS"

# convert team and positions to factors
salaries$Team <- as.factor(salaries$Team)
salaries$Position <- as.factor(salaries$Position)

levels_salaries_Team <- levels(salaries$Team)

# Check that factors for team are the same in projections and salaries DFs
intersect(levels_salaries_Team, levels_fs_team)
setdiff(levels_salaries_Team, levels_fs_team)

# merge projections with salaries
projections <- merge(projections_fs, salaries, by= c("Name", "Team"))
keep <- c("Name", "Team", "Pts", "Salary", "Position.x")
projections <- projections[, keep]
colnames(projections) <- c("Player", "Team", "Points", "Cost", "Position")


#check that all data included for salaries and projections
setdiff(salaries$Name, projections$Player)
setdiff(projections$Name, salaries$Name)
setdiff(projections_fs$Name, projections$Player)

# convert to numeric
projections$Points <- as.numeric(projections$Points)
projections$Cost <- as.numeric(projections$Cost)
proj <- as.matrix(projections)

# write projections to csv
write.csv(projections, file = paste(getwd(), "/fantasy_football/projections_10082015.csv", sep = ""))

Opt.squad<-function(dat,cost,n.d=1,n.k=1,n.qb=1,n.rb=2, n.wr=3, n.te=1) {
    
    ##Create dummy variables
    fact.only<-projections[, which(sapply(projections, class)=="factor")]
    non.fact<-projections[, which(sapply(projections, class)!="factor")]
    dat.dummy<-model.matrix(~ Position, data=fact.only, contrasts.arg=list(Position=contrasts(fact.only$Position, contrasts=F)))
                            
    
    dat2<-dat.dummy[,-1]
    colnames(dat2)<-gsub("[^#A-Za-z0-9_]","",colnames(dat2))
    dat<-cbind(non.fact,dat2)
    ##Pick optimum team
    f.obj <- dat$Points
    nn<-length(f.obj)
    num_play  <- rep(1,nn)
    n.choose<-n.d+n.k+n.qb+n.rb+n.wr+n.te
    f.con <- cbind(Cost=dat$Cost,dat[,colnames(dat) %in% colnames(dat2)],num_play)
    f.con<-t(f.con)
    f.dir <- c("<=","=","=","=","=","=", "=", "=")
    f.rhs <- c(cost,n.d,n.k,n.qb,n.rb,n.te,n.wr,n.choose)

    out1  <- lp("max", f.obj, f.con, f.dir, f.rhs,all.bin=TRUE)
    
    if (out1$status == 2) {
        print ("There was no optimal solution. Try a different approach or increase/decrease your total cost")
    } else {
        sol1  <- out1$solution
        total1 <- out1$objval 
        ###Take numbers and take out 
        pick1a <- rep(0,15)
        k     <- 0
        f.obj2<-f.obj
        for (j in 1:nn) {
            if (sol1[j]==1) {
                k        <- k+1
                pick1a[k] <- j
            }
        }
        final.pick<-dat[pick1a, 1:3]
        print(total1)
        final.pick
    }
}

Opt.squad(proj, 60000)
