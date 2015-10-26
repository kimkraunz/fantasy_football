# WORKS BUT NEED TO FIX NAMES IF GOING TO USE IN BAYESIAN ANALYSIS
# Also need to remove optimizer at end

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
dst_fp <- mutate(dst_fp, pos = "D")

# Merge position tables into one data frame
df_fp <- rbind_list(qb_fp, wr_fp, rb_fp, te_fp, k_fp, dst_fp)

# Keep only Player, Proj.Points, Salary, and pos columns
df_fp <- select(df_fp, Player, Proj.Points, Salary, pos)

# Clean up names
df_fp$Player <- str_replace(df_fp$Player, " \\(.*", "")
df_fp$first_name <- lapply(strsplit(as.character(df_fp$Player), " "), "[", 1)
df_fp$last_name <- lapply(strsplit(as.character(df_fp$Player), " "), "[", 2)

df_fp$first_name <- toupper(df_fp$first_name)
df_fp$last_name <- toupper(df_fp$last_name)

df_fp$Player <- paste0(df_fp$last_name, df_fp$first_name, sep = "")


#Remove special characters
df_fp$Salary <- gsub("[[:punct:]]", "", df_fp$Salary)
df_fp$Proj.Points <- gsub(" pts", "", df_fp$Proj.Points)
df_fp$Player <- gsub("[[:punct:]]", "", df_fp$Player)

df_fp$pos <- as.factor(df_fp$pos)
df_fp$Proj.Points <- as.numeric(df_fp$Proj.Points)
df_fp$Salary <- as.numeric(df_fp$Salary)

df_fp <- select(df_fp, Player, Proj.Points, Salary, pos)
colnames(df_fp) <- c("Player", "Points", "Cost", "Position")

# write projected points to csv
write.csv(df_fp, file = paste0(getwd(), "/fantasy_football/projections_data/fp_projections_", Sys.Date(), ".csv", sep = ""))

df_fp <- df_fp[df_fp$Player != "New York Jets (NYJ - DST)", ]
df_fp <- df_fp[1:317, ]

df_fp <- df_fp[df_fp$Player != "Allen Robinson (JAC - WR)", ]

Opt.squad<-function(dat,cost,n.d=1,n.k=1,n.qb=1,n.rb=2, n.wr=3, n.te=1) {
    
    ##Create dummy variables
    fact.only<-df_fp[, which(sapply(df_fp, class)=="factor")]
    non.fact<-df_fp[, which(sapply(df_fp, class)!="factor")]
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

Opt.squad(df_fp, 60000)

df <- read.csv("/Users/kimkraunz/Documents/fantasy_football/fp_projections_09242015.csv", header = TRUE)
Opt.squad(df, 60000)
