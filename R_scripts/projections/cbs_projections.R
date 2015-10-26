### NOT WORKING


#Load libraries
library("XML")
library("stringr")
library("ggplot2")
library("plyr")
library("data.table")

#Projection Info
suffix <- "cbs"
week <- 6

#Download fantasy football projections from cbssports.com
cbs_baseurl <- "http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/"
cbs_pos <- c("QB","RB","WR","TE","K","DST")

cbs_urls <- paste0(cbs_baseurl, cbs_pos, "/season/", rep(cbs_writers, each=length(cbs_pos)), "/", cbs_leaguetype, "?&print_rows=9999")


#Scrape
cbs <- lapply(cbs_urls, function(x) {data.table(readHTMLTable(x, stringsAsFactors = FALSE)[7]$'NULL')})
cbsList <- cbs

#Clean data
qbNames <- c("player","passAtt","passComp","passYds","passTds","passInt","passCompPct","passYdsPerAtt","rushAtt","rushYds","rushYdsPerAtt","rushTds","fumbles","points")
rbNames <- c("player","rushAtt","rushYds","rushYdsPerAtt","rushTds","rec","recYds","recYdsPerRec","recTds","fumbles","points")
wrNames <- c("player","rec","recYds","recYdsPerRec","recTds","fumbles","points")
teNames <- c("player","rec","recYds","recYdsPerRec","recTds","fumbles","points")
kNames <- c("player","fg","fgAtt","xp","points")
dstNames <- c("player","dstInt","dstFumlRec","dstFumlForce","dstSack","dstTd","dstSafety","dstPtsAllowed","dstYdsAllowed","points")

for(i in 1:length(cbsList)){
  #Add writer's name to projection
  cbsList[[i]][,sourceName := rep(cbs_source, each=length(cbs_pos))[i]]
  
  #Add position to projection
  cbsList[[i]][,pos := rep(cbs_pos, length(cbs_source))[i]]
  cbsList[[i]][,pos := as.factor(pos)]
  
  #Trim dimensions  
  if(unique(cbsList[[i]][,pos]) %in% c("K","DST")){
    cbsList[[i]] <- cbsList[[i]][2:(nrow(cbsList[[i]])-1)]
  } else if(unique(cbsList[[i]][,pos]) %in% c("QB")){
    cbsList[[i]] <- cbsList[[i]][3:(nrow(cbsList[[i]]))]
  } else{
    cbsList[[i]] <- cbsList[[i]][3:(nrow(cbsList[[i]])-1)]
  }
  
  #Add variable names
  if(unique(cbsList[[i]][,pos]) == "QB"){
    setnames(cbsList[[i]], c(qbNames, "sourceName", "pos"))
  } else if(unique(cbsList[[i]][,pos]) == "RB"){
    setnames(cbsList[[i]], c(rbNames, "sourceName", "pos"))
  } else if(unique(cbsList[[i]][,pos]) == "WR"){
    setnames(cbsList[[i]], c(wrNames, "sourceName", "pos"))
  } else if(unique(cbsList[[i]][,pos]) == "TE"){
    setnames(cbsList[[i]], c(teNames, "sourceName", "pos"))
  } else if(unique(cbsList[[i]][,pos]) == "K"){
    setnames(cbsList[[i]], c(kNames, "sourceName", "pos"))
  } else if(unique(cbsList[[i]][,pos]) == "DST"){
    setnames(cbsList[[i]], c(dstNames, "sourceName", "pos"))
  }
}

#Merge
projections_cbs <- rbindlist(cbsList, use.names=TRUE, fill=TRUE)

#Convert variables from character strings to numeric
numericVars <- names(projections_cbs)[names(projections_cbs) %in% scoreCategories]
projections_cbs[, (numericVars) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = numericVars]

#Player teams
projections_cbs[,team_cbs := cleanTeamAbbreviations(str_trim(str_sub(player, start = -3)))]

#Player names
projections_cbs[,name_cbs := str_sub(player, end=str_locate(string=player, ',')[,1]-1)]
projections_cbs[which(pos == "DST"), name_cbs := convertTeamName(projections_cbs$team_cbs[which(projections_cbs$pos == "DST")])]
projections_cbs[,name := nameMerge(name_cbs)]

#Remove Duplicate Cases
projections_cbs1 <- projections_cbs[which(sourceName == "cbs1")]
projections_cbs2 <- projections_cbs[which(sourceName == "cbs2")]

duplicateCases_cbs1 <- projections_cbs1[duplicated(name)]$name
duplicateCases_cbs2 <- projections_cbs2[duplicated(name)]$name

projections_cbs1[which(name %in% duplicateCases_cbs1),]
projections_cbs2[which(name %in% duplicateCases_cbs2),]

#Rename Players
projections_cbs[name=="TIMOTHYWRIGHT", name:="TIMWRIGHT"]

#Calculate Overall Rank
projections_cbs <- projections_cbs[order(-points)][,overallRank := 1:.N, by=list(sourceName)]

#Calculate Position Rank
projections_cbs <- projections_cbs[order(-points)][,positionRank := 1:.N, by=list(sourceName, pos)]

#Order variables in data set
allVars <- c(prefix, paste(sourceSpecific, suffix, sep="_"), varNames)
keepVars <- allVars[allVars %in% names(projections_cbs)]
projections_cbs <- projections_cbs[,keepVars, with=FALSE]

#Order players by overall rank
projections_cbs <- projections_cbs[order(projections_cbs$overallRank),]

#Density Plot
ggplot(projections_cbs, aes(x=points)) + geom_density(fill="red", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of CBS Projected Points")
ggsave(paste(getwd(),"/FantasyFootballAnalyticsR/Figures/CBS projections.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections_cbs, file = paste0(getwd(),"/FantasyFootballAnalyticsR/Data/CBS-Projections.RData"))
write.csv(projections_cbs, file = paste0(getwd(),"/FantasyFootballAnalyticsR/Data/CBS-Projections.csv"), row.names=FALSE)

# historical save not working
save(projections_cbs, file = paste0(getwd(),"/FantasyFootballAnalyticsR/Data/Historical Projections/CBS-Projections-", season, "week", week, ".RData"))
write.csv(projections_cbs, file = paste0(getwd(),"/FantasyFootballAnalyticR/Data/Historical Projections/CBS-Projections-", season, "week", week, ".csv"), row.names=FALSE)
