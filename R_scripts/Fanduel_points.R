Fanduel_points <- function(df) {
    source(paste(getwd(),"/fantasy_football/R_scripts/league_points.R", sep=""))
    df$pts_RuY = pts_perRuY * df$RuY          #Rushing Yard points
    df$pts_RuTD = pts_perRuTD * df$RuTD       #Rushing Touchdowns
    df$pts_PaY = pts_perPaY * df$PaY          #Passing Yards 
    df$pts_PaTD = pts_perPaTD * df$PaTD       #Passing Touchdowns 
    df$pts_I = pts_perI * df$I                #Interceptions
    df$pts_ReY = pts_perReY * df$ReY          #Receiving Yards 
    df$pts_ReTD = pts_perReTD * df$ReTD       #Receiving Touchdowns
    df$pts_Re = pts_perRe * df$Re             #Receptions
    df$pts_KRTD = pts_perKRTD * df$KRTD       #Kick-Return Touchdowns
    df$pts_PRTD = pts_perPRTD * df$PRTD       #Punt-Return Touchdowns
    df$pts_FUL = pts_perFUL * df$FUL       #Fumbles Lost
    df$pts_FUTD = pts_perFUTD * df$FUTD     #Own Fumbles Recovered Touchdowns 
    df$pts_twoPCS = pts_pertwoPCS * df$twoPCS #Two-Point Conversions Scored 
    df$pts_twoPCP = pts_pertwoPCP * df$twoPCP    #Two-Point Conversion Passes 
    df$pts_FGu20 = pts_perFGu20 * df$FGu20       #Field-Goals From 0-19 Yards
    df$pts_FGu30 = pts_perFGu30 * df$FGu30       #Field-Goals From 20-29 Yards
    df$pts_FGu40 = pts_perFGu40 * df$FGu40      #Field-Goals From 30-39 Yards 
    df$pts_FGu50 = pts_perFGu50 * df$FGu50       #Field-Goals From 40-49 Yards
    df$pts_FGo50 = pts_perFGo50 * df$FGo50       #Field-Goals From 50+ Yards
    df$pts_PATP = pts_perPATP * df$PATP           #Extra-Point Conversions
    df$pts_DES = pts_perDES * df$DES              #Defense Sacks
    df$pts_DEFR = pts_perDEFR * df$DEFR           #Opponent-Fumbles Recovered
    df$pts_DERTD = pts_perDERTD * df$DERTD  #Interception Return Touchdowns
    df$pts_DEFRTD = pts_perDEFRTD * df$DEFRTD      #Fumble Return Touchdowns
    df$pts_DEBRTD = pts_perDEBRTD * df$DEBRTD  #Blocked Kick Return Touchdowns
    df$pts_DEXPR = pts_perDEXPR * df$DEXPR       #Extra Points Return
    df$pts_DERTD = pts_perDERTD * df$DERTD     #Kickoff Return Touchdowns
    df$pts_DESF = pts_perDESF * df$DESF        #Safeties 
    df$pts_DEB = pts_perDEB * df$DEB         #Blocked Kicks
    df$pts_DEI = pts_perDEI * df$DEI         #Defensive Interceptions Made 
    
    if (DEPA == 0) df$pts_DEF = 10         #0 Defensive Points Allowed 
    else if (DEPA < 7 ) df$pts_DEF = 7     #1-6 Defensive Points Allowed
    else if (DEPA < 14 ) df$pts_DEF = 4    #7-13 Defensive Points Allowed
    else if (DEPA < 21 ) df$pts_DEF = 1    #14-20 Defensive Points Allowed
    else if (DEPA < 28 ) df$pts_DEF = 0    #21-27 Defensive Points Allowed
    else if (DEPA < 34 ) df$pts_DEF = -1   #28-34 Points Allowed 
    else if (DEPA > 34 ) df$pts_DEF = -4   #35+ Points Allowed 
    
    # calculate fanduel points
    df$fanduel_points <-  pts_RuY + pts_RuTD + pts_PaY + pts_PaTD + pts_I + pts_ReY + pts_ReTD + pts_Re + pts_KRTD + pts_PRTD + pts_FUL + pts_FUTD + pts_2PCS + pts_2PCP + pts_FGu20 + pts_FGu30 + pts_FGu40 + pts_FGu50 + pts_FGo50 + pts_PATP + pts_DES + pts_DEFR + pts_DERTD + pts_DEFRTD + pts_DEBRTD + pts_DEXPR + pts_DERTD + pts_DESF  + pts_DEB + pts_DEI + pts_DEF
    
    dat <- cbind(df$Player, df$)
    return 
}