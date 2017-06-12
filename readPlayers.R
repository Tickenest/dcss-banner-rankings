txt <- readLines("http://dobrazupa.org/tournament/0.18/all-players.html")

playerLines <- txt[grepl("<tr class=", txt)]

playerDF <- data.frame(Player = character(length(playerLines)),
                       Clan   = character(length(playerLines)),
                       Points = integer(length(playerLines)),
                       Wins   = integer(length(playerLines)),
                       Played = integer(length(playerLines)),
                       WinPct = numeric(length(playerLines)),
                       stringsAsFactors = FALSE)

for (i in 1:length(playerLines)){
    #Extract playername
    curLine     <- playerLines[i]
    playerStart <- regexpr(".html\">", curLine) + 7
    
    curLine     <- substr(curLine, playerStart, nchar(curLine))
    playerEnd   <- regexpr("</a>", curLine)
    
    playerName  <- substr(curLine, 1, playerEnd - 1)
    curLine     <- substr(curLine, playerEnd, nchar(curLine))
    
    #Extract clanname
    clanStart   <- regexpr("celltext\">", curLine) + 10
    
    curLine     <- substr(curLine, clanStart, nchar(curLine))
    clanEnd     <- regexpr("</td>", curLine)
    
    clanName    <- substr(curLine, 1, clanEnd - 1)
    curLine     <- substr(curLine, clanEnd, nchar(curLine))
    
    #If there's no clan name, then the processing is done.  Otherwise, process
    #the remaining data to extract the clan name.
    if (clanName != "") {
        clanStart <- regexpr(".html\">", clanName) + 7
        
        clanName  <- substr(clanName, clanStart, nchar(clanName))
        clanEnd   <- regexpr("</a>", clanName)
        
        clanName    <- substr(clanName, 1, clanEnd - 1)
        #curLine     <- substr(curLine, clanEnd, nchar(curLine))
    }
    
    #Extract points
    pointsStart <- regexpr("numeric\">", curLine) + 9
    
    curLine     <- substr(curLine, pointsStart, nchar(curLine))
    pointsEnd   <- regexpr("</td>", curLine)
    
    points      <- as.integer(substr(curLine, 1, pointsEnd - 1))
    curLine     <- substr(curLine, pointsEnd, nchar(curLine))
    
    #Extract wins
    winsStart   <- regexpr("numeric\">", curLine) + 9
    
    curLine     <- substr(curLine, winsStart, nchar(curLine))
    winsEnd     <- regexpr("</td>", curLine)
    
    wins        <- as.integer(substr(curLine, 1, winsEnd - 1))
    curLine     <- substr(curLine, winsEnd, nchar(curLine))
    
    #Extract games played
    playedStart <- regexpr("numeric\">", curLine) + 9
    
    curLine     <- substr(curLine, playedStart, nchar(curLine))
    playedEnd   <- regexpr("</td>", curLine)
    
    played      <- as.integer(substr(curLine, 1, playedEnd - 1))
    curLine     <- substr(curLine, playedEnd, nchar(curLine))
    
    #Calculate winning percentage
    winPct      <- round(wins/played, 4)
    
    #Add the current player to the playerDF data frame
    playerDF[i,1:6] <- list(playerName, clanName, points, wins, played, winPct)
}

fullPlayerCount <- nrow(playerDF)
#Filter out players who didn't complete a game
playerDF <- playerDF[playerDF$Played > 0,]

#Create a separate data frame for players with at least one win
winnerDF <- playerDF[playerDF$Wins > 0,]

#CALCULATE FOR ALL PLAYERS

#Calculate overall win percentage
overallWinPct <- round(sum(playerDF$Wins)/sum(playerDF$Played), 4)

#Sum points by clan, eliminate blank clan
pointsByClan <- aggregate(Points ~ Clan, playerDF, sum)
pointsByClan <- pointsByClan[pointsByClan$Clan != "",]
clanCounts <- as.data.frame(table(playerDF$Clan))
names(clanCounts) <- c("Clan", "Count")

#Join pointsByClan and clanCounts
pointsByClan <- merge(pointsByClan, clanCounts)

#Calculate each clan's points per player
pointsByClan$PointsPerPlayer <- pointsByClan$Points / pointsByClan$Count

pointsByClan[order(pointsByClan$PointsPerPlayer, decreasing=TRUE),]

#CALCULATE JUST FOR WINNERS

#Calculate overall win percentage
overallWinPct <- round(sum(winnerDF$Wins)/sum(winnerDF$Played), 4)

#Sum points by clan, eliminate blank clan
pointsByClan <- aggregate(Points ~ Clan, winnerDF, sum)
pointsByClan <- pointsByClan[pointsByClan$Clan != "",]
clanCounts <- as.data.frame(table(winnerDF$Clan))
names(clanCounts) <- c("Clan", "Count")

#Join pointsByClan and clanCounts
pointsByClan <- merge(pointsByClan, clanCounts)

#Calculate each clan's points per player
pointsByClan$PointsPerPlayer <- pointsByClan$Points / pointsByClan$Count

pointsByClan[order(pointsByClan$PointsPerPlayer, decreasing=TRUE),]