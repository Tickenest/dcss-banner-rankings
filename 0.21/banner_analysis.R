#Uses the gdata library for writing the FWF output
library(gdata)

#Set the working directory
wd <- "D:\\Statistics\\DCSS\\dcss-banner-rankings\\0.21"
setwd(wd)

#Get the page for all the player banners from the tournament.  Only run this
#line of code once per session (no need to hammer the server repeatedly.)
page <- paste(readLines('http://dobrazupa.org/tournament/0.21/banners.html'),collapse='')

#All 72 categories, to be used for extracting HTML values and also as column
#names
cats <- c("<h3>The Explorer I", "<h3>The Explorer II", "<h3>The Explorer III",
          "<h3>The Heretic I", "<h3>The Heretic II", "<h3>The Heretic III",
          "<h3>Slow and Steady I", "<h3>Slow and Steady II", "<h3>Slow and Steady III",
          "<h3>The Politician I", "<h3>The Politician II", "<h3>The Politician III",
          "<h3>The Pious I", "<h3>The Pious II", "<h3>The Pious III",
          "<h3>Nature's Ally I", "<h3>Nature's Ally II", "<h3>Nature's Ally III",
          "<h3>Avarice I", "<h3>Avarice II", "<h3>Avarice III",
          "<h3>The Inheritor I", "<h3>The Inheritor II", "<h3>The Inheritor III",
          "<h3>Gelatinous Body I", "<h3>Gelatinous Body II", "<h3>Gelatinous Body III",
          "<h3>Lord of Darkness I", "<h3>Lord of Darkness II", "<h3>Lord of Darkness III",
          "<h3>Spiteful I", "<h3>Spiteful II", "<h3>Spiteful III",
          "<h3>Speed Demon I", "<h3>Speed Demon II", "<h3>Speed Demon III",
          "<h3>Nemelex' Choice I", "<h3>Nemelex' Choice II", "<h3>Nemelex' Choice III",
          "<h3>The Conqueror I", "<h3>The Conqueror II", "<h3>The Conqueror III",
          "<h3>The Prophet I", "<h3>The Prophet II", "<h3>The Prophet III",
          "<h3>The Ascetic I", "<h3>The Ascetic II", "<h3>The Ascetic III",
          "<h3>The Lorekeeper I", "<h3>The Lorekeeper II", "<h3>The Lorekeeper III",
          "<h3>Vow of Courage I", "<h3>Vow of Courage II", "<h3>Vow of Courage III",
          "<h3>Brute Force I", "<h3>Brute Force II", "<h3>Brute Force III",
          "<h3>Graceful I", "<h3>Graceful II", "<h3>Graceful III",
          "<h3>Ruthless Efficiency I", "<h3>Ruthless Efficiency II", "<h3>Ruthless Efficiency III",
          "<h3>Descent into Madness I", "<h3>Descent into Madness II", "<h3>Descent into Madness III",
          "<h3>The Harvest I", "<h3>The Harvest II", "<h3>The Harvest III",
          "<h3>Angel of Justice I", "<h3>Angel of Justice II", "<h3>Angel of Justice III")

#Function to remove the "<h3>" from the category names
subs <- function(instr) {
    return(substr(instr,5,nchar(instr)))
}
#Get the list of column names
cols <- sapply(cats, subs)

#Build a data frame for the player information
players <- data.frame(Name=character(), stringsAsFactors=FALSE)

#Populate the other 72 columns, one for each achievement
for (i in cols) {
    players[, i] <- logical()
}

#Figure out the player banners

#Work from right to left, because for each level 1 or level 2 banner, we need
#to check whether the player has already achieved a higher level banner so that
#we don't give them credit for more than one banner in the same category
#(possible with Nemelex' Choice).  This also avoids weird situations such as
#a lower level banner being worth more than a higher level banner in the same
#group just because fewer players happened to stop at level 2 than went on to
#level 3.

#The variable i will go from 72 to 1.
for (i in length(cats):1){
    #Get the starting position of the current banner in the HTML
    tStart <- regexpr(cats[i], page, fixed=TRUE)[1]
    #If tStart = -1, it means that the current banner hasn't been won yet, so
    #just move on to the next one
    if (tStart == -1) {next}
    #Get the ending position of the current banner in the HTML
    tEnd <- regexpr("</div>", substr(page,tStart,nchar(page)), fixed=TRUE)[1]
    #Get the actual text we're going to parse
    sText <- substr(page, tStart, tStart+tEnd)
    
    #Now loop through the text, pulling out each player's name and giving them
    #credit for the achievement
    while (TRUE) {
        #Get the start of a chunk of text that contains the player's name
        pStart <- regexpr(".html", sText, fixed=TRUE)[1]
        
        #If ".html" wasn't found in the remaining text, then we're done
        #processing players
        if (pStart == -1){break}
        
        #Get the end of a chunk of text that contains the player's name
        pEnd   <- regexpr("</a>", substr(sText, pStart, nchar(sText)), fixed=TRUE)[1]
        
        #Now actually extract the player's name from the chunk of text.
        #Because a few players use the same name but different capitalizations
        #on different servers, convert all names to all lowercase.
        pName  <- tolower(substr(sText, pStart+7, pStart+pEnd-2))
        
        #If the current banner is one of Nemelex' Choice, then we have to do
        #some additional name processing because the player name will also
        #include, in parentheses, the character combinations with which the
        #player achieved that particular banner.  If we just extracted all of
        #that text as the player name, the script would treat that player as a
        #distinct player, which we don't want.  For example, "Tickenest" and
        #"Tickenest(SpCj)" would be treated as distinct players.
        if (length(grep("Nemelex'", cats[i])) > 0) {
            nEnd  <- regexpr("(", pName, fixed=TRUE)[1]
            pName <- substr(pName,1,nEnd-1)
        }
        
        #If this isn't the player's first banner, give them credit for the
        #current banner only if it's their highest level banner in the group.
        #It's possible for players to earn more than one Nemelex' Choice banner
        #but we only want to credit the player with their highest Nemelex'
        #Choice banner.  This is why the checks for each banner group go from
        #highest to lowest.
        if (pName %in% players$Name){
            if (i %% 3 == 2) {
                if (players[players$Name == pName,i+2] != TRUE){
                    players[players$Name == pName,i+1] <- TRUE
                }
            } else if (i %% 3 == 1) {
                if ((players[players$Name == pName,i+3] != TRUE) & (players[players$Name == pName,i+2] != TRUE)){
                    players[players$Name == pName,i+1] <- TRUE
                }
            } else {
                players[players$Name == pName,i+1] <- TRUE
            }
        #If this is the current player's first banner, then that player has to
        #be added to the data frame and they have to be given a FALSE value for
        #every banner except the current banner.
        } else {
            players[nrow(players)+1,1] <- pName
            players[nrow(players),2:ncol(players)] <- FALSE
            players[nrow(players),i+1] <- TRUE
        }
        #Now remove the current player from the HTML text that we're processing,
        #then start the loop over with the remaining HTML text
        sText <- substr(sText, pStart+pEnd, nchar(sText))
    }
}

################################################################################
#We'll calculate points based upon the rarity of each particular banner.  First,
#calculate the number of players who achieved each level.  Then, figure out
#which banner was the most popular (so players who didn't achieve ANY banners
#don't alter the calculations.)  To calculate the value of each banner, divide
#the number of players who got the most popular banner by the number of players
#who got each banner.  So if 1000 players got the most popular banner, that
#banner is worth 1000/1000 = 1 point.  If 1 player got a particular banner, then
#that banner is worth 1000/1 = 1000 points.  If 50 players got a particular
#banner, then that banner is worth 1000/50 = 20 points.
########################################
#This time, only allow each player to have one banner (their highest) per group.
#For each column, the number of player who earned that banner is the number of
#player who earned that banner OR a higher banner in the same group.  Remember
#that each player only gets credit for their highest banner in the group.

#Create a new vector that will hold the final player counts for each banner
#column in the players dataframe
colCounts <- integer()

#If we're looking at a column for the highest banner of a group, just count how
#many players got that banner.  If it's a level 2 banner, count the players who
#got that banner or the level 3 banner, and so on for a level 1 banner.
for (i in 2:ncol(players)) {
    if (i %% 3 == 1) {
        colCounts[length(colCounts)+1] <- sum(players[,i])
    } else if (i %% 3 == 0) {
        colCounts[length(colCounts)+1] <- sum(players[,i]) + sum(players[,i+1])
    } else {
        colCounts[length(colCounts)+1] <- sum(players[,i]) + sum(players[,i+1]) + sum(players[,i+2])
    }
}

#Get the number of players who got the most popular banner
numer <- 0
for (i in 1:(length(colCounts)/3)) {
    print(sum(colCounts[(3*i-2):(3*i)]))
    if (sum(colCounts[(3*i-2):(3*i)]) > numer) {numer <- sum(colCounts[(3*i-2):(3*i)])}
}
#numer <- max(colCounts)
#Calculate the column scores for each column
colScores <- numer/colCounts
#Create a dataframe of the columns and what they are worth
colScoresDF <- as.data.frame(colScores)
colScoresDF$Achievement <- cols
colScoresDF <- colScoresDF[,c(2,1)]
names(colScoresDF) <- c("Achievement", "Score")

#Write the achievement scores to a text file
write.fwf(colScoresDF, file="banner_scores.txt")

#Function to determine the total score for one player in the players dataframe
getScore <- function(pName) {
    #Get the banner TRUE-FALSE values for player pName, then transpose it so
    #that R doesn't treat the scores as a single vector, then subset colScores
    #by that vector.  This will filter out all of the colScores for the columns
    #that the player didn't earn.  Finally, add up the sum of those scores for
    #the good columns to get the player's final score, and return that score.
    return (sum(colScores[as.logical(t(players[players$Name == pName,2:73]))]))
}

#Calculate each player's banner score
players$Score <- sapply(players$Name, getScore)

#Now sort the rows by player score, and then assign each player their final rank
players <- players[order(players$Score, decreasing=TRUE),]
players$Rank <- rank(-players$Score, ties.method="min")

#Function to return an "X" for a TRUE value and a "" for a FALSE value
returnChar <- function(inVal){
    return (ifelse(inVal == TRUE, "X", ""))
}

#Convert the TRUE values to X and the FALSE values to "" so that it looks nicer
#in a displayed table
players[,2:(ncol(players)-2)] <- apply(players[,2:(ncol(players)-2)], c(1,2), returnChar)

#Put the player name, score, and rank columns at the front
players <- players[,c(1,(ncol(players)-1),ncol(players),2:(ncol(players)-2))]

#Round the player scores to 3 decimal places
players$Score <- round(players$Score,3)

#Now download the actual player rankings so that they can be compared to the
#banner rankings
scores <- paste(readLines('http://dobrazupa.org/tournament/0.21/all-players.html'),collapse='')

pScoresDF <- data.frame(Name=character(), T.Score=integer(), T.Rank=integer(),
                        stringsAsFactors=FALSE)

#Now loop through the scores text, pulling out each player's name and their
#score for the tournament
while (TRUE) {
    
    #Get the start of a chunk of text that contains the player's name
    lStart <- regexpr("<tr class=", scores, fixed=TRUE)[1]
    
    #If "<tr class=" wasn't found in the remaining text, then we're done
    #processing players
    if (lStart == -1){break}
    
    #Get the end of a chunk of text that contains the player's info
    lEnd   <- regexpr("</tr>", substr(scores, lStart, nchar(scores)), fixed=TRUE)[1]
    
    #Extract the player info line from the HTML
    lText <- substr(scores, lStart, lStart+lEnd)
    
    #Hold on to the number of characters to advance after we process the
    #current player
    advance <- lStart+lEnd
    
    #Find the first number, which will be the player's rank
    pRankStart <- regexpr('<td class="numeric">', lText, fixed=TRUE)[1]
    
    #Find the end of the score
    pRankEnd <- regexpr("</td>", lText, fixed=TRUE)[1]
    
    #Now extract the actual score
    pRank <- as.integer(substr(lText, pRankStart+20, pRankEnd-1))
    
    #Get the start of a chunk of text that contains the player's name
    pStart <- regexpr(".html", lText, fixed=TRUE)[1]
    
    #Get the end of a chunk of text that contains the player's name
    pEnd   <- regexpr("</a>", substr(lText, pStart, nchar(lText)), fixed=TRUE)[1]
    
    #Now actually extract the player's name from the chunk of text.
    #Because a few players use the same name but different capitalizations
    #on different servers, convert all names to all lowercase.
    pName  <- tolower(substr(lText, pStart+7, pStart+pEnd-2))
    
    #Now pare down lText to include only the text after the player's name
    lText <- substr(lText, pStart+pEnd+2, nchar(lText))
    
    #Now find the first number after the player's name
    pScoreStart <- regexpr('<td class="numeric">', lText, fixed=TRUE)[1]
    
    #Find the end of the score
    pScoreEnd <- regexpr("</td>", substr(lText, pScoreStart, nchar(lText)), fixed=TRUE)[1]
    
    #Now extract the actual score
    pScore <- as.integer(substr(lText, pScoreStart+20, pScoreStart+pScoreEnd-2))
    
    #Now remove the current player from the HTML text that we're processing,
    #then start the loop over with the remaining HTML text.
    scores <- substr(scores, advance, nchar(scores))
    
    #Now add the current player and score to pScoresDF
    pScoresDF[nrow(pScoresDF)+1, c(1,2,3)] <- list(pName, pScore, pRank)
}

#Join the players dataframe to the pScoresDF dataframe, discarding any players
#in pScoresDF who aren't in players
players <- merge(players, pScoresDF, all.x=TRUE)

#Now reorder the players dataframe by banner rank
players <- players[order(players$Rank),]

#Write the final output table, both ranking info and all the banner info
write.fwf(players[,c(1,2,3,ncol(players)-1,ncol(players))], file="players_banner_rankings.txt",
          sep="      ")
write.csv(players, "players_banner_rankings.csv", row.names=FALSE)
#write.csv(players, file="clipboard-16384", row.names=FALSE)

########################################
#Now calculate clan banner rankings.  A clan will get credit for the highest
#level of each banner that any player in the clan has achieved.  No extra
#credit for more than one player in a clan getting the same banner and level.

#Get the page for all the clan members from the tournament.  Only run this
#line of code once per session (no need to hammer the server repeatedly.)
clans <- paste(readLines('http://dobrazupa.org/tournament/0.21/teams.html'),collapse='')

#Data frame to hold the information about each clan
cScoresDF <- data.frame(Name=character(), T.Score=integer(), T.Rank=integer(),
                        player1=character(), player2=character(), player3=character(),
                        player4=character(), player5=character(), player6=character(),
                        Score=numeric(), Rank=integer(),
                        stringsAsFactors=FALSE)

#Now loop through the scores text, pulling out each clan's name and their
#score for the tournament
while (TRUE) {
    
    #Get the start of a chunk of text that contains the clan's name
    cStart <- regexpr("<tr class=", clans, fixed=TRUE)[1]
    
    #If "<tr class=" wasn't found in the remaining text, then we're done
    #processing clans
    if (cStart == -1){break}
    
    #Get the end of a chunk of text that contains the clan's info
    cEnd <- regexpr("</tr>", substr(clans, cStart, nchar(clans)), fixed=TRUE)[1]
    
    #Hold on to the number of characters to advance after we process the
    #current clan
    advance <- cStart+cEnd
    
    #Extract the clan info line from the HTML
    cText <- substr(clans, cStart, advance)
    
    #Find the first number, which will be the clan's rank
    cRankStart <- gregexpr('<td class="numeric">', cText, fixed=TRUE)[[1]][1]
    
    #Find the end of the rank
    cRankEnd <- gregexpr("</td>", cText, fixed=TRUE)[[1]][1]
    
    #Now extract the actual rank
    cRank <- as.integer(substr(cText, cRankStart+20, cRankEnd-1))
    
    #Now make a new row for the current clan and add its rank
    cScoresDF[nrow(cScoresDF)+1, 3] <- cRank
    
    #Find the second number, which will be the clan's score
    cScoreStart <- gregexpr('<td class="numeric">', cText, fixed=TRUE)[[1]][2]
    
    #Find the end of the score
    cScoreEnd <- gregexpr("</td>", cText, fixed=TRUE)[[1]][2]
    
    #Now extract the actual score
    cScore <- as.integer(substr(cText, cScoreStart+20, cScoreEnd-1))
    
    #Now add the current clan's score to its row
    cScoresDF[nrow(cScoresDF), 2] <- cScore
    
    #Now extract the text that holds the clan name and the player names
    cInfo <- gregexpr('<a href=(\\s|\\S)*?</a>', cText)
    
    #The first entry is for the clan's name.  Every other entry is for a player.
    cNameInfo <- substr(cText, cInfo[[1]][1],
                        cInfo[[1]][1] + attr(cInfo[[1]],"match.length")[1])
    
    #Extract the name of the clan
    cNameStart <- regexpr('">', cNameInfo)[1] + 2
    cNameEnd   <- regexpr('</a', cNameInfo)[1] - 1
    cName      <- substr(cNameInfo, cNameStart, cNameEnd)
    
    #Make a new row in the dataframe and add the clan name to it
    cScoresDF[nrow(cScoresDF), 1] <- cName
    
    #Now get the name of every player in the clan
    for (i in 2:length(cInfo[[1]])) {
        
        #Extract the current player's info from cText
        cPNameInfo <- substr(cText, cInfo[[1]][i],
                             cInfo[[1]][i] + attr(cInfo[[1]],"match.length")[i])
        
        #Extract the name of the player.  Because a few players use the same
        #name but different capitalizations on different servers, convert all
        #names to all lowercase.
        cPNameStart <- regexpr('">', cPNameInfo)[1] + 2
        cPNameEnd   <- regexpr('</a', cPNameInfo)[1] - 1
        cPName      <- tolower(substr(cPNameInfo, cPNameStart, cPNameEnd))
        
        #Add the current player's name to his clan's dataframe row
        cScoresDF[nrow(cScoresDF), i+2] <- cPName
        
        #If this is the first player in the clan (the captain), add that
        #player's name to the end of the clan name.  This serves to distinguish
        #different clans using the same name (which is apparently allowed)
        if (i == 2) {
            cScoresDF[nrow(cScoresDF), 1] <- 
                paste0(cScoresDF[nrow(cScoresDF), 1], " (", cPName, ")")
        }
    }
    
    #Now remove the current clan from the HTML text that we're processing,
    #then start the loop over with the remaining HTML text.
    clans <- substr(clans, advance, nchar(clans))
}

#Now consolidate all of the banners for each clan.  Take the highest level
#banner that anyone in the clan has achieved, and then score the clan as if
#it were a person.

#Add a column for each banner to cScoresDF.
for (i in cols) {
    cScoresDF[, i] <- character()
}

#Start by subsetting the player banner information we've already created.
#We don't need the scoring columns, just the name and banner columns.
pBanners <- players[,c(1,4:75)]

#For each clan, create a dataframe of just the banners of the players in the
#clan.  Then consolidate those banners as described above.  Finally, score the
#clan.
for (i in 1:nrow(cScoresDF)) {
    
    #Start by making a vector of the players in the clan.  Any clans that don't
    #have the maximum numbers of players will have NA entries at the end of
    #this vector, which will wind up being harmless.
    cPlayers <- as.character(cScoresDF[i,4:9])
    
    #Subset pBanners based upon the names in cPlayers
    cBanners <- pBanners[pBanners$Name %in% cPlayers,2:ncol(pBanners)]
    
    #Make a new row at the bottom of the dataframe that will hold the
    #consolidated info.  Make all of the entries blank strings to start.
    cBanners[nrow(cBanners)+1,1:72] <- ""
    
    #For each set of 3 columns representing one banner group, find the latest
    #column in the group that contains at least one "X"
    for (j in 1:(ncol(cBanners)/3)) {
        
        if (any(cBanners[3*j] != "")) {
            cBanners[nrow(cBanners),3*j] <- "X"
            next
        } else if (any(cBanners[3*j-1] != "")) {
            cBanners[nrow(cBanners),3*j-1] <- "X"
            next 
        } else if (any(cBanners[3*j-2] != "")) {
            cBanners[nrow(cBanners),3*j-2] <- "X"
        }
    }
    
    #Now calculate the current clan's score based upon the banner scores
    cBannerVec <- t(cBanners[nrow(cBanners),1:72])
    cScoresDF$Score[i] <- sum(colScores[ifelse(cBannerVec == "", FALSE, TRUE)])
    
    #Assign the final banner information to the correct columns in cScoresDF
    cScoresDF[i,12:ncol(cScoresDF)] <- cBannerVec
}

#Now reorder the rows by clan banner score, assign the clan banner rankings,
#reorder the columns a bit, and write the output
cScoresDF <- cScoresDF[order(cScoresDF$Score, decreasing=TRUE),]
cScoresDF$Rank <- rank(-cScoresDF$Score, ties.method="min")
cScoresDF <- cScoresDF[c(1,10,11,2:9,12:ncol(cScoresDF))]

write.fwf(cScoresDF[,1:11], file="clans_banner_rankings.txt", sep="      ")
write.csv(cScoresDF, "clans_banner_rankings.csv", row.names=FALSE)