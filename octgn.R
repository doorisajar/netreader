# Script file for processing OCTGN Netrunner data. 

# Includes. Need to handle dates, split/apply data frames, compute player ratings from wins/losses, and 
# plot stuff. 
library(lubridate)
library(ggplot2)
library(plyr)
library(PlayerRatings)
library(data.table)
library(dplyr)

# Function and data wrapper for computing faction win rates from data that is passed in. 
source("FactionWinrates.R")
source("WinRate.R")
source("FlatlineWins.R")


#-----------------------------------------------------------------------------
# PARSING AND CLEANUP
#-----------------------------------------------------------------------------

octgn.df <- read.csv('OCTGN_stats_anonymized-2014-01-13.csv', as.is = TRUE)

# Basic cleanup. This does eliminate 2012 games (about 12,000 of 156,000), because earlier versions
# didn't include the agenda count or deck size. 
octgn.df <- na.omit(octgn.df)
octgn.df$GameStart <- parse_date_time(octgn.df$GameStart, "%Y%m%d %H%M%S")

# Remove Laramy Fisk and The Collective. 
octgn.df <- octgn.df[with(octgn.df, 
                          !(Opponent_Faction == "Criminal | Laramy Fisk" 
                            | Opponent_Faction == "Shaper | The Collective")), ]

# Convert the player identifiers to numeric in order to play nice with PlayerRatings.  
octgn.df$Corp_Player <- as.numeric(octgn.df$Corp_Player)
octgn.df$Runner_Player <- as.numeric(octgn.df$Runner_Player)

# Coerce identities to factors. "Player_Faction" denotes Corp, "Opponent_Faction" denotes Runner.  
octgn.df$Player_Faction <- as.factor(octgn.df$Player_Faction)
str(octgn.df$Player_Faction) # 11 levels, all Corp
levels(octgn.df$Player_Faction)

octgn.df$Opponent_Faction <- as.factor(octgn.df$Opponent_Faction)
str(octgn.df$Opponent_Faction) # 10 levels, all Runner
levels(octgn.df$Opponent_Faction)

# There are six possible outcomes:
# Corp Loss: Agenda Defeat, Deck Defeat, Conceded
# Corp Win: Agenda Victory, Flatline Victory, ConcedeVictory
octgn.df$Result <- as.factor(octgn.df$Result)
levels(octgn.df$Result)

octgn.df$Win <- as.factor(octgn.df$Win)
levels(octgn.df$Win)

# Take the date floor of each month in order to divide the results by Year/Month. 
octgn.df$Period <- floor_date(octgn.df$GameStart, "month")

# Rename the columns to simpler values. 
names(octgn.df)[names(octgn.df) == "Player_Faction"] <- "CorpID"
names(octgn.df)[names(octgn.df) == "Opponent_Faction"] <- "RunID"
names(octgn.df)[names(octgn.df) == "P_ANR"] <- "CorpAgCards"
names(octgn.df)[names(octgn.df) == "P_CNR"] <- "CorpDeckSize"
names(octgn.df)[names(octgn.df) == "O_ANR"] <- "RunAgCards"
names(octgn.df)[names(octgn.df) == "O_CNR"] <- "RunDeckSize"

names(octgn.df)

# Now convert it to a data table. I could have done all of the above manipulation after the conversion, but
# it was all written already. 
# octgn.dt <- data.table(octgn.df)


# ALSO NEED TO FILTER GAMES WHERE RUNDECKSIZE OR CORPDECKSIZE ARE ZERO! Or check with db0 about what
# that means. 

# Auditing flatline win percentage code. 
test <- rated.games[rated.games$CorpID=="Jinteki | Personal Evolution", ]
test$Period <- as.character(test$Period)
test <- test[test$Period=="2013-01-01", ]

wins <- sum(as.logical(test$Win)) / nrow(test)
flatline.wins <- sum(as.logical(test$Result == "FlatlineVictory")) / sum(as.logical(test$Win))
# This does return the same value as calling the function. 



# Other interesting questions:
# Average win speed by faction?
# ...flatline wins, win speed, win margin, and influence usage by faction, matchup, and skill level. 
