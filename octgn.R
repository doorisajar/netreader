# Script file for processing OCTGN Netrunner data. 

# Includes. Need to handle dates, split/apply data frames, compute player ratings from wins/losses, and 
# plot stuff. 
library(lubridate)
library(ggplot2)
library(plyr)
library(PlayerRatings)
library(data.table)
library(dplyr)
library(reshape2)

# Useful functions. 
source("FactionWinrates.R")
source("WinRate.R")
source("FlatlineWins.R")


#-----------------------------------------------------------------------------
# PARSING AND CLEANUP
#-----------------------------------------------------------------------------

octgn.df <- read.csv('OCTGN_stats_anonymized-2014-04-01.csv', as.is = TRUE)
octgn.df <- tbl_df(octgn.df)

# Basic cleanup. This does eliminate 2012 games (about 12,000 of 156,000), because earlier versions
# didn't include the agenda count or deck size. 
# The POSIXct conversion is to make it play nice with dplyr. 
octgn.df <- na.omit(octgn.df)
octgn.df$GameStart <- parse_date_time(octgn.df$GameStart, "%Y%m%d %H%M%S")
octgn.df$GameStart <- as.POSIXct(octgn.df$GameStart)

# Convert the player identifiers to numeric in order to play nice with PlayerRatings.  
octgn.df$Corp_Player <- as.numeric(octgn.df$Corp_Player)
octgn.df$Runner_Player <- as.numeric(octgn.df$Runner_Player)

# Coerce identities to factors. "Player_Faction" denotes Corp, "Opponent_Faction" denotes Runner.  
octgn.df$Corporation <- as.factor(octgn.df$Corporation)
octgn.df$Runner <- as.factor(octgn.df$Runner)

# There are six possible outcomes:
# Corp Loss: Agenda Defeat, Deck Defeat, Conceded
# Corp Win: Agenda Victory, Flatline Victory, ConcedeVictory
octgn.df$Result <- as.factor(octgn.df$Result)

CheckWin <- function(x) { 
  win <- "NA"
  if ( x == "AgendaDefeat" | x == "DeckDefeat" | x == "Conceded" | x == "DeckVictory") 
    { win <- "False" }
  else if ( x == "AgendaVictory" | x == "FlatlineVictory" | x == "ConcedeVictory" | x == "Flatlined")
    { win <- "True" }
  win
}

# This doesn't work. mutate seems to pass the entire Result column to CheckWin, which is why it
# also didn't work when I wrote the whole if structure into the mutate call. 
# octgn.df <- mutate(octgn.df, Win = CheckWin(Result))

Win <- vector(length = length(octgn.df$Result))

for ( i in 1:length(octgn.df$Result) ) {
  Win[i] <- CheckWin(octgn.df$Result[i])
}

octgn.df$Win <- Win

# Success.
# filter(octgn.df, Win=="NA")

octgn.df$Win <- as.factor(octgn.df$Win)


# Rename the columns to simpler values. 
names(octgn.df)[names(octgn.df) == "Corporation"] <- "CorpID"
names(octgn.df)[names(octgn.df) == "Runner"] <- "RunID"

# New pruning with dplyr. Filter out Laramy Fisk, The Collective, wins/losses by concession, and deck sizes
# of zero. 
octgn.df <- filter(octgn.df, RunID              != "Criminal | Laramy Fisk"                &
                             RunID              != "Shaper | The Collective"               &
                             CorpID             != "Jinteki | Selective Mind Mapping"      &
                             CorpID             != "Haas-Bioroid | Selective Mind Mapping" &
                             Result             != "Conceded"                              &
                             Result             != "ConcedeVictory"                        &
                             Runner_Deck_Size   != 0                                       &
                             Corp_Deck_Size     != 0                                       &
                             Duration           >= 0
)



#-----------------------------------------------------------------------------
# PERIOD SELECTION FOR GLICKO CALCULATION
#-----------------------------------------------------------------------------

period.select <- "week"

# Take the date floor of each period to divide the games up for Glicko. 
# Tested "month" before, now testing "week".

octgn.df$Period <- floor_date(octgn.df$GameStart, period.select)



# Other interesting questions:
# Average win speed by faction?
# ...flatline wins, win speed, win margin, and influence usage by faction, matchup, and skill level. 
