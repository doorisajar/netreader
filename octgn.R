# Script file for processing OCTGN Netrunner data. 
# 

library(lubridate)
library(dplyr)

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

# Set Corp win to TRUE, Runner win to FALSE. 
octgn.df <- mutate(octgn.df, Win = (Result == "AgendaVictory"   | 
                                    Result == "FlatlineVictory" | 
                                    Result == "ConcedeVictory"  | 
                                    Result == "Flatlined"
                                    )
                   )

# May not need this anymore with the conversion to dplyr. 
# octgn.df$Win <- as.factor(octgn.df$Win)

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
                             Runner_Deck_Size   >= 45                                      &
                             Corp_Deck_Size     >= 40                                      &
                             Duration           >= 0
)
