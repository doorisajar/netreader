# Script file for processing OCTGN Netrunner data. 
# Simply modify the filename and path in line 11, and season to taste. 

library(lubridate)
library(dplyr)

#-----------------------------------------------------------------------------
# PARSING AND CLEANUP
#-----------------------------------------------------------------------------

# octgn.df <- read.csv('OCTGN_stats_anonymized-2014-04-01.csv', as.is = TRUE)
octgn.df <- tbl_df(octgn.df)

# Basic cleanup. This does eliminate 2012 games (about 12,000 of 156,000), because earlier versions
# didn't include the agenda count or deck size. 
# The POSIXct conversion is to make it play nice with dplyr. 
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

# Rename the columns to simpler values. 
names(octgn.df)[names(octgn.df) == "Corporation"] <- "CorpID"
names(octgn.df)[names(octgn.df) == "Runner"] <- "RunID"


#-----------------------------------------------------------------------------
# FILTERING
#-----------------------------------------------------------------------------

# Filter out Laramy Fisk, The Collective, wins/losses by concession, and deck sizes of zero. 
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


#-----------------------------------------------------------------------------
# DATA PACKS
#-----------------------------------------------------------------------------

# Use version number to determine which data packs were included. 

# List of plugin versions associated with data packs, for reference. 
# "3.9.0","Double Time"
# "3.8.0","Fear and Loathing"
# "3.7.0","True Colors"
# "3.6.0","Mala Tempora"
# "3.5.0","Second Thoughts"
# "3.3.0","Opening Moves"
# "3.2.0","Creation and Control"
# "3.1.0","Future Proof"
# "3.0.0","Humanity's Shadow"
# "2.3.0","A Study in Static"
# "2.2.0","Cyber Exodus"
# "2.1.0","Trace Amount" # NOT CERTAIN
# "2.0.0","What Lies Ahead"

# Consolidate all packs into a df. 
data.packs <- data.frame(rep( (20:39) / 10 ), 
                         c("What Lies Ahead",
                           "Trace Amount",
                           "Cyber Exodus",
                           "A Study in Static",
                           "A Study in Static",
                           "A Study in Static",
                           "A Study in Static",
                           "A Study in Static",
                           "A Study in Static",
                           "A Study in Static",
                           "Humanity's Shadow",
                           "Future Proof",
                           "Creation and Control",
                           "Opening Moves",
                           "Opening Moves",
                           "Second Thoughts",
                           "Mala Tempora",
                           "True Colors",
                           "Fear and Loathing",
                           "Double Time"))

names(data.packs) <- c("Version", "Pack")

data.packs$Pack <- ordered(data.packs$Pack, c("What Lies Ahead",
                                              "Trace Amount",
                                              "Cyber Exodus",
                                              "A Study in Static",
                                              "Humanity's Shadow",
                                              "Future Proof",
                                              "Creation and Control",
                                              "Opening Moves",
                                              "Second Thoughts",
                                              "Mala Tempora",
                                              "True Colors",
                                              "Fear and Loathing",
                                              "Double Time"
)
)

data.packs <- select(data.packs, Version:Pack)
data.packs$Version <- as.character(data.packs$Version)
data.packs$Version[1] <- "2.0"
data.packs$Version[11] <- "3.0"

# Need to match the versions in this df to the versions in octgn.df. 
# Follow this form:
#
# substr(octgn.df$Version[1], 1, 3)
# 
# For each 1:3 chars in octgn.df$Version, I want to match with data.packs$Version and populate a new
# column in octgn.df with the associated data pack. 
#
# Write a simple match function to take a character string of the form "x.x" and return the most recent
# data pack. 

CheckPack <- function(x, data.packs) {
  return( data.packs$Pack[match( substr(x, 1, 3), data.packs$Version )] )
}

octgn.df <- mutate(octgn.df, Pack = CheckPack(octgn.df$Version, data.packs))

# I checked out the games with Pack = NA; they're simply a small subset of games (about 300) where
# the version number was either mangled or missing. No problem to simply drop them. 
octgn.df <- filter(octgn.df, Pack %in% levels(octgn.df$Pack))
