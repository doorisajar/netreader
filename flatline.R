# Script file for estimating the effect of player skill on flatline winrates for both Corp and Runner IDs. 

# Includes. Need to handle dates, split/apply data frames, compute player ratings from wins/losses, and 
# plot stuff. 
library(lubridate)
library(plyr)
library(PlayerRatings)
library(dplyr)

# Useful functions. 
source("FactionWinrates.R")
source("WinRate.R")
source("FlatlineWins.R")

# Files that we've written out in previous scripts. 
load('player-ratings.Rda')


#-----------------------------------------------------------------------------
# PERIOD SELECTION FOR FLATLINE CALCULATION
#-----------------------------------------------------------------------------

flatline.df <- rated.games
flatline.df$Period <- flatline.df$Pack

#-----------------------------------------------------------------------------
# 
#-----------------------------------------------------------------------------

top.all <- player.ratings
top.half <- filter(player.ratings, Rating >= mean(player.ratings$Rating))
top.sd <- filter(player.ratings, Rating >= mean(player.ratings$Rating) + sd(player.ratings$Rating))
top.2sd <- filter(player.ratings, Rating >= mean(player.ratings$Rating) + 2 * sd(player.ratings$Rating))

top.all <- filter(flatline.df, Corp_Player %in% top.all$Player | Runner_Player %in% top.all$Player)
top.half <- filter(flatline.df, Corp_Player %in% top.half$Player | Runner_Player %in% top.half$Player)
top.sd <- filter(flatline.df, Corp_Player %in% top.sd$Player | Runner_Player %in% top.sd$Player)
top.2sd <- filter(flatline.df, Corp_Player %in% top.2sd$Player | Runner_Player %in% top.2sd$Player)


#-----------------------------------------------------------------------------
# CORP WINS BY FLATLINE
#-----------------------------------------------------------------------------

corp.all <- FlatlineWinrates(top.all)
corp.half <- FlatlineWinrates(top.half)
corp.sd <- FlatlineWinrates(top.sd)
corp.2sd <- FlatlineWinrates(top.2sd)

packs <- c("Trace Amount",
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

corp.all$Pack <- packs
corp.half$Pack <- packs
corp.sd$Pack <- packs
corp.2sd$Pack <- packs

corp.all <- tbl_df(top.all)
corp.half <- tbl_df(top.half)
corp.sd <- tbl_df(top.sd)
corp.2sd <- tbl_df(top.2sd)

corp.all$"Weyland Consortium | Building a Better World"
corp.half$"Weyland Consortium | Building a Better World"
corp.sd$"Weyland Consortium | Building a Better World"
corp.2sd$"Weyland Consortium | Building a Better World"

corp.all$"Jinteki | Personal Evolution"
corp.half$"Jinteki | Personal Evolution"
corp.sd$"Jinteki | Personal Evolution"
corp.2sd$"Jinteki | Personal Evolution"

# So percentage of games won by flatline doesn't change massively with Corp skill. 

#-----------------------------------------------------------------------------
# RUNNER LOSSES BY FLATLINE
#-----------------------------------------------------------------------------

run.all <- FlatlineLosses(top.all)
run.half <- FlatlineLosses(top.half)
run.sd <- FlatlineLosses(top.sd)
run.2sd <- FlatlineLosses(top.2sd)

run.all$Pack <- packs
run.half$Pack <- packs
run.sd$Pack <- packs
run.2sd$Pack <- packs

run.all$"Shaper | Kate McCaffrey"
run.half$"Shaper | Kate McCaffrey"
run.sd$"Shaper | Kate McCaffrey"
run.2sd$"Shaper | Kate McCaffrey"

run.all$"Criminal | Andromeda"
run.half$"Criminal | Andromeda"
run.sd$"Criminal | Andromeda"
run.2sd$"Criminal | Andromeda"


run.all <- top.all %.%
            group_by(RunID, Pack) %.%
            summarise(FlatLoss = (Result == "FlatlineVictory" | Result == "Flatlined"),
                      Games = n(),
                      FlatRate = sum(FlatLoss) / Games
            )
  
  # V5 = (V1 == 1 & V2 != 4) + 2 * (V2 == 4 & V3 != 1)
  
top.all %.% 
  mutate(Flatline = Result == "FlatlineVictory" | Result == "Flatlined") %.%
  select(CorpID, RunID, Flatline)

top.all$Result[4]
top.all$Win[4]

  matchups.df <- rated.games %.%
  group_by(CorpID, RunID, Pack) %.%
  summarise(CorpWins = sum(Win) / length(Win),
            RunWins = 1 - CorpWins,
            Games = n()
  )
