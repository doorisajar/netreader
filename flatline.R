# Script file for estimating the effect of player skill on flatline winrates for both Corp and Runner IDs. 

library(dplyr)

# Files that we've written out in previous scripts. 
load('player-ratings.Rda')
load('rated-games.Rda')


#-----------------------------------------------------------------------------
# GAME SUBSET GENERATION
#-----------------------------------------------------------------------------

flatline.df <- octgn.df

# Take all players, the top half, 1+ sd above mean, 2+ sd above mean, and create a df for each group
# comprising all of their games. This allows comparisons by changing skill level. 

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

corp.all <- top.all %.% 
            group_by(CorpID, Pack) %.%
            summarise(CorpWins = sum(Win) / length(Win),
                      RunWins = 1 - CorpWins,
                      Games = n(),
                      Flatline = sum(Result == "FlatlineVictory" | Result == "Flatlined") / sum(Win)
                      )

corp.half <- top.half %.% 
  group_by(CorpID, Pack) %.%
  summarise(CorpWins = sum(Win) / length(Win),
            RunWins = 1 - CorpWins,
            Games = n(),
            Flatline = sum(Result == "FlatlineVictory" | Result == "Flatlined") / sum(Win)
  )

corp.sd <- top.sd %.% 
  group_by(CorpID, Pack) %.%
  summarise(CorpWins = sum(Win) / length(Win),
            RunWins = 1 - CorpWins,
            Games = n(),
            Flatline = sum(Result == "FlatlineVictory" | Result == "Flatlined") / sum(Win)
  )

corp.2sd <- top.2sd %.% 
  group_by(CorpID, Pack) %.%
  summarise(CorpWins = sum(Win) / length(Win),
            RunWins = 1 - CorpWins,
            Games = n(),
            Flatline = sum(Result == "FlatlineVictory" | Result == "Flatlined") / sum(Win)
  )

# Need some code here to compare flatline percentages (percentage of wins that are flatlines) across
# Corp skill levels. The Corp data frames have different row counts... we lose some IDs in some data packs
# as we cut from all to half to the stdevs. 

# setdiff looks at two arguments and returns the observations that are different. 

#-----------------------------------------------------------------------------
# RUNNER LOSSES BY FLATLINE
#-----------------------------------------------------------------------------

run.all <- top.all %.% 
  group_by(RunID, Pack) %.%
  summarise(CorpWins = sum(Win) / length(Win),
            RunWins = 1 - CorpWins,
            Games = n(),
            Flatline = sum(Result == "FlatlineVictory" | Result == "Flatlined") / sum(!Win)
  )

run.half <- top.half %.% 
  group_by(RunID, Pack) %.%
  summarise(CorpWins = sum(Win) / length(Win),
            RunWins = 1 - CorpWins,
            Games = n(),
            Flatline = sum(Result == "FlatlineVictory" | Result == "Flatlined") / sum(!Win)
  )

run.sd <- top.sd %.% 
  group_by(RunID, Pack) %.%
  summarise(CorpWins = sum(Win) / length(Win),
            RunWins = 1 - CorpWins,
            Games = n(),
            Flatline = sum(Result == "FlatlineVictory" | Result == "Flatlined") / sum(!Win)
  )

run.2sd <- top.2sd %.% 
  group_by(RunID, Pack) %.%
  summarise(CorpWins = sum(Win) / length(Win),
            RunWins = 1 - CorpWins,
            Games = n(),
            Flatline = sum(Result == "FlatlineVictory" | Result == "Flatlined") / sum(!Win)
  )
