# Includes. Need to handle dates, split/apply data frames, compute player ratings from wins/losses, and 
# plot stuff. 
library(lubridate)
# library(plyr)
library(PlayerRatings)
library(dplyr)

# Deprecated functions. 
# source("FactionWinrates.R")
# source("WinRate.R")
# source("FlatlineWins.R")


#-----------------------------------------------------------------------------
# PERIOD SELECTION FOR GLICKO CALCULATION
#-----------------------------------------------------------------------------

period.select <- "week"

# Take the date floor of each period to divide the games up for Glicko. 
# Tested "month" before, now testing "week".

octgn.df$Period <- floor_date(octgn.df$GameStart, period.select)

#-----------------------------------------------------------------------------
# COMPUTING PLAYER RATINGS
#-----------------------------------------------------------------------------

# Use PlayerRatings package to compute Glicko rating for each player. Glicko requires the data to have:
# 1. Time block as numeric.
# 2. Numeric or character identifier for player one. 
# 3. Numeric or character identifier for player two.
# 4. The result of the game expressed as numeric -- 1 for P1 win, 0 for P2 win, 0.5 for draw. 

ratings <- select(octgn.df, Period, Corp_Player, Runner_Player, Win)

# Convert Win/Loss factor to 1/0. 
ratings$Win <- as.numeric(ratings$Win)

# Convert the Period "dates" to a factor and then to a numeric in order to pass them to glicko(). 
ratings$Period <- cut(ratings$Period, breaks = period.select)
ratings$Period <- as.numeric(ratings$Period)

# Note that per its creator, Glicko works best when each player is playing 5-10 games per rating period. 
ratings <- glicko(ratings, history = TRUE, sort = TRUE, init = c(1500, 350))

# Now I have ratings for each player in each period. 
player.ratings <- tbl_df(ratings$ratings)

# A quick review of the data shows that we have tons of people with the same ratings. 
# This is because we have many player IDs that have only played a few games, resulting in them getting
# the same rating. Note the large spike around 1500, the default rating. 
# summary(player.ratings$Rating)
# plot(player.ratings$Rating)
# hist(player.ratings$Rating, breaks = 100)
# str(summary(player.ratings$Rating))

# Prune to > 5 games played. 
# This actually eliminates about 50% of the remaining players, so it's pretty common for OCTGN
# players to try it a few times and then quit. 
player.ratings <- filter(player.ratings, Games >= 5)

# Store full deviation data for future use.
devplot.df <- player.ratings
save(devplot.df, file = 'devplot.Rda')

# Now to prune players by Deviation. This removes quite a few players! 
player.ratings <- filter(player.ratings, Deviation < 150)

# Take players who are more than 1 sd above the mean as skilled players. 
top.ratings <- filter(player.ratings, Rating >= mean(player.ratings$Rating) + sd(player.ratings$Rating))

# Store the pruned player ratings for future use. 
save(player.ratings, file = 'player-ratings.Rda')

# After all that, we still have over 70,000 games with these players in the original data. Nice. 
# Store this subset for future use as well. 
rated.games <- filter(octgn.df, Corp_Player %in% top.ratings$Player | Runner_Player %in% top.ratings$Player)
save(rated.games, file = 'rated-games.Rda')

# Compute win fraction for each ID in each period, and write those data frames out for future use. 
runwins.df <- rated.games %.% 
                group_by(RunID, Period) %.%
                summarise(Games = n(),
                Winrate = 1 - sum(Win) / Games
                         )

corpwins.df <- rated.games %.%
                group_by(CorpID, Period) %.%
                summarise(Games = n(),
                          Winrate = sum(Win) / Games
                          )

save(runwins.df, file = 'runwins.Rda')
save(corpwins.df, file = 'corpwins.Rda')