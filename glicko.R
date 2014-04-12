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
ratings$Win <- ratings$Win - 1

# Convert the Period "dates" to a factor and then to a numeric in order to pass them to glicko(). 
ratings$Period <- cut(ratings$Period, breaks = "week")
ratings$Period <- as.numeric(ratings$Period)

# Note that per its creator, Glicko works best when each player is playing 5-10 games per rating period. 
ratings <- glicko(ratings, history = TRUE, sort = TRUE, init = c(1500, 350))

# Now I have ratings for each player in each period. 

# Eventually I could build this into a Shiny app. That could be amazing. 

player.ratings <- tbl_df(ratings$ratings)

# A quick review of the data shows that we have tons of people with the same ratings. 
# This is because we have many player IDs that have only played a few games, resulting in them getting
# the same rating. Note the large spike around 1500, the default rating. 
summary(player.ratings$Rating)
plot(player.ratings$Rating)
hist(player.ratings$Rating, breaks = 100)
str(summary(player.ratings$Rating))

# Prune to > 5 games played. 
# player.ratings <- player.ratings[player.ratings$Games >= 20, ]
player.ratings <- filter(player.ratings, Games >= 5)

# The plot looks smoother and the histogram looks like a normal distribution. 
summary(player.ratings$Rating)
plot(player.ratings$Rating)
hist(player.ratings$Rating, breaks = 100)

# Take the top quartile. Should be pretty skilled players. 
# top.ratings <- player.ratings[player.ratings$Rating >= summary(player.ratings$Rating)["3rd Qu."], ]
top.ratings <- filter(player.ratings, Rating >= summary(player.ratings$Rating)["3rd Qu."])

# Obviously non-normal now, since I've lopped off the bottom 3/4 of the data. 
summary(top.ratings$Rating)
plot(top.ratings$Rating)
hist(top.ratings$Rating, breaks = 100)

# Rating vs Games per Week. Not super informative. 
qplot(data = top.ratings, Games / 50, Rating)

# Ratings vs Deviation
qplot(data = top.ratings, Deviation, Rating)

# Histograms, compare top players to all players. 
qplot(data = player.ratings, Deviation, binwidth = 10)
qplot(data = top.ratings, Deviation, binwidth = 10)

summary(player.ratings$Deviation)
summary(top.ratings$Deviation)

# Yikes! Who the heck is this?!?
outlier1 <- player.ratings[player.ratings$Games > 2000, ]
outlier1 

# And this player must have wins against amazing opponents. 
# This is the sort of player we want to filter out due to high deviation, so the deviation threshold
# should be set at or below 150. 
outlier2 <- player.ratings[player.ratings$Rating > 2000, ]
outlier2

# Plot the ratings vs the player win/loss ratio. Looks about how we'd expect; ratings flatten out. 
qplot(data = player.ratings, Win/Loss, Rating)

# Plot ratings vs win percentage. Straight line, but with quite a bit of noise. 
qplot(data = player.ratings, Win / (Win + Loss), Rating)

# R-squared of 0.7685. So win/loss is a decent predictor of Glicko, but not perfect. 
player.ratings <- mutate(player.ratings, Percentage = Win / (Win + Loss))
pred.lm <- lm(Rating ~ Percentage, player.ratings)
summary(pred.lm)

# Now to prune players by Deviation. This removes quite a few players! 
player.ratings <- filter(player.ratings, Deviation < 150)
top.ratings <- filter(top.ratings, Deviation < 150)

# After all that, though, I still have almost 80,000 games with these players in the original data. Nice. 
rated.games <- filter(octgn.df, Corp_Player %in% top.ratings$Player | Runner_Player %in% top.ratings$Player)

winrates.list <- FactionWinrates(rated.games)

runwins.df <- data.frame(winrates.list[1])
corpwins.df <- data.frame(winrates.list[2])


# Fix the name formatting. 
names(corpwins.df) <- levels(octgn.df$CorpID)
names(runwins.df) <- levels(octgn.df$RunID)


# Assign the year-month pairings to a df and then append that to the winrate data. 
dates <- data.frame(cbind(1:13))
names(dates) <- "Dates"
for (i in 1:12) { dates[i, 1] <- paste("2013-", i, sep = "") }
dates[13, 1] <- "2014-01"



corpwins.df$dates <- dates
runwins.df$dates <- dates


# Next step is to validate Glicko parameter selections by predicting player wins. 
# Note that Glicko works best when each player plays 5-10 games per period... need to look at average
# number of games by player per period. 

# Testing how to subset the data by date. Lubridate makes it easy. Thanks, @hadleywickham!
# test <- rated.games[rated.games$Period == ymd("2014-01-01"), ]
