#-----------------------------------------------------------------------------
# COMPUTING PLAYER RATINGS
#-----------------------------------------------------------------------------

# I'll need some player filtering. Options:
# Remove all games with 0 influence decks. (Easy.)
# Compute winrate for each player. (Moderate.)
# Compute ELO / Glicko for each player. (Hard.)

# Use PlayerRatings package to compute Glicko rating for each player. Glicko requires the data to have:
# 1. Time block as numeric, 2. Numeric or character identifier for player one, 3. Same for P2, 4. The
# result of the game expressed as numeric -- 1 for P1 win, 0 for P2 win, 0.5 for draw. 

ratings <- select(octgn.df, Period, Corp_Player, Runner_Player, Win)

# Convert Win/Loss factor to 1/0. 
ratings$Win <- as.numeric(ratings$Win)
ratings$Win <- ratings$Win - 1

# Convert the Period "dates" to a factor and then to a numeric in order to pass them to glicko(). 
ratings$Period <- cut(ratings$Period, breaks = "month")
ratings$Period <- as.numeric(ratings$Period)

# Note that per its creator, Glicko works best when each player is playing 5-10 games per rating period. 
ratings <- glicko(ratings, history = TRUE, sort = TRUE, init = c(1500, 350))

# Now I have ratings for each player in each month. 

# Eventually I could build this into a Shiny app. That could be amazing. 

player.ratings <- ratings$ratings

# A quick review of the data shows that we have tons of people with the same ratings. 
# This is because we have many player IDs that have only played a few games, resulting in them getting
# the same rating. Note the large spike around 1500, the default rating. 
summary(player.ratings$Rating)
plot(player.ratings$Rating)
hist(player.ratings$Rating, breaks = 100)
str(summary(player.ratings$Rating))

# Prune to > 20 games played. 
player.ratings <- player.ratings[player.ratings$Games >= 20, ]

# The plot looks smoother and the histogram looks like a normal distribution. 
summary(player.ratings$Rating)
plot(player.ratings$Rating)
hist(player.ratings$Rating, breaks = 100)

# Take the top quartile. Should be pretty skilled players. 
top.ratings <- player.ratings[player.ratings$Rating >= summary(player.ratings$Rating)["3rd Qu."], ]

# Obviously non-normal now, since I've lopped off the bottom 3/4 of the data. 
summary(top.ratings$Rating)
plot(top.ratings$Rating)
hist(top.ratings$Rating, breaks = 100)

qplot(data = top.ratings, Rating, Games)

# Yikes! Who the heck is this?!?
outlier1 <- player.ratings[player.ratings$Games > 2000, ]

# And this player must have wins against amazing opponents, since they have the best rating despite a 
# 23-37 record! 
outlier2 <- player.ratings[player.ratings$Rating > 2000, ]
qplot(data = player.ratings, Rating, Win/Loss)


# After all that, though, I still have 78,856 games with these players in the original data. Nice. 
rated.games <- octgn.df[with(octgn.df, 
                             Corp_Player %in% top.ratings$Player | Runner_Player %in% top.ratings$Player), ]

winrates.list <- FactionWinrates(rated.games)

runwins.df <- data.frame(winrates.list[1])
corpwins.df <- data.frame(winrates.list[2])


# Assign the year-month pairings to a df and then append that to the winrate data. 
dates <- data.frame(cbind(1:13))
names(dates) <- "Dates"
for (i in 1:12) { dates[i, 1] <- paste(c("2013-", i), collapse = " ") }
dates[13, 1] <- "2014-01"

corpwins.df$dates <- dates
runwins.df$dates <- dates



# Next step is to validate Glicko parameter selections by predicting player wins. 
# Note that Glicko works best when each player plays 5-10 games per period... need to look at average
# number of games by player per period. 

# Testing how to subset the data by date. Lubridate makes it easy. Thanks, @hadleywickham!
test <- rated.games[rated.games$Period == ymd("2014-01-01"), ]
