# Script file to calculate matchup winrates for each identity. 

require(dplyr)

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


# Need to match the versions in this df to the versions in by.period.df. 
# Follow this form:
#
# substr(octgn.df$Version[1], 1, 3)
# 
# For each 1:3 chars in by.period.df$Version, I want to match with data.packs$Version and populate a new
# column in by.period.df with the associated data pack. 
#
# Write a simple match function to take a character string of the form "x.x" and return the most recent
# data pack. 

CheckPack <- function(x, data.packs) {
  return( data.packs$Pack[match( substr(x, 1, 3), data.packs$Version )] )
}

rated.games <- mutate(rated.games, Pack = CheckPack(rated.games$Version, data.packs))

# I checked out the games with Pack = NA; they're simply a small subset of games (about 300) where
# the version number was either mangled or missing. No problem to simply drop them. 
rated.games <- filter(rated.games, Pack %in% levels(rated.games$Pack))

# Calculate winrates for each ID by matchup, and write the data out for future use. 
matchups.df <- rated.games %.%
                  group_by(CorpID, RunID, Pack) %.%
                  summarise(CorpWins = sum(Win) / length(Win),
                            RunWins = 1 - CorpWins,
                            Games = n()
                            )

save(matchups.df, file = 'matchups.Rda')

