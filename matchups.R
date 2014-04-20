# Script file to calculate matchup winrates for each identity. 

require(dplyr)

# Calculate winrates for each ID by matchup, and write the data out for future use. 
matchups.df <- rated.games %.%
                  group_by(CorpID, RunID, Pack) %.%
                  summarise(CorpWins = sum(Win) / length(Win),
                            RunWins = 1 - CorpWins,
                            Games = n()
                            )

save(matchups.df, file = 'matchups.Rda')

