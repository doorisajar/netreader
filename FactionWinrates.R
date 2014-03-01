FactionWinrates <- function (octgn, ...) {
  # Computes the fraction of games passed in from OCTGN data that were won by each faction in that data. 
  # 
  # Args:
  #   data: a data frame containing OCTGN data and a "Period" column that breaks the games up into groups. 
  #
  # Returns:
  #   A list of two data frames: runwins.df containing Runner ID win rates for each period, and 
  #   corpwins.df, the same thing for the Corproration IDs. 
  
  corpwins.df <- data.frame(matrix(nrow = length(levels(as.factor(as.character(octgn$Period)))), 
                                   ncol = length(levels(octgn$CorpID))))

# Iterate through Corp IDs and compute monthly winrate for each. 

  # test <- select(octgn.df, -GameStart, -Period)

#  test <- octgn.df
#  test$GameStart <- as.factor(as.character(test$GameStart))
#  test$Period <- as.factor(as.character(test$Period))

#  ctest <- group_by(test, Period)
#  ctest <- group_by(test, CorpID)

#  test %.% 
#    group_by(Period) %.%
#    group_by(CorpID) %.%
#    summarise()

  for (i in 1:length(levels(octgn$CorpID))) {
    
    faction <- as.character(levels(octgn$CorpID))[i]
    corpwins.df[, i] <- ddply(octgn, .(Period), .progress = "text", WinRate, faction)[, 2]
    names(corpwins.df)[i] <- faction
    
  }
  
  
  runwins.df <- data.frame(matrix(nrow = length(levels(as.factor(as.character(octgn$Period)))), 
                                  ncol = length(levels(octgn$RunID))))

  # Iterate through Runner IDs and compute monthly winrate for each. 
  for (i in 1:length(levels(octgn$RunID))) {
    
    faction <- as.character(levels(octgn$RunID))[i]
    runwins.df[, i] <- ddply(octgn, .(Period), .progress = "text", WinRate, faction)[, 2]
    names(runwins.df)[i] <- faction
    
  }
  
return (list(runwins.df, corpwins.df))
}