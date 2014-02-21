FactionWinrates <- function (octgn, ...) {

#-----------------------------------------------------------------------------
# COMPUTING WIN RATES BY FACTION 
#-----------------------------------------------------------------------------
  
  corpwins.df <- data.frame(matrix(nrow = length(levels(as.factor(as.character(octgn$Period)))), 
                                   ncol = length(levels(octgn$CorpID))))

# Iterate through Corp IDs and compute monthly winrate for each. 

#ctest <- group_by(octgn.df, Period)

for (i in 1:length(levels(octgn$CorpID))) {
    
    faction <- as.character(levels(octgn$CorpID))[i]
    corpwins.df[, i] <- ddply(octgn, .(Period), .progress = "text", WinRate, faction)[, 2]
    names(corpwins.df)[i] <- faction
    
  }
  
  
#  runwins.df <- cbind(1:13, 1:13)
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