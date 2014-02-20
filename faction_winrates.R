FactionWinrates <- function (octgn, ...) {

#-----------------------------------------------------------------------------
# COMPUTING WIN RATES BY FACTION 
#-----------------------------------------------------------------------------

# Okay, now I want this to become a function, so that I can call it on the results of glicko.R! 
# I think?

# Create a new column that consolidates the win/loss types into just 1/0. 
# ...er... there's already a true/false win column! Oops!

# octgn.df$CorpWin <- lapply(octgn.df$Result, 
#                           function(x) { 
#                                         if (x %in% c("AgendaDefeat","DeckDefeat","Conceded")) {0} 
#                                         else {1} 
#                                       } 
#                           )

# octgn.df$CorpWin <- as.numeric(octgn.df$CorpWin)

  # First test of using by to apply WinRate to octgn.df by a factor.
  octgn$YearMonth <- floor_date(octgn$GameStart, "month")
  
  corpwins.df <- data.frame(cbind(1:12, 1:13))
  
  # Iterate through Corp IDs and compute monthly winrate for each. 
  for (i in 1:length(levels(octgn.df$CorpID))) {
    
    faction <- as.character(levels(octgn.df$CorpID))[i]
    corpwins.df[, i] <- ddply(octgn.df, .(YearMonth), .progress = "text", WinRate, faction)[, 2]
    names(corpwins.df)[i] <- faction
    
  }
  
#  str(octgn$RunID)
  
  runwins.df <- data.frame(cbind(1:13, 1:13))
  
  # Iterate through Runner IDs and compute monthly winrate for each. 
  for (i in 1:length(levels(octgn$RunID))) {
    
    faction <- as.character(levels(octgn$RunID))[i]
    runwins.df[, i] <- ddply(octgn, .(YearMonth), .progress = "text", WinRate, faction)[, 2]
    names(runwins.df)[i] <- faction
    
  }
  
return (list(runwins.df, corpwins.df))
}