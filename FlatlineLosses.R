FlatlineLosses <- function (octgn, ...) {
  
  #-----------------------------------------------------------------------------
  # COMPUTING FLATLINE WIN RATES BY FACTION 
  #-----------------------------------------------------------------------------
  
  flatloss.df <- data.frame(matrix(nrow = length(levels(as.factor(as.character(octgn$Period)))), 
                                   ncol = length(levels(octgn$RunID))))
  
  FlatRate <- function (octgn, faction, ...) {
    if ( faction %in% levels(octgn$RunID) ) {
      sample <- octgn[octgn$RunID == faction, ]
            
      flatloss.games <- sum(as.logical(sample$Result == "FlatlineVictory" | sample$Result == "Flatlined")) / sum(as.logical(sample$Win))
      
    }
  }  
  
  for (i in 1:length(levels(octgn$RunID))) {
    
    faction <- as.character(levels(octgn$RunID))[i]
    flatloss.df[, i] <- ddply(octgn, .(Period), .progress = "text", FlatRate, faction)[, 2]
    names(flatloss.df)[i] <- faction
    
  }
  
  return(flatloss.df)
}