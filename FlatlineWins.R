FlatlineWinrates <- function (octgn, ...) {
  
  #-----------------------------------------------------------------------------
  # COMPUTING FLATLINE WIN RATES BY FACTION 
  #-----------------------------------------------------------------------------
  
  flatwins.df <- data.frame(matrix(nrow = length(levels(as.factor(as.character(octgn$Period)))), 
                                   ncol = length(levels(octgn$CorpID))))

  FlatRate <- function (octgn, faction, ...) {
    if ( faction %in% levels(octgn$CorpID) ) {
      sample <- octgn[octgn$CorpID == faction, ]
      
      wins <- sum(as.logical(sample$Win)) / nrow(sample)
    
      flatline.wins <- sum(as.logical(sample$Result == "FlatlineVictory" | sample$Result == "Flatlined")) / sum(as.logical(sample$Win))
  
    }
  }  
  
  for (i in 1:length(levels(octgn$CorpID))) {
    
    faction <- as.character(levels(octgn$CorpID))[i]
    flatwins.df[, i] <- ddply(octgn, .(Period), .progress = "text", FlatRate, faction)[, 2]
    names(flatwins.df)[i] <- faction
    
  }
  
  return(flatwins.df)
}