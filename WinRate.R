WinRate <- function(data, faction, ...) {
  # Computes the fraction of games passed in from OCTGN data that were won by the requested faction. 
  # 
  # Args:
  #   data: a data frame containing OCTGN data. 
  #   faction: a valid OCTGN faction string, e.g. "Jinteki | Personal Evolution"
  #   range: one of "month" "week" "day" "year" DELETE THIS IF I DON'T END UP USING IT
  #
  # Returns:
  #   The proportion of games in which one player used the requested faction that were won by that faction. 
  
  # Subset for games including the requested ID. 
  if ( faction %in% levels(data$CorpID) ) {
   sample <- data[data$CorpID==faction, ]
   return(sum(as.logical(sample$Win)) / nrow(sample))
  }
   
  else if ( faction %in% levels(data$RunID) ) {
    sample <- data[data$RunID==faction, ]
    return(1 - sum(as.logical(sample$Win)) / nrow(sample))
  }
  
  # Error handling:
  else stop("Faction ID not found in data.")
  
  # Compute the fraction of games that the reqested ID won. 

}
