# First test plot. Jinteki PE game duration vs date. Have PE games gotten shorter as more tools have been 
# introduced? The answer appears to be no. 
qplot(octgn.df[octgn.df$CorpID == "Jinteki | Personal Evolution"
               & octgn.df$Duration <= 240
               & octgn.df$Duration > 0
               , ]$GameStart, # Variable
      octgn.df[octgn.df$CorpID == "Jinteki | Personal Evolution"
               & octgn.df$Duration <= 240
               & octgn.df$Duration > 0
               , ]$Duration, # Variable
      alpha = I(1/10)
) +
  xlab("Date") +
  ylab("Jinteki PE Game Duration") 


# Second plot is game duration vs date for NBN. When was AstroScript printed?
qplot(rated.games[(rated.games$CorpID == "NBN | Making News"
                 | rated.games$CorpID == "NBN | The World Is Yours")
                 & rated.games$Duration <= 240
                 & rated.games$Duration > 0
                   , ]$GameStart, # Variable
      rated.games[(rated.games$CorpID == "NBN | Making News"
                 | rated.games$CorpID == "NBN | The World Is Yours")
                 & rated.games$Duration <= 240
                 & rated.games$Duration > 0
                   , ]$Duration, # Variable
      alpha = I(2/15)
) +
  xlab("Date") +
  ylab("NBN Game Duration") 


# New idea: plot Jinteki flatline wins over time. But I need to obtain the win frequency over time. 
qplot(octgn.df[octgn.df$CorpID == "Jinteki | Personal Evolution"
               & octgn.df$Duration <= 240
               & octgn.df$Duration > 0
               , ]$GameStart, # Variable
      octgn.df[octgn.df$CorpID == "Jinteki | Personal Evolution"
               & octgn.df$Duration <= 240
               & octgn.df$Duration > 0
               , ]$Duration, # Variable
      alpha = I(1/10)
) +
  xlab("Date") +
  ylab("Jinteki PE Game Duration") 


# Next plot 
qplot(1:13, flatline.df[, 6],
      ylim = c(0, 1),
) +
  xlab("Month") +
  ylab("Jinteki PE % Flatline Wins") 