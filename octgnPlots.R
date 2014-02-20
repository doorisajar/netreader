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
qplot(octgn.df[(octgn.df$CorpID == "NBN | Making News"
               | octgn.df$CorpID == "NBN | The World Is Yours")
               & octgn.df$Duration <= 240
               & octgn.df$Duration > 0
               , ]$GameStart, # Variable
      octgn.df[(octgn.df$CorpID == "NBN | Making News"
               | octgn.df$CorpID == "NBN | The World Is Yours")
               & octgn.df$Duration <= 240
               & octgn.df$Duration > 0
               , ]$Duration, # Variable
      alpha = I(1/15)
) +
  xlab("Date") +
  ylab("NBN Game Duration") 

# New idea: plot Jinteki flatline wins over time. But I need to obtain the win frequency over time. 

