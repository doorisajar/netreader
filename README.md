**Android: Netrunner OCTGN Analysis**

These script files parse and transform Netrunner OCTGN data. 

*Usage*

Note the following required packages:

dplyr 0.1.3+
lubridate 1.3.0+
ggplot2 0.9.3.1+
PlayerRatings 1.0+

In addition, an Android: Netrunner OCTGN CSV data file is required in your working directory, and you will need to read it and name it octgn.df prior to executing the script. An example line is provided as a comment at the start of octgn.R for this purpose. 

*octgn.R*

* Removes games where:
  * Runner deck size was less than 45 cards.
  * Corp deck size was less than 40 cards. 
  * Match duration was less than 0 minutes. 
  * Either player conceded. 
* Removes invalid IDs:
  * Laramy Fisk and The Collective.
  * Jinteki and Haas-Bioroid: Selective Mind-Mapping.
* Removes games with missing data:
  * Games with no version number (used to determine which data packs were valid). 

*glicko.R*

This script calculates a weekly Glicko rating for each player, then filters out players with < 5 games played or > 150 Glicko deviation. Players with final ratings more than one standard deviation above the mean are separated out as skilled players. 

*matchups.R*

This script calculates the winrates for each ID against each opposing side ID during each data pack window. 

*flatline.R*

This script is incomplete, but is structured to calculate flatlines as a share of Runner losses or Corporation wins across various player skill levels. 