"
Author: Asaad Khattab

English Premier League (EPL) 
http://www.football-data.co.uk/englandm.php

Features: 
#	Date (in day/month/year format) 
# Home Team
# Away Team
# FTHG – the number of goals scored by the home team
# FTAG – the number of goals scored by the away team
#	FTR – the result of the match (H indicates home team won, A indicates the away team won, D indicates a draw or tie)
"

EPL_Standings <- function(date, season){
  #Import Libraries 
  library(tidyverse)
  library(lubridate)
  library(dplyr)
  
}
  
socceR <- EPL_Standings("07/15/2021", "2020/21")

view(socceR)
