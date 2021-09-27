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
  
  ### SCRAPE AND IMPORT DATASET FROM football-data.co.uk ----------------------------------------
  website <- "http://www.football-data.co.uk/mmz4281/"
  seasons <- paste0(substr(season,3,4), substr(season,6,7),'/')
  premier_league <- "/E0.csv"
  url <- read_csv(url(paste0(website, seasons, premier_league)))
  
  ### CONVERT AND FILTER DATE MM/DD/YY
  date_mdy <- as.Date(date, format = '%m/%d/%Y')
  
  epl_date <- url %>%
    select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)%>%
    filter(Date <= date_mdy) 
  
  ### CALCULATE POINTS FOR HOME AND AWAY ---------------------------------------------------------
  epl_points <- epl_date %>%
    mutate(
      win_home =  ifelse(FTR == 'H', 1, 0), # 1 Point for Home
      lose_home = ifelse(FTR == 'A', 1, 0), # 1 Point for Away if home loses
      
      win_away =  ifelse(FTR == 'A', 1, 0), # 1 Point for Away
      lose_away = ifelse(FTR == 'H', 1, 0), # 1 Point for Home if away loses
      
      draw = ifelse(FTR == 'D', 1, 0)
    ) 
  
  
  ### MAKE TWO DATAFRAMES FOR HOME AND AWAY TO JOIN THEM AND GROUP THEM IN A TEAMNAME-----------
  home_epl <- epl_points %>%
    select(Date, HomeTeam, FTHG, FTAG, FTR, win_home, lose_home, draw) %>%
    group_by(TeamName = HomeTeam) %>%  
    summarise(
      counts_home = n(),  
      wins_home = sum(win_home),  
      losses_home = sum(lose_home),  
      draws_home = sum(draw),  
      goals_for_home = sum(FTHG),
      goals_against_home = sum(FTAG)
    )
  
  away_epl <- epl_points %>%
    select(Date, AwayTeam, FTHG, FTAG, FTR, win_away, lose_away, draw) %>%
    group_by(TeamName = AwayTeam) %>% 
    summarise(
      counts_away = n(), 
      wins_away = sum(win_away), 
      losses_away = sum(lose_away), 
      draws_away = sum(draw),  
      goals_for_away = sum(FTAG),  
      goals_against_away = sum(FTHG)
    ) 
  
  ### LAST 10 AND STREAK ---------------------------------------------------------------------------
  HomeTen <- epl_points %>%
    select(Date, TeamName = HomeTeam, Win_t = win_home, Loss_t = lose_home, Draw_t = draw)
  AwayTen <- epl_points %>%
    select(Date, TeamName = AwayTeam, Win_t = win_away, Loss_t = lose_away, Draw_t = draw)
  
  # FILTER LAST TEN GAMES using top_n  -------
  LastTen_Total <- rbind(HomeTen, AwayTen) %>%
    group_by(TeamName) %>%
    top_n(10, wt=Date) %>% 
    summarise(
      Win_Ten = sum(Win_t),
      Loss_Ten = sum(Loss_t),
      Draw_Ten = sum(Draw_t)
    ) %>%
    mutate(
      Last10 = paste(Win_Ten, Loss_Ten, Draw_Ten, sep='-')
    ) 
  
  # STREAK USING LAG FUNCTION ---
  Streak_Total <- rbind(HomeTen, AwayTen) %>%
    mutate(
      result = ifelse(Win_t  == 1, "W", NA),
      result = ifelse(Loss_t == 1, "L", result),
      result = ifelse(Draw_t == 1, "D", result)) %>%
    group_by(TeamName) %>%
    mutate(
      result_lag = ifelse(result == lag(result), 1, 0)
    ) %>%
    summarise(
      streak_type = result[min(row_number())],
      streak_len = min(which(result_lag == 0)) - 1
    ) %>%
    mutate(
      Streak = paste0(streak_type, streak_len)
    ) 
  
  ### JOIN DATAFRAMES ----------------------------------------------------------------------------
  
  ultimate_join <- home_epl %>%
    inner_join(away_epl, by = 'TeamName')%>%
    inner_join(LastTen_Total, by = 'TeamName') %>%
    inner_join(Streak_Total, by = 'TeamName')
  
  ################################################################################################
  
  ### PERFORM CALCULATION ON HOME AND AWAY -------------------------------------------------------
  epl <- ultimate_join %>%
    mutate(
      
      WinsHA = wins_home + wins_away,
      LosesHA = losses_home + losses_away, 
      DrawsHA = draws_home + draws_away, 
      
      # Record as wins-loses-ties (Record) 
      Record = paste(WinsHA, LosesHA, DrawsHA, sep = "-"), 
      
      HomeRec = paste(wins_home, losses_home, draws_home, sep = "-"), # home record
      AwayRec = paste(wins_away, losses_away, draws_away, sep = "-"), # away record
      
      # Count of Matches Played
      MatchesPlayed = (counts_home + counts_away),
      
      #Multiply with 3 
      Points = ((3*WinsHA) + DrawsHA),
      
      #Calculate Points Per Match
      PPM = (Points / MatchesPlayed),
      
      #Calculate Point Percentate
      PtPct = (Points / (3 * MatchesPlayed)),
      
      #Goals Scored 
      GS = (goals_for_home + goals_for_away), 
      
      #Goals scored per match (GSM)
      GSM = (GS / MatchesPlayed), 
      
      #Goals Allowed
      GA = (goals_against_home + goals_against_away),
      
      #Goals allowed per match 
      GAM = (GA / MatchesPlayed)
    )
  
  league <- epl %>%
    arrange(desc(PPM), desc(WinsHA), desc(Points), desc(GSM), GAM)  %>%
    select(TeamName, Record, HomeRec, AwayRec, MatchesPlayed, Points, PPM, PtPct, GS, GSM, GA, GAM, Last10, Streak)
  
  return(league)
  
  
} # END EPL_STANDING FUNCTION

socceR <- EPL_Standings("07/15/2021", "2020/21")

view(socceR)


