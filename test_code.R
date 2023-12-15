#test

library(rsconnect)
library(ggplot2)
library(tidyverse)
library(janitor)
library(lubridate)
library(jsonlite)
library(dplyr)
library(scales)
library(httr)
library(purrr)
library(rvest)
library(furrr)


x <- Sys.Date()

url_reg <- "https://lscluster.hockeytech.com/feed/?feed=modulekit&view=statviewtype&type=topscorers&key=2976319eb44abe94&fmt=json&client_code=ohl&lang=en&league_code=&season_id=76&first=0&limit=50000&sort=active&stat=all&order_direction="


# use jsonlite::fromJSON to handle NULL values
json_data <- jsonlite::fromJSON(url_reg, simplifyDataFrame = TRUE)



# create data frame
df <- json_data[["SiteKit"]][["Statviewtype"]] %>%
  select(rank, player_id:num_teams) %>% 
  select(-c(birthtown, birthprov, birthcntry,
            loose_ball_recoveries, caused_turnovers, turnovers,
            phonetic_name, last_years_club, suspension_games_remaining,
            suspension_indefinite)) %>%
  mutate(player_id = as.numeric(player_id)) %>%
  mutate(across(active:age, ~as.numeric(.))) %>% 
  mutate(across(rookie:jersey_number, ~as.numeric(.))) %>% 
  mutate(team_id = as.numeric(team_id)) %>% 
  mutate(across(games_played:faceoff_pct, ~as.numeric(.))) %>%
  mutate(across(shots_on:num_teams, ~as.numeric(.))) %>% 
  mutate(birthdate_year = stringr::str_split(birthdate_year,
                                             "\\'", simplify = TRUE, n = 2)[,2]) %>% 
  mutate(birthdate_year = as.numeric(birthdate_year)) %>% 
  mutate(birthdate_year = 2000 + birthdate_year)



# create data frame with columns required for tableau viz
LeagueStats_2024 <- df %>% 
  select(Name = "name",
         Rookie = "rookie",
         JN = "jersey_number",
         BD = "birthdate",
         BD_Y = "birthdate_year",
         Hgt = "height",
         Wgt = "weight",
         Pos = "position",
         Team = "team_name",
         GP = "games_played",
         G = "goals",
         A = "assists",
         PTS = "points",
         `Pts/G` = "points_per_game",
         `+/-` = "plus_minus",
         PPG = "power_play_goals",
         PPA = "power_play_assists",
         SHG = "short_handed_goals",
         SHA = "short_handed_assists",
         SHPTS = "short_handed_points",
         GWG = "game_winning_goals",
         ENG = "empty_net_goals",
         PIM = "penalty_minutes",
         Active = "active") %>% 
  filter(Active == 1)