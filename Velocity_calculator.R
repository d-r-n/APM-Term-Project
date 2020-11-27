### load packages
library(devtools)
library(dplyr)
library(gganimate)
library(ggforce)
library(ggplot2)
library(readr)

options(scipen=999)
# load helper functions
install.packages("patchwork")
source_url("https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/scripts/data_utils.R")
source_url("https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/scripts/plot_utils.R")

# fetch BAL highlight plays from the 2019 season
#highlights <- fetch_highlights_list(team_ = "BAL", season_ = 2019)

### fetch tracking data for play 242
#df <- fetch_df(playKey_ = 242)

#### Read in a team file
ARI_plays <- read_csv("/Users/alexmcgraw/Documents/Adv_Predictive_Modeling/Final_Presentation/team_plays/ARI_plays.csv", col_types = cols(X1 = col_skip()))
head(ARI_plays)
length(ARI_plays)
colnames(ARI_plays)

practice <- ARI_plays %>% filter(gameId == 2018090909, playId == 387)

velocities <- setNames(data.frame(matrix(ncol=5, nrow=0)), c("playId", "nflId", "frame", "v_x", "v_y"))

unique(ARI_plays$event)

#########################################################################################################
unique_games <- ARI_plays %>% unique(c=("gameId"))

for (val in unique_games) {
  plays <- ARI_plays %>% filter(gameId == val)
  unique_plays <- unique(plays$playId)
  
  for (play in unique_plays) {
    df <- ARI_plays %>% filter(gameId == val, playId == play)
    frames <- unique(df$frame)
    
    
    # * get play metadata ----
    play_dir <- df$playDirection %>% .[1]
    yards_togo <- df$yardsToGo %>% .[1]
    los <- df$absoluteYardlineNumber %>% .[1]
    togo_line <- if(play_dir=="left") los-yards_togo else los+yards_togo
    first_frame <- df %>%
      filter(event == "line_set" | event == "ball_snap") %>% 
      distinct(frame) %>% 
      slice_max(frame) %>% 
      pull()
    final_frame <- df %>% 
      filter(event == "tackle" | event == "touchdown" | event == "out_of_bounds" | event == 'pass_outcome_incomplete' | event == "pass_outcome_interception") %>% 
      distinct(frame) %>% 
      slice_max(frame) %>% 
      pull() + 10
    
    # * separate player and ball tracking data ----
    player_data <- df %>% 
      filter(between(frame, first_frame, final_frame)) %>% 
      select(playId, frame, homeTeamFlag, nflId, teamAbbr, displayName, jerseyNumber, position, positionGroup,
             x, y, s, o, dir, event) %>% 
      filter(displayName != "Football")
    
    
    #  velocity angle in radians
    player_data$dir_rad <- player_data$dir * pi / 180
    
    #  velocity components
    player_data$v_x <- sin(player_data$dir_rad) * player_data$s
    player_data$v_y <- cos(player_data$dir_rad) * player_data$s
    
    # adding data to "velocities" dataframe
    #vector <- c(player_data$nflId, player_data$v_x, player_data$v_y)
    velocities1 <- player_data[,c("playId", "nflId", "frame", "v_x", "v_y")]
    
    velocities <- rbind(velocities, velocities1)
    
    
  }
  
}