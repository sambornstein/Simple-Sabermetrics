# Simple Sabermetrics #

#install.packages("dplyr")
library(dplyr)

#setwd("~/BASEBALL/Simple Sabermetrics")
setwd("YOUR FILE PATH HERE")
statcast_data <- read.csv("mlb_2020_statcast_pitcher.csv")

#######################################################################

## Using filter() ##

NL_ROY <- statcast_data %>% 
  filter(player_name == "Devin Williams")

NL_CY <- statcast_data %>%
  filter(player_name %in% c("Trevor Bauer", "Jacob deGrom", "Yu Darvish"))


## Using select() ##

ROY_pitch_info <- NL_ROY %>%
  select(player_name, pitch_type, release_speed, release_spin_rate)

ROY_columns <- NL_ROY %>%
  select(player_name, pitch_type:release_speed)


## Using arrange() ##

ROY_release_speed <- NL_ROY %>%
  select(player_name, pitch_type, release_speed, release_spin_rate) %>%
  arrange(desc(release_speed))


## Tying together filter(), select(), arrange() ##

ROY_FF <- NL_ROY %>%
  select(player_name, pitch_type, release_speed, release_spin_rate) %>%
  filter(pitch_type == "FF") %>%
  arrange(desc(release_speed))

#######################################################################
  
## group_by() / summarize() ##

ROY_pitch_count <- NL_ROY %>%
  group_by(pitch_type) %>%
  summarize('pitch_count' = n())

ROY_avg_velos <- NL_ROY %>%
  group_by(pitch_type) %>%
  summarize('pitch_count' = n(),
            'average_velocity' = mean(release_speed, na.rm = TRUE))

ROY_range_velos <- NL_ROY %>%
  group_by(pitch_type) %>%
  summarize('pitch_count' = n(),
            'min_velocity' = min(release_speed, na.rm = TRUE),
            'max_velocity' = max(release_speed, na.rm = TRUE))

CY_FF_ranks <- NL_CY %>%
  filter(pitch_type == "FF") %>%
  group_by(player_name, pitch_type) %>%
  summarize('average_velocity' = mean(release_speed, na.rm = TRUE),
            'average_spin' = mean(release_spin_rate, na.rm = TRUE)) %>%
  mutate('bauer_units' = round(average_spin/average_velocity,1))

