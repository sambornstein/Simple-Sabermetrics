# Simple Sabermetrics #

#install.packages("dplyr")
library(dplyr)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("ggpubr")
library(ggpubr)

setwd("~/BASEBALL/Simple Sabermetrics")
statcast_data <- read.csv("mlb_2020_statcast_pitcher.csv")

#######################################################################

NL_ROY <- statcast_data %>% 
  filter(player_name == "Devin Williams")

NL_CY <- statcast_data %>%
  filter(player_name %in% c("Trevor Bauer", "Jacob deGrom", "Yu Darvish"))

#######################################################################

##### Bar Chart #####

# Prepare dataset
FB_over_95 <- NL_CY %>%
  filter(pitch_type == "FF") %>%
  group_by(player_name) %>%
  summarize('95+' = sum(release_speed > 95, na.rm = TRUE))

# Basic Visual
ggplot(FB_over_95, aes(x = player_name, y = `95+`)) + 
  geom_bar(stat = "identity")

# Advanced Visual
ggplot(FB_over_95, aes(x = reorder(player_name, -`95+`), y = `95+`, fill = player_name)) + 
  geom_bar(stat = "identity") +
  labs(x = "Pitcher", y = "", title = "Number of fastballs over 95 MPH") +
  theme_bw() +
  theme(legend.position = "none")


##### Density Plot #####

# Prepare dataset
Fastballs_deGrom <- NL_CY %>%
  filter(player_name == "Jacob deGrom", pitch_type == "FF")
  
# Basic Visual
ggplot(Fastballs_deGrom, aes(x = release_speed)) +
  geom_density()

# Advanced Visual
ggplot(Fastballs_deGrom, aes(x = release_speed)) + 
  geom_density(color = "red", fill = "lightblue") +
  labs(x = "Velocity (MPH)", title = "Distribution of Fastball Velocity") +
  theme_bw()


##### Box Plot #####

# Prepare dataset
Fastballs_NL_CY <- NL_CY %>%
  filter(pitch_type == "FF")

# Basic Visual
ggplot(Fastballs_NL_CY, aes(x = player_name, y = release_speed)) +
  geom_boxplot()

# Advanced Visual
ggplot(Fastballs_NL_CY, aes(x = player_name, y = release_speed, fill = player_name)) +
  geom_boxplot(width = 0.5) + 
  labs(x = "Pitcher", y = "Velocity (MPH)", title = "Distribution of Fastball Velocity") +
  theme_bw() +
  theme(legend.position = "none")


##### Scatter Plot 1 #####

# Basic Visual
ggplot(NL_ROY, aes(x = release_speed, y = release_spin_rate, color = pitch_type)) +
  geom_point()

# Advanced Visual
ggplot(NL_ROY, aes(x = release_speed, y = release_spin_rate, color = pitch_type)) +
  geom_point(size = 2, na.rm = TRUE) +
  scale_x_continuous(limits = c(80,100), breaks = c(80,85,90,95,100)) +
  scale_y_continuous(limits = c(2000,3250), breaks = c(2000,2250,2500,2750,3000,3250)) +
  labs(x = "Velocity (MPH)", y = "Spin Rate (RPM)", color = "Pitch Type",
       title = "Devin Williams Velocity vs. Spin Rate") +
  theme_bw() +
  theme(legend.position = "bottom")


##### Scatter Plot 2 #####

# Prepare dataset
cor_df <- statcast_data %>%
  filter(pitch_type == "FF") %>%
  group_by(player_name, pitch_type) %>%
  summarize('n' = n(),
            'mean_release_speed' = mean(release_speed, na.rm = TRUE),
            'sw_str_percentage' = sum(description %in% c("swinging_strike", "swinging_strike_blocked"))/n()) %>%
  filter(n > 100, mean_release_speed > 85)

# Visual
ggscatter(data = cor_df, x = "mean_release_speed", y = "sw_str_percentage", 
          add = "reg.line", cor.coef = TRUE, cor.method = "pearson",
          conf.int = TRUE, add.params = list(color = "red", fill = "lightgray"),
          xlab = "Velocity (MPH)", ylab = "Swinging Strike %", 
          title = "Correlation: AVG FB Velocity vs. Sw. Strike %")


##### Line Plot #####

# Prepare dataset
NL_CY$game_date <- as.Date(NL_CY$game_date)
time_df <- NL_CY %>%
  filter(pitch_type == "FF") %>%
  group_by(player_name, pitch_type, game_date) %>%
  summarize('n' = n(),
            'mean_release_speed' = mean(release_speed, na.rm = TRUE))

# Basic Visual
ggplot(data = time_df, aes(x = game_date, y = mean_release_speed, group = player_name)) + 
  geom_line() + geom_point()

# Advanced Visual
ggplot(data = time_df, aes(x = game_date, y = mean_release_speed, group = player_name, color = player_name)) + 
  geom_line(size = 1) + geom_point(size = 3) +
  labs(y = "Velocity (MPH)", title = "Average FB Velocity by Game", color = "") +
  theme_bw() +
  theme(legend.position = "bottom", axis.title.x = element_blank())
  
