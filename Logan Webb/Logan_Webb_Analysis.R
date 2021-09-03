
##### ~ Simple Sabermetrics - Logan Webb Article ~ #####

library(dplyr)
library(tidyr)
library(htmltools)
library(ggplot2)
library(ggrepel)
library(reactable)
library(sysfonts)
library(showtext)

webb <- read.csv("logan_webb_statcast_data.csv")

# Fix 'pitch_type' column
names(webb)[1] <- "pitch_type"

# Rename 2-Seam Fastballs to Sinkers
webb$pitch_name[webb$pitch_name == "2-Seam Fastball"] <- "Sinker"
webb$pitch_type[webb$pitch_type == "FT"] <- "SI"

# Factor pitch types
webb$pitch_name <- factor(webb$pitch_name, levels = c("4-Seam Fastball", "Sinker", "Cutter", "Slider", "Changeup"))
webb$pitch_type <- factor(webb$pitch_type, levels = c("FF", "SI", "FC", "SL", "CH"))

# Add Simple Sabermetric font
font_add("proxima_nova", regular = "Proxima Nova Condensed Black.otf")
showtext_auto()

# Apply Statcast pitch colors
sc_colors <- scale_color_manual(values = c("4-Seam Fastball" = "#ce2e48", "Sinker" = "#f49e0c", "Cutter" = "#904e3f", "Slider" = "#c4ba0f", "Changeup" = "#25c241"))

### Pitch Usage by Season

# Create table for visualization
seasonal_pitch_usage <- webb %>%
  group_by(game_year, pitch_name) %>%
  summarize(usage = n()) %>%
  group_by(game_year) %>%
  mutate(usage = round(usage/sum(usage)*100,1))

# Visualize Line Plot
ggplot(seasonal_pitch_usage, aes(x = game_year, y = usage, group = pitch_name, color = pitch_name)) + 
  geom_line(size = 2) + geom_point(size = 4, na.rm = TRUE) + 
  scale_x_continuous(limits = c(2019, 2021), breaks = c(2019, 2020, 2021), minor_breaks = c(2019, 2020, 2021)) + 
  scale_y_continuous(limits = c(0, 100),  breaks = c(0, 20, 40, 60, 80, 100), minor_breaks = seq(0, 100, 10)) +
  labs(y = "Pitch Usage (%)", color = " ", title = "Pitch Usage by Season") + sc_colors +
  theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, family = "proxima_nova")) +
  theme(text = element_text(size = 12, family = "proxima_nova"), panel.grid = element_line(colour = 'gray90', linetype = 'dashed')) +
  theme(legend.position = "bottom", axis.title.x = element_blank())

# Create table in wide format
seasonal_pitch_usage_wide <- seasonal_pitch_usage %>%
  mutate(usage = usage/100) %>%
  pivot_wider(names_from = "pitch_name", values_from = "usage")

# Fill Cutter NA with 0
seasonal_pitch_usage_wide$Cutter[is.na(seasonal_pitch_usage_wide$Cutter)] <- 0

# Create bar chart function for reactable
bar_chart <- function(label, width = "100%", height = "14px", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}

# Create reactable for pitch usage by season
reactable(seasonal_pitch_usage_wide, bordered = TRUE, highlight = TRUE,
          defaultColDef = colDef(align = "center", headerStyle = list(background = "#f7f7f8")),
          columns = list(
            game_year = colDef(name = "Season"),
            '4-Seam Fastball' = colDef(
              name = "4-Seam", 
              cell = function(value) {
                value <- paste0(format(value * 100, nsmall = 1, digits = 1), "%")
                bar_chart(value, width = value, fill = "#FD5A1E", background = "#e1e1e1")}
            ),
            'Sinker' = colDef(
              cell = function(value) {
                value <- paste0(format(value * 100, nsmall = 1, digits = 1), "%")
                bar_chart(value, width = value, fill = "#FD5A1E", background = "#e1e1e1")}
            ),
            'Slider' = colDef(
              cell = function(value) {
                value <- paste0(format(value * 100, nsmall = 1, digits = 1), "%")
                bar_chart(value, width = value, fill = "#FD5A1E", background = "#e1e1e1")}
            ),
            'Changeup' = colDef(
              cell = function(value) {
                value <- paste0(format(value * 100, nsmall = 1, digits = 1), "%")
                bar_chart(value, width = value, fill = "#FD5A1E", background = "#e1e1e1")}
            ),
            'Cutter' = colDef(
              cell = function(value) {
                value <- paste0(format(value * 100, nsmall = 1, digits = 1), "%")
                bar_chart(value, width = value, fill = "#FD5A1E", background = "#e1e1e1")}
            )
          )
)

### Pitch Location Plots

webb_two_seasons <- webb %>% filter(game_year != 2019, pitch_name %in% c("4-Seam Fastball", "Sinker"))

# Visualize pitch locations
ggplot(data = webb_two_seasons, aes(x = plate_x * -1, y = plate_z)) + 
  facet_grid(rows = vars(pitch_name), cols = vars(game_year)) +
  labs(x = "", y = "", color = "", title = paste("2020 & 2021 Fastball Pitch Locations")) +
  geom_point(aes(color = pitch_name), alpha = 0.7, size = 3, na.rm = TRUE) + sc_colors +
  geom_rect(aes(xmin = -.833, xmax = .833, ymin = 1.5, ymax = 3.5), alpha = 0, size = 1, color = "black") +
  geom_segment(aes(x = -0.75, y = 0.15, xend = 0.75, yend = 0.15), size = 1) + 
  geom_segment(aes(x = -0.75, y = 0.3, xend = -0.75, yend = 0.15), size = 1) +
  geom_segment(aes(x = -0.75, y = 0.3, xend = 0, yend = 0.5), size = 1) + 
  geom_segment(aes(x = 0, y = 0.5, xend = 0.75, yend = 0.3), size = 1) +
  geom_segment(aes(x = 0.75, y = 0.3, xend = 0.75, yend = 0.15), size = 1) + 
  scale_x_continuous(limits = c(-2.5, 2.5), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, 5), expand = c(0, 0)) +
  theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, family = "proxima_nova")) +
  theme(text = element_text(size = 12, family = "proxima_nova"), panel.grid = element_line(colour = 'gray90', linetype = 'dashed')) +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = "none")

# Visualize heatmap
ggplot(data = webb_two_seasons, aes(x = plate_x * -1, y = plate_z)) + 
  facet_grid(rows = vars(pitch_name), cols = vars(game_year)) +
  stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon", na.rm = TRUE) +
  labs(x = "", y = "", color = "", title = paste("2020 & 2021 Fastball Heatmaps")) +
  geom_rect(aes(xmin = -.833, xmax = .833, ymin = 1.5, ymax = 3.5), alpha = 0, size = 1, color = "black") +
  geom_segment(aes(x = -0.75, y = 0.15, xend = 0.75, yend = 0.15), size = 1) + 
  geom_segment(aes(x = -0.75, y = 0.3, xend = -0.75, yend = 0.15), size = 1) +
  geom_segment(aes(x = -0.75, y = 0.3, xend = 0, yend = 0.5), size = 1) + 
  geom_segment(aes(x = 0, y = 0.5, xend = 0.75, yend = 0.3), size = 1) +
  geom_segment(aes(x = 0.75, y = 0.3, xend = 0.75, yend = 0.15), size = 1) + 
  scale_fill_gradientn(colors = c("royalblue3", "white", "red2")) +
  scale_x_continuous(limits = c(-2.5, 2.5), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, 5), expand = c(0, 0)) +
  theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, family = "proxima_nova")) +
  theme(text = element_text(size = 12, family = "proxima_nova"), panel.grid = element_line(colour = 'gray90', linetype = 'dashed')) +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = "none")
