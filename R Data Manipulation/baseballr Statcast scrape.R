
# Simple Sabermetrics #

# baseballr: scrape Savant #

library(baseballr)

scrape1 <- scrape_statcast_savant(start_date = "2020-07-23", end_date = "2020-07-31", player_type = "pitcher")
scrape2 <- scrape_statcast_savant(start_date = "2020-08-01", end_date = "2020-08-07", player_type = "pitcher")
scrape3 <- scrape_statcast_savant(start_date = "2020-08-08", end_date = "2020-08-14", player_type = "pitcher")
scrape4 <- scrape_statcast_savant(start_date = "2020-08-15", end_date = "2020-08-21", player_type = "pitcher")
scrape5 <- scrape_statcast_savant(start_date = "2020-08-22", end_date = "2020-08-28", player_type = "pitcher")
scrape6 <- scrape_statcast_savant(start_date = "2020-08-29", end_date = "2020-09-04", player_type = "pitcher")
scrape7 <- scrape_statcast_savant(start_date = "2020-09-05", end_date = "2020-09-11", player_type = "pitcher")
scrape8 <- scrape_statcast_savant(start_date = "2020-09-12", end_date = "2020-09-18", player_type = "pitcher")
scrape9 <- scrape_statcast_savant(start_date = "2020-09-19", end_date = "2020-09-27", player_type = "pitcher")

statcast_data <- rbind(scrape1, scrape2, scrape3, scrape4, scrape5, scrape6, scrape7, scrape8, scrape9)