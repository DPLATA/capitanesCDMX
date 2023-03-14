library(ggtext)
library(dplyr)
library(ggplot2)

advanced_boxscore <- read.csv('data/capitanes_12mar23_vs_bulls/boxscore_advanced.csv')

fourfactors_boxscore <- read.csv('data/capitanes_12mar23_vs_bulls/boxscore_fourfactors.csv')

misc_boxscore <- read.csv('data/capitanes_12mar23_vs_bulls/boxscore_misc.csv')

scoring_boxscore <- read.csv('data/capitanes_12mar23_vs_bulls/boxscore_scoring.csv')

tracking_boxscore <- read.csv('data/capitanes_12mar23_vs_bulls/boxscore_tracking.csv')

traditional_boxscore <- read.csv('data/capitanes_12mar23_vs_bulls/boxscore_traditional.csv')

usage_boxscore <- read.csv('data/capitanes_12mar23_vs_bulls/boxscore_usage.csv')


pbp <- read.csv('data/capitanes_12mar23_vs_bulls/pbp.csv')
pbp_fourth <- read.csv('data/capitanes_12mar23_vs_bulls/pbp_4th.csv')

dictionary_of_events <- read.csv('data/capitanes_12mar23_vs_bulls/dictionary_of_events.csv')

fil_pbp_miss <- filter(pbp, grepl('MISS', HOMEDESCRIPTION))
fil_pbp_fourth_miss <- filter(pbp_fourth, grepl('MISS', HOMEDESCRIPTION))

#fil_pbp_fourth_pts <- filter(pbp_fourth, grepl('PTS', HOMEDESCRIPTION))

miss_plot_fourth <- ggplot(fil_pbp_fourth_miss, aes(x=PLAYER1_NAME)) + 
  geom_bar()
miss_plot <- ggplot(fil_pbp_miss, aes(x=PLAYER1_NAME)) + 
  geom_bar()

fil_pbp_miss$EVENTMSGACTIONTYPE_STRING <- dictionary_of_events$EVENTMSGACTIONTYPE_STRING[match(fil_pbp_miss$EVENTMSGACTIONTYPE, dictionary_of_events$EVENTMSGACTIONTYPE)]
fil_pbp_fourth_miss$EVENTMSGACTIONTYPE_STRING <- dictionary_of_events$EVENTMSGACTIONTYPE_STRING[match(fil_pbp_fourth_miss$EVENTMSGACTIONTYPE, dictionary_of_events$EVENTMSGACTIONTYPE)]
miss_plot_type <- ggplot(fil_pbp_miss, aes(x=EVENTMSGACTIONTYPE_STRING)) + 
  geom_bar(fill = "#F9EB44") +
  theme_bw() +
  labs(x=NULL,
       y=NULL,
       title = "Alfonzo McKinnie #28",
       subtitle = "Shot Chart 2022 - 2023",
       caption = "<br>**Data:** Data: NBA.com | **Plot:** @el_dato_mx, @foreverpelon") +
  theme(
    plot.title = element_text(face = "bold", size = 10, color = "#F9EB44"),
    plot.subtitle = element_text(size = 8, face = "bold", color = "#F9EB44"),
    plot.caption = element_markdown(size = 6, color = "#F9EB44"),
    panel.background = element_rect(fill = "#3A4C98"),
    plot.background = element_rect(fill = "#1F2B60"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 7, face = "bold", color = "#F9EB44"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank()
  )
  
miss_plot_type_fourth <- ggplot(fil_pbp_fourth_miss, aes(x=EVENTMSGACTIONTYPE_STRING)) + 
  geom_bar(fill = "#FF6666")

miss_plot_fourth
miss_plot
miss_plot_type_fourth
miss_plot_type


fil_pbp_fourth_miss$EVENTMSGACTIONTYPE_STRING <- dictionary_of_events$EVENTMSGACTIONTYPE_STRING[match(fil_pbp_fourth_miss$EVENTMSGACTIONTYPE, dictionary_of_events$EVENTMSGACTIONTYPE)]

pts_plot <- ggplot(fil_pbp_fourth_pts, aes(x=PLAYER1_NAME)) + 
  geom_bar()
pts_plot <- ggplot(fil_pbp_fourth_miss, aes(x=EVENTMSGACTIONTYPE_STRING)) + 
  geom_bar()

pts_plot
