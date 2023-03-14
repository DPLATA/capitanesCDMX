library(dplyr)
library(ggplot2)
library(ggtext)

dictionary_of_events <- read.csv('data/capitanes_12mar23_vs_bulls/dictionary_of_events.csv')

list_csv_files <- list.files(path = "data/games_pbp/")
season_games = do.call(rbind, lapply(list_csv_files, function(x) read.csv(glue::glue("data/games_pbp/{x}"), stringsAsFactors = FALSE)))

season_games_first_three_quarters <- filter(season_games, (PERIOD == 1 | PERIOD == 2 | PERIOD == 3) & (PLAYER1_TEAM_NICKNAME == 'Capitanes'))
season_games_fourth_quarters <- filter(season_games, PERIOD == 4 & PLAYER1_TEAM_NICKNAME == 'Capitanes')

filtered_misses_first_three_quarters <- filter(season_games_first_three_quarters, grepl('MISS', HOMEDESCRIPTION)| grepl('MISS', VISITORDESCRIPTION))
filtered_misses_fourth_quarters <- filter(season_games_fourth_quarters, grepl('MISS', HOMEDESCRIPTION) | grepl('MISS', VISITORDESCRIPTION))

miss_plot_first_three_quarters <- ggplot(filtered_misses_first_three_quarters, aes(x=PLAYER1_NAME)) + 
  geom_bar(fill = "#F9EB44") +
  theme_bw() +
  labs(x=NULL,
       y=NULL,
       title = "MISSED SHOTS",
       subtitle = "FIRST THREE QUARTERS SEASON 2022 - 2023",
       caption = "<br>**Data:** Data: NBA.com | **Plot:** DIEGO PLATA") +
  theme(
    plot.title = element_text(face = "bold", size = 10, color = "#F9EB44"),
    plot.subtitle = element_text(size = 8, face = "bold", color = "#F9EB44"),
    plot.caption = element_markdown(size = 6, color = "#F9EB44"),
    panel.background = element_rect(fill = "#3A4C98"),
    plot.background = element_rect(fill = "#1F2B60"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 7, face = "bold", color = "#F9EB44"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank()
  )


miss_plot_fourth_quarters <- ggplot(filtered_misses_fourth_quarters, aes(x=PLAYER1_NAME)) + 
  geom_bar(fill = "#F9EB44") +
  theme_bw() +
  labs(x=NULL,
       y=NULL,
       title = "MISSED SHOTS",
       subtitle = "FOURTH QUARTERS SEASON 2022 - 2023",
       caption = "<br>**Data:** Data: NBA.com | **Plot:** DIEGO PLATA") +
  theme(
    plot.title = element_text(face = "bold", size = 10, color = "#F9EB44"),
    plot.subtitle = element_text(size = 8, face = "bold", color = "#F9EB44"),
    plot.caption = element_markdown(size = 6, color = "#F9EB44"),
    panel.background = element_rect(fill = "#3A4C98"),
    plot.background = element_rect(fill = "#1F2B60"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 7, face = "bold", color = "#F9EB44"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank()
  )

miss_plot_first_three_quarters
miss_plot_fourth_quarters

filtered_misses_first_three_quarters$EVENTMSGACTIONTYPE_STRING <- dictionary_of_events$EVENTMSGACTIONTYPE_STRING[match(filtered_misses_first_three_quarters$EVENTMSGACTIONTYPE, dictionary_of_events$EVENTMSGACTIONTYPE)]
filtered_misses_fourth_quarters$EVENTMSGACTIONTYPE_STRING <- dictionary_of_events$EVENTMSGACTIONTYPE_STRING[match(filtered_misses_fourth_quarters$EVENTMSGACTIONTYPE, dictionary_of_events$EVENTMSGACTIONTYPE)]


miss_plot_by_type_first_three_quarters <- ggplot(filtered_misses_first_three_quarters, aes(x=EVENTMSGACTIONTYPE_STRING)) + 
  geom_bar(fill = "#F9EB44") +
  theme_bw() +
  labs(x=NULL,
       y=NULL,
       title = "MISSED SHOTS BY TYPE",
       subtitle = "FIRST THREE QUARTERS SEASON 2022 - 2023",
       caption = "<br>**Data:** Data: NBA.com | **Plot:** DIEGO PLATA") +
  theme(
    plot.title = element_text(face = "bold", size = 10, color = "#F9EB44"),
    plot.subtitle = element_text(size = 8, face = "bold", color = "#F9EB44"),
    plot.caption = element_markdown(size = 6, color = "#F9EB44"),
    panel.background = element_rect(fill = "#3A4C98"),
    plot.background = element_rect(fill = "#1F2B60"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 7, face = "bold", color = "#F9EB44"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank()
  )
miss_plot_by_type_fourth_quarters <- ggplot(filtered_misses_fourth_quarters, aes(x=EVENTMSGACTIONTYPE_STRING)) + 
  geom_bar(fill = "#F9EB44") +
  theme_bw() +
  labs(x=NULL,
       y=NULL,
       title = "MISSED SHOTS BY TYPE",
       subtitle = "FOURTH QUARTERS SEASON 2022 - 2023",
       caption = "<br>**Data:** Data: NBA.com | **Plot:** DIEGO PLATA") +
  theme(
    plot.title = element_text(face = "bold", size = 10, color = "#F9EB44"),
    plot.subtitle = element_text(size = 8, face = "bold", color = "#F9EB44"),
    plot.caption = element_markdown(size = 6, color = "#F9EB44"),
    panel.background = element_rect(fill = "#3A4C98"),
    plot.background = element_rect(fill = "#1F2B60"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 7, face = "bold", color = "#F9EB44"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank()
  )

miss_plot_by_type_first_three_quarters
miss_plot_by_type_fourth_quarters
