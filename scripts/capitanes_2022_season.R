library(dplyr)
library(scales)
library(gt)
library(tidyverse)
library(webshot2)
library(ggrepel)
library(ggthemes)
library(xkcd)
library(extrafont)
library(showtext)
library(patchwork)
library(ggimage)
library(ggtext)

asp_ratio <- 1.618

capitanes_22_season_pergame <- read.csv('data/capitanes_game.csv')
capitanes_22_playerpictures <- read.csv('data/capitanes_headshots.csv')
capitanes_22_season_perthirtysix <- read.csv('data/capitanes_thirtysix.csv')

capitanes_22_season_pergame <- merge(x = capitanes_22_season_pergame, y = capitanes_22_playerpictures, by = "Player") %>% 
  arrange(Rk)

capitanes_22_season_perthirtysix <- merge(x = capitanes_22_season_perthirtysix, y = capitanes_22_playerpictures, by = "Player") %>% 
  arrange(Rk)

pts_pergame <- ggplot(head(capitanes_22_season_pergame), aes(Player, PTS)) +
  geom_point() +
  geom_image(aes(image=player_headshot),size = 0.05, by = "width", asp = asp_ratio) +
  theme_minimal() +
  labs(x=NULL,
       y=NULL,
       title = "PUNTOS POR JUEGO TEMPORADA 2022-23",
       subtitle = "JUGADORES TOP 5",
       caption = "<br>**Data:** Data: basketball-reference.com (2023) | **Plot:** @el_dato_mx, @foreverpelon") +
  theme(
    #axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12),
    #text = element_text(family = "Chivo"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 10),
    plot.subtitle = element_text(size = 8, face = "bold"),
    plot.caption = element_markdown(size = 6),
    axis.text = element_text(size = 7, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(margin = margin(r = -25, unit = "pt")),
    axis.ticks.y = element_blank()
  )

tail_pts_pergame <- ggplot(tail(capitanes_22_season_pergame), aes(Player, PTS)) +
  geom_point() +
  geom_image(aes(image=player_headshot),size = 0.05, by = "width", asp = asp_ratio) +
  theme_minimal() +
  labs(x=NULL,
       y=NULL,
       title = "PUNTOS POR JUEGO TEMPORADA 2022-23",
       subtitle = "JUGADORES BOTTOM 5",
       caption = "<br>**Data:** Data: basketball-reference.com (2023) | **Plot:** @el_dato_mx, @foreverpelon") +
  theme(
    #axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12),
    #text = element_text(family = "Chivo"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 10),
    plot.subtitle = element_text(size = 8, face = "bold"),
    plot.caption = element_markdown(size = 6),
    axis.text = element_text(size = 7, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(margin = margin(r = -25, unit = "pt")),
    axis.ticks.y = element_blank()
  )

#pts_pergame + tail_pts_pergame

pts_perthirtysix <- ggplot(head(capitanes_22_season_perthirtysix), aes(Player, PTS)) +
  geom_point() +
  geom_image(aes(image=player_headshot),size = 0.05, by = "width", asp = asp_ratio) +
  theme_minimal() +
  labs(x=NULL,
       y=NULL,
       title = "PUNTOS POR 36MINS TEMPORADA 2022-23",
       subtitle = "JUGADORES TOP 5",
       caption = "<br>**Data:** Data: basketball-reference.com (2023) | **Plot:** @el_dato_mx, @foreverpelon") +
  theme(
    #axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12),
    #text = element_text(family = "Chivo"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 10),
    plot.subtitle = element_text(size = 8, face = "bold"),
    plot.caption = element_markdown(size = 6),
    axis.text = element_text(size = 7, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(margin = margin(r = -25, unit = "pt")),
    axis.ticks.y = element_blank()
  )

tail_pts_perthirtysix <- ggplot(tail(capitanes_22_season_perthirtysix), aes(Player, PTS)) +
  geom_point() +
  geom_image(aes(image=player_headshot),size = 0.05, by = "width", asp = asp_ratio) +
  theme_minimal() +
  labs(x=NULL,
       y=NULL,
       title = "PUNTOS POR 36MINS TEMPORADA 2022-23",
       subtitle = "JUGADORES BOTTOM 5",
       caption = "<br>**Data:** Data: basketball-reference.com (2023) | **Plot:** @el_dato_mx, @foreverpelon") +
  theme(
    #axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12),
    #text = element_text(family = "Chivo"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 10),
    plot.subtitle = element_text(size = 8, face = "bold"),
    plot.caption = element_markdown(size = 6),
    axis.text = element_text(size = 7, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(margin = margin(r = -25, unit = "pt")),
    axis.ticks.y = element_blank()
  )


(pts_pergame + tail_pts_pergame) / (pts_perthirtysix + tail_pts_perthirtysix)

