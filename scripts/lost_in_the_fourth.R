library(dplyr)
library(ggplot2)
library(ggtext)
library(patchwork)

#list_csv_files <- list.files(path = "data/games_pbp/lost_in_fourth/")
#meltdowns = do.call(rbind, lapply(list_csv_files, function(x) read.csv(glue::glue("data/games_pbp/lost_in_fourth/{x}"), stringsAsFactors = FALSE)))

data_dictionary <- read.csv('data/games_pbp/lost_in_fourth/data_dictionary.csv')
capitanes_hustle <- read.csv('data/games_pbp/lost_in_fourth/2022200046.csv')

# --------------------- FIRST QUARTER -----------------------------------------

capitanes_hustle_first_quarter_capitanes <- filter(capitanes_hustle, (PERIOD == 1 & PLAYER1_TEAM_CITY == "Mexico City"))
capitanes_hustle_first_quarter_capitanes_missed_fgs_list <- filter(capitanes_hustle_first_quarter_capitanes, EVENTMSGTYPE == 2)

capitanes_hustle_first_quarter_capitanes_total_fgs <- sum(capitanes_hustle_first_quarter_capitanes$EVENTMSGTYPE == 1) + sum(capitanes_hustle_first_quarter_capitanes$EVENTMSGTYPE == 2)
capitanes_hustle_first_quarter_capitanes_missed_fgs <- sum(capitanes_hustle_first_quarter_capitanes$EVENTMSGTYPE == 2)
capitanes_hustle_first_quarter_capitanes_missed_fgs_pctg <- (capitanes_hustle_first_quarter_capitanes_missed_fgs * 100) / (capitanes_hustle_first_quarter_capitanes_total_fgs)
capitanes_hustle_first_quarter_capitanes_made_fgs_pctg <- 100 - capitanes_hustle_first_quarter_capitanes_missed_fgs_pctg

made_miss_fg_pctg <- data.frame(
  category=c("MADE","MISS"),  
  value=c(capitanes_hustle_first_quarter_capitanes_made_fgs_pctg, capitanes_hustle_first_quarter_capitanes_missed_fgs_pctg)
)
made_miss_fg_pctg_plot <- ggplot(made_miss_fg_pctg, aes(x=category, y=value)) +
  geom_bar(stat = "identity", fill = "#F9EB44") +
  theme_bw() +
  labs(x=NULL,
       y=NULL,
       title = "FG PERCENTAGE",
       subtitle = "FIRST QUARTER VS MEMPHIS HUSTLE JANUARY 3 2023",
       caption = "<br>**Data:** Data: NBA.com | **Plot:** DIEGO PLATA") +
  scale_y_continuous(label = function(x) {return(paste(x, "%"))}) +
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
made_miss_fg_pctg_plot

miss_per_player <- capitanes_hustle_first_quarter_capitanes_missed_fgs_list %>% count(PLAYER1_NAME)
miss_per_player_plot <- ggplot(miss_per_player, aes(x=PLAYER1_NAME, y=n)) +
  geom_bar(stat = "identity", fill = "#F9EB44") +
  theme_bw() +
  labs(x=NULL,
       y=NULL,
       title = "FG MISSED BY PLAYER",
       subtitle = "FIRST QUARTER VS MEMPHIS HUSTLE JANUARY 3 2023",
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
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank()
  )
miss_per_player_plot

# --------------------- SECOND QUARTER -----------------------------------------

capitanes_hustle_second_quarter_capitanes <- filter(capitanes_hustle, (PERIOD == 2 & PLAYER1_TEAM_CITY == "Mexico City"))
capitanes_hustle_second_quarter_capitanes_missed_fgs_list <- filter(capitanes_hustle_second_quarter_capitanes, EVENTMSGTYPE == 2)

capitanes_hustle_second_quarter_capitanes_total_fgs <- sum(capitanes_hustle_second_quarter_capitanes$EVENTMSGTYPE == 1) + sum(capitanes_hustle_second_quarter_capitanes$EVENTMSGTYPE == 2)
capitanes_hustle_second_quarter_capitanes_missed_fgs <- sum(capitanes_hustle_second_quarter_capitanes$EVENTMSGTYPE == 2)
capitanes_hustle_second_quarter_capitanes_missed_fgs_pctg <- (capitanes_hustle_second_quarter_capitanes_missed_fgs * 100) / (capitanes_hustle_second_quarter_capitanes_total_fgs)
capitanes_hustle_second_quarter_capitanes_made_fgs_pctg <- 100 - capitanes_hustle_second_quarter_capitanes_missed_fgs_pctg

second_made_miss_fg_pctg <- data.frame(
  category=c("MADE","MISS"),  
  value=c(capitanes_hustle_second_quarter_capitanes_made_fgs_pctg, capitanes_hustle_second_quarter_capitanes_missed_fgs_pctg)
)
second_made_miss_fg_pctg_plot <- ggplot(second_made_miss_fg_pctg, aes(x=category, y=value)) +
  geom_bar(stat = "identity", fill = "#F9EB44") +
  theme_bw() +
  labs(x=NULL,
       y=NULL,
       title = "FG PERCENTAGE",
       subtitle = "SECOND QUARTER VS MEMPHIS HUSTLE JANUARY 3 2023",
       caption = "<br>**Data:** Data: NBA.com | **Plot:** DIEGO PLATA") +
  scale_y_continuous(label = function(x) {return(paste(x, "%"))}) +
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
second_made_miss_fg_pctg_plot

second_miss_per_player <- capitanes_hustle_second_quarter_capitanes_missed_fgs_list %>% count(PLAYER1_NAME)
second_miss_per_player_plot <- ggplot(second_miss_per_player, aes(x=PLAYER1_NAME, y=n)) +
  geom_bar(stat = "identity", fill = "#F9EB44") +
  theme_bw() +
  labs(x=NULL,
       y=NULL,
       title = "FG MISSED BY PLAYER",
       subtitle = "SECOND QUARTER VS MEMPHIS HUSTLE JANUARY 3 2023",
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
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank()
  )
second_miss_per_player_plot

# --------------------- THIRD QUARTER -----------------------------------------

capitanes_hustle_third_quarter_capitanes <- filter(capitanes_hustle, (PERIOD == 3 & PLAYER1_TEAM_CITY == "Mexico City"))
capitanes_hustle_third_quarter_capitanes_missed_fgs_list <- filter(capitanes_hustle_third_quarter_capitanes, EVENTMSGTYPE == 2)

capitanes_hustle_third_quarter_capitanes_total_fgs <- sum(capitanes_hustle_third_quarter_capitanes$EVENTMSGTYPE == 1) + sum(capitanes_hustle_third_quarter_capitanes$EVENTMSGTYPE == 2)
capitanes_hustle_third_quarter_capitanes_missed_fgs <- sum(capitanes_hustle_third_quarter_capitanes$EVENTMSGTYPE == 2)
capitanes_hustle_third_quarter_capitanes_missed_fgs_pctg <- (capitanes_hustle_third_quarter_capitanes_missed_fgs * 100) / (capitanes_hustle_third_quarter_capitanes_total_fgs)
capitanes_hustle_third_quarter_capitanes_made_fgs_pctg <- 100 - capitanes_hustle_third_quarter_capitanes_missed_fgs_pctg

third_made_miss_fg_pctg <- data.frame(
  category=c("MADE","MISS"),  
  value=c(capitanes_hustle_third_quarter_capitanes_made_fgs_pctg, capitanes_hustle_third_quarter_capitanes_missed_fgs_pctg)
)
third_made_miss_fg_pctg_plot <- ggplot(third_made_miss_fg_pctg, aes(x=category, y=value)) +
  geom_bar(stat = "identity", fill = "#F9EB44") +
  theme_bw() +
  labs(x=NULL,
       y=NULL,
       title = "FG PERCENTAGE",
       subtitle = "THIRD QUARTER VS MEMPHIS HUSTLE JANUARY 3 2023",
       caption = "<br>**Data:** Data: NBA.com | **Plot:** DIEGO PLATA") +
  scale_y_continuous(label = function(x) {return(paste(x, "%"))}) +
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
third_made_miss_fg_pctg_plot

third_miss_per_player <- capitanes_hustle_third_quarter_capitanes_missed_fgs_list %>% count(PLAYER1_NAME)
third_miss_per_player_plot <- ggplot(third_miss_per_player, aes(x=PLAYER1_NAME, y=n)) +
  geom_bar(stat = "identity", fill = "#F9EB44") +
  theme_bw() +
  labs(x=NULL,
       y=NULL,
       title = "FG MISSED BY PLAYER",
       subtitle = "THIRD QUARTER VS MEMPHIS HUSTLE JANUARY 3 2023",
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
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank()
  )
third_miss_per_player_plot

# --------------------- FOURTH QUARTER -----------------------------------------

capitanes_hustle_fourth_quarter_capitanes <- filter(capitanes_hustle, (PERIOD == 4 & PLAYER1_TEAM_CITY == "Mexico City"))
capitanes_hustle_fourth_quarter_capitanes_missed_fgs_list <- filter(capitanes_hustle_fourth_quarter_capitanes, EVENTMSGTYPE == 2)

capitanes_hustle_fourth_quarter_capitanes_total_fgs <- sum(capitanes_hustle_fourth_quarter_capitanes$EVENTMSGTYPE == 1) + sum(capitanes_hustle_fourth_quarter_capitanes$EVENTMSGTYPE == 2)
capitanes_hustle_fourth_quarter_capitanes_missed_fgs <- sum(capitanes_hustle_fourth_quarter_capitanes$EVENTMSGTYPE == 2)
capitanes_hustle_fourth_quarter_capitanes_missed_fgs_pctg <- (capitanes_hustle_fourth_quarter_capitanes_missed_fgs * 100) / (capitanes_hustle_fourth_quarter_capitanes_total_fgs)
capitanes_hustle_fourth_quarter_capitanes_made_fgs_pctg <- 100 - capitanes_hustle_fourth_quarter_capitanes_missed_fgs_pctg

fourth_made_miss_fg_pctg <- data.frame(
  category=c("MADE","MISS"),  
  value=c(capitanes_hustle_fourth_quarter_capitanes_made_fgs_pctg, capitanes_hustle_fourth_quarter_capitanes_missed_fgs_pctg)
)
fourth_made_miss_fg_pctg_plot <- ggplot(fourth_made_miss_fg_pctg, aes(x=category, y=value)) +
  geom_bar(stat = "identity", fill = "#F9EB44") +
  theme_bw() +
  labs(x=NULL,
       y=NULL,
       title = "FG PERCENTAGE",
       subtitle = "FOURTH QUARTER VS MEMPHIS HUSTLE JANUARY 3 2023",
       caption = "<br>**Data:** Data: NBA.com | **Plot:** DIEGO PLATA") +
  scale_y_continuous(label = function(x) {return(paste(x, "%"))}) +
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
fourth_made_miss_fg_pctg_plot

fourth_miss_per_player <- capitanes_hustle_fourth_quarter_capitanes_missed_fgs_list %>% count(PLAYER1_NAME)
fourth_miss_per_player_plot <- ggplot(fourth_miss_per_player, aes(x=PLAYER1_NAME, y=n)) +
  geom_bar(stat = "identity", fill = "#F9EB44") +
  theme_bw() +
  labs(x=NULL,
       y=NULL,
       title = "FG MISSED BY PLAYER",
       subtitle = "FOURTH QUARTER VS MEMPHIS HUSTLE JANUARY 3 2023",
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
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank()
  )
fourth_miss_per_player_plot

first <- made_miss_fg_pctg_plot + miss_per_player_plot 
second <- second_made_miss_fg_pctg_plot + second_miss_per_player_plot
third <- third_made_miss_fg_pctg_plot + third_miss_per_player_plot
fourth <- fourth_made_miss_fg_pctg_plot + fourth_miss_per_player_plot

first / second
third / fourth

capitanes_hustle_first_quarter <- filter(capitanes_hustle, (PERIOD == 1))
capitanes_hustle_second_quarter <- filter(capitanes_hustle, (PERIOD == 2))
capitanes_hustle_third_quarter <- filter(capitanes_hustle, (PERIOD == 3))
capitanes_hustle_fourth_quarter <- filter(capitanes_hustle, PERIOD == 4)

fgs_first_quarter <- sum(capitanes_hustle_first_quarter$EVENTMSGTYPE == 1) + sum(capitanes_hustle_first_quarter$EVENTMSGTYPE == 2)


