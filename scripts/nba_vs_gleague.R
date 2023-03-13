library(dplyr)
library(ggplot2)
library(patchwork)
library(ggthemes)
library(ggtext)

gleague <- read.csv('data/gleagueleaders.csv')
nba <- read.csv('data/nbaleaders.csv')

#capitanes <- subset(capitanes, TEAM=="MXC")
#arrange(desc(FG_PCT))

gleague_fg <- subset(gleague, select = c("PLAYER", "TEAM", "GP", "FGM", "FGA", "FG_PCT", "FG3M", "FG3A", "FG3_PCT", "FTM", "FTA", "FT_PCT"))
gleague_fg <- subset(gleague_fg, GP > 20)

nba_fg <- subset(nba, select = c("PLAYER", "TEAM", "GP", "FGM", "FGA", "FG_PCT", "FG3M", "FG3A", "FG3_PCT", "FTM", "FTA", "FT_PCT"))
nba_fg <- subset(nba_fg, GP > 20)


nba_fg <- nba_fg %>% 
  mutate(FGA_PGM = FGA/GP) %>% 
  mutate(FGM_PGM = FGM/GP) %>% 
  mutate(FG3A_PGM = FG3A/GP) %>%
  mutate(FG3M_PGM = FG3M/GP) %>%
  mutate(FTA_PGM = FTA/GP) %>%
  mutate(FTM_PGM = FTM/GP) %>%
  mutate(LEAGUE = "NBA") %>%
  arrange(desc(FGA_PGM))

gleague_fg <- gleague_fg %>% 
  mutate(FGA_PGM = FGA/GP) %>% 
  mutate(FGM_PGM = FGM/GP) %>% 
  mutate(FG3A_PGM = FG3A/GP) %>%
  mutate(FG3M_PGM = FG3M/GP) %>%
  mutate(FTA_PGM = FTA/GP) %>%
  mutate(FTM_PGM = FTM/GP) %>%
  mutate(LEAGUE = "G LEAGUE") %>%
  arrange(desc(FGA_PGM))

both_leagues_fg <- rbind(gleague_fg,nba_fg)

fgmpg <- ggplot(both_leagues_fg, aes(x=LEAGUE, y=FGM_PGM, fill = LEAGUE)) + 
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) + 
  labs(
    title = "FIELD GOALS MADE PER GAME",
    caption = "<br>**Data:** Data: NBA.com | **Plot:** @el_dato_mx, @foreverpelon",
    x = NULL,
    y = NULL
  ) +
  theme_classic() +
  theme(
    plot.caption = element_markdown(size = 6),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

fgapg <- ggplot(both_leagues_fg, aes(x=LEAGUE, y=FGA_PGM, fill = LEAGUE)) + 
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) + 
  labs(
    title = "FIELD GOALS ATTEMPTS PER GAME",
    caption = "<br>**Data:** Data: NBA.com | **Plot:** @el_dato_mx, @foreverpelon",
    x = NULL,
    y = NULL
  ) +
  theme_classic() +
  theme(
    plot.caption = element_markdown(size = 6),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

fgtmpg <- ggplot(both_leagues_fg, aes(x=LEAGUE, y=FG3M_PGM, fill = LEAGUE)) + 
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) + 
  labs(
    title = "THREE POINTER MADE PER GAME",
    caption = "<br>**Data:** Data: NBA.com | **Plot:** @el_dato_mx, @foreverpelon",
    x = NULL,
    y = NULL
  ) +
  theme_classic() +
  theme(
    plot.caption = element_markdown(size = 6),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

fgtapg <- ggplot(both_leagues_fg, aes(x=LEAGUE, y=FG3A_PGM, fill = LEAGUE)) + 
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) + 
  labs(
    title = "THREE POINTER ATTEMPTS PER GAME",
    caption = "<br>**Data:** Data: NBA.com | **Plot:** @el_dato_mx, @foreverpelon",
    x = NULL,
    y = NULL
  ) +
  theme_classic() +
  theme(
    plot.caption = element_markdown(size = 6),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

ftmpg <- ggplot(both_leagues_fg, aes(x=LEAGUE, y=FTM_PGM, fill = LEAGUE)) + 
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) + 
  labs(
    title = "FREE THROWS MADE PER GAME",
    caption = "<br>**Data:** Data: NBA.com | **Plot:** @el_dato_mx, @foreverpelon",
    x = NULL,
    y = NULL
  ) +
  theme_classic() +
  theme(
    plot.caption = element_markdown(size = 6),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

ftapg <- ggplot(both_leagues_fg, aes(x=LEAGUE, y=FTA_PGM, fill = LEAGUE)) + 
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) + 
  labs(
    title = "FREE THROW ATTEMPTS PER GAME",
    caption = "<br>**Data:** Data: NBA.com | **Plot:** @el_dato_mx, @foreverpelon",
    x = NULL,
    y = NULL
  ) +
  theme_classic() +
  theme(
    plot.caption = element_markdown(size = 6),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

fgpct <- ggplot(both_leagues_fg, aes(x=LEAGUE, y=FG_PCT, fill = LEAGUE)) + 
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  labs(
    title = "FIELD GOAL PERCENTAGE",
    caption = "<br>**Data:** Data: NBA.com | **Plot:** @el_dato_mx, @foreverpelon",
    x = NULL,
    y = NULL
  ) +
  scale_y_continuous(label = function(x) {return(paste(x*100, "%"))}) +
  theme_classic() +
  theme(
    plot.caption = element_markdown(size = 6),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
fgtpct <- ggplot(both_leagues_fg, aes(x=LEAGUE, y=FG3_PCT, fill = LEAGUE)) + 
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) + 
  labs(
    title = "THREE POINTER PERCENTAGE",
    caption = "<br>**Data:** Data: NBA.com | **Plot:** @el_dato_mx, @foreverpelon",
    x = NULL,
    y = NULL
  ) +
  scale_y_continuous(label = function(x) {return(paste(x*100, "%"))}) +
  theme_classic() +
  theme(
    plot.caption = element_markdown(size = 6),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
ftpct <- ggplot(both_leagues_fg, aes(x=LEAGUE, y=FT_PCT, fill = LEAGUE)) + 
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) + 
  labs(
    title = "FREE THROW PERCENTAGE",
    caption = "<br>**Data:** Data: NBA.com | **Plot:** @el_dato_mx, @foreverpelon",
    x = NULL,
    y = NULL
  ) +
  scale_y_continuous(label = function(x) {return(paste(x*100, "%"))}) +
  theme_classic() +
  theme(
    plot.caption = element_markdown(size = 6),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


fgmpg
fgapg
fgtapg
fgtmpg
ftmpg
ftapg

fgpct
fgtpct
ftpct

(fgmpg + fgapg) / (fgtmpg + fgtapg) / (ftmpg + ftapg)

(fgpct + fgtpct + ftpct)
