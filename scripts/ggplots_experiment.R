library(dplyr)
library(scales)
library(gt)
library(tidyverse)
library(webshot2)
library(gtExtras)
library(ggplot2)

library(grid)
library(ggnewscale)
library(ggtext)
library(tidyverse)
library(shadowtext)
library(patchwork)


capitanes_fourfactors_22season <- read.csv('data/capitanes_fourfactors_2022_season.csv')

# ----- This section prepare a dataframe for labels ---- #
# Get the name and the y position of each label
label_data <- capitanes_fourfactors_22season

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$GAME_NO-0.5) / number_of_bar
# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)
# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)

p <- ggplot(capitanes_fourfactors_22season, aes(x=as.factor(GAME_NO), y=eFGPCTG)) + 
  geom_bar(stat="identity", fill=alpha("blue", 0.9)) +
  ylim(-0.5,1) +
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm")     # This remove unnecessary margin around plot
  ) +
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=label_data, aes(x=GAME_NO, y=eFGPCTG, label=GAME, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

p

my_plot <- ggplot(capitanes_fourfactors_22season, aes(x=GAME_NO)) + 
  geom_line(aes(y=eFGPCTG), color="#3b4c98") +
  geom_line(aes(y=OP_eFGPCTG), color="black") +
  facet_grid()
  # Add the labels, using the label_data dataframe that we have created before
  #+ geom_text(data=label_data, aes(x=GAME_NO, y=eFGPCTG, label=RESULT), color="black", fontface="bold",alpha=0.6, size=2.5, inherit.aes = FALSE)

my_plot
  
