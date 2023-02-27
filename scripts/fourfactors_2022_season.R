library(dplyr)
library(scales)
library(gt)
library(tidyverse)
library(webshot2)
library(gtExtras)

capitanes_fourfactors_22season <- read.csv('data/capitanes_fourfactors_2022_season.csv')

#Generate a gt table from head of data
table <- capitanes_fourfactors_22season %>% 
  gt()

table <- table %>% 
  gt::text_transform(
    #Apply a function to a column
    locations = cells_body(c(CAPITANES_LOGO, OPPONENT_LOGO)),
    fn = function(x) {
      #Return an image of set dimensions
      web_image(
        url = x,
        height = 24
      )
    }
  ) %>% 
  #Hide column header flag_URL and reduce width
  cols_width(c(CAPITANES_LOGO, OPPONENT_LOGO) ~ px(30)) %>% 
  cols_label(CAPITANES_LOGO = "") %>% 
  cols_label(OPPONENT_LOGO = "")

table


 table <- table %>%
  gt_color_rows(
    eFGPCTG, pal_type = "discrete",
    palette = c("red", "green"), domain = range(table$eFGPCTG,OP_eFGPCTG)
  )
table
