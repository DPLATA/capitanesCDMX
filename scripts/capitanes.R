library(dplyr)
library(scales)
library(gt)
library(tidyverse)
library(webshot2)

capitanes <- read.csv('data/capitanes.csv') %>% 
  arrange(desc(PTS)) 
  #%>% 
  #mutate(player_headshot = glue::glue("https://capitanes.mx/wp-content/uploads/2022/11/Alfonzo-254x300.png"))

#Generate a gt table from head of data
head(capitanes) %>% 
  gt()

(points_table <- head(capitanes) %>% 
    gt() %>% 
    #Hide unwanted columns
    cols_hide(columns = c(Season,Tm,G,GS,MP,FG,FGA,FGPCTG,THREEP,THREEPA,THREEPPCTG,TWOP,TWOPA,TWOPPCTG,FT,FTA,FTPCTG,ORB,DRB,TRB,AST,STL,BLK,TOV,PF)) %>% 
    #Rename columns
    cols_label(PTS = "PUNTOS POR JUEGO", Player = "JUGADOR") %>% 
    #Add a table title
    #Notice the `md` function allows us to write the title using markdown syntax (which allows HTML)
    tab_header(title = md("MÁXIMOS ANOTADORES POR JUEGO 2022-23")) %>% 
    #Add a data source footnote
    tab_source_note(source_note = "Data: basketball-reference.com (2023) @el_dato_mx, @foreverpelon"))


(points_table <- points_table %>% 
    #Apply new style to all column headers
    tab_style(
      locations = cells_column_labels(columns = everything()),
      style     = list(
        #Give a thick border below
        cell_borders(sides = "bottom", weight = px(3)),
        #Make text bold
        cell_text(weight = "bold")
      )
    ) %>% 
    #Apply different style to the title
    tab_style(
      locations = cells_title(groups = "title"),
      style     = list(
        cell_text(weight = "bold", size = 24)
      )
    ))

#Apply our palette explicitly across the full range of values so that the top countries are coloured correctly
min_PTS <- min(capitanes$PTS)
max_PTS <- max(capitanes$PTS)
pts_palette <- col_numeric(c("#BDD89E", "#103E0C"), domain = c(min_PTS, max_PTS), alpha = 0.75)

(points_table <- points_table %>% 
    data_color(columns = c(PTS),
               colors = (pts_palette)))

(points_table <- points_table %>% 
    #All column headers are capitalised
    opt_all_caps() %>% 
    #Use the Chivo font
    #Note the great 'google_font' function in 'gt' that removes the need to pre-load fonts
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    ) %>%
    #Change the width of columns
    cols_width(c(PTS) ~ px(150),
               c(Player) ~ px(300)) %>% 
    tab_options(
      #Remove border between column headers and title
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      #Remove border around table
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      #Reduce the height of rows
      data_row.padding = px(3),
      #Adjust font sizes and alignment
      source_notes.font.size = 12,
      heading.align = "left"
    ))

points_table %>% 
  gt::text_transform(
    #Apply a function to a column
    locations = cells_body(c(player_headshot)),
    fn = function(x) {
      #Return an image of set dimensions
      web_image(
        url = x,
        height = 24
      )
    }
  ) %>% 
  #Hide column header flag_URL and reduce width
  cols_width(c(player_headshot) ~ px(30)) %>% 
  cols_label(player_headshot = "")

