library(dplyr)
library(scales)
library(gt)
library(tidyverse)
library(webshot2)

# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# ----------------------- FOUR FACTORS------------------------------------
# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# ------------------------------------------------------------------------

capitanes_fourfactors_05mar23 <- read.csv('data/capitanes_vs_gogo_05mar23_fourfactors.csv')

#Generate a gt table from head of data
head(capitanes_fourfactors_05mar23) %>% 
  gt()

(ff_table <- head(capitanes_fourfactors_05mar23) %>% 
    gt() %>%
    cols_label(TEAM = "EQUIPO",eFGPCTG = "% EFECTIVO DE TIRO",TOVPCTG = "% DE TOVs",ORBPCTG = "% DE ORBs",FTPERFGA = "FT / FGA",ORtg = "RATING OFENSIVO") %>% 
    #Add a table title
    #Notice the `md` function allows us to write the title using markdown syntax (which allows HTML)
    tab_header(title = md("CAPITANES VS GO-GO")) %>% 
    #Add a data source footnote
    tab_source_note(source_note = "Data: basketball-reference.com (2023) @el_dato_mx, @foreverpelon"))


(ff_table <- ff_table %>% 
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
min_FTPCTG <- .524
max_FTPCTG <- .598
min_TOVPCTG <- 9.6
max_TOVPCTG<- 11.1
min_ORBPCTG <- 18.9
max_ORBPCTG <- 28.9
min_FTPERFGA <- .134
max_FTPERFGA <- .138
ftpctg_palette <- col_numeric(c("red", "green"), domain = c(min_FTPCTG, max_FTPCTG), alpha = 0.9)
tovpctg_palette <- col_numeric(c("green", "red"), domain = c(min_TOVPCTG, max_TOVPCTG), alpha = 0.9)
orbpctg_palette <- col_numeric(c("red", "green"), domain = c(min_ORBPCTG, max_ORBPCTG), alpha = 0.9)
ftperfga_palette <- col_numeric(c("red", "green"), domain = c(min_FTPERFGA, max_FTPERFGA), alpha = 0.9)

(ff_table <- ff_table %>%
    data_color(columns = c(eFGPCTG),
               colors = (ftpctg_palette)) %>% 
    data_color(columns = c(TOVPCTG),
               colors = (tovpctg_palette)) %>% 
    data_color(columns = c(ORBPCTG),
               colors = (orbpctg_palette)) %>% 
    data_color(columns = c(FTPERFGA),
               colors = (ftperfga_palette))
  )

(ff_table <- ff_table %>% 
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
    cols_width(c(ORtg) ~ px(75),
               c(TEAM) ~ px(100)) %>% 
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

ff_table %>% 
  gt::text_transform(
    #Apply a function to a column
    locations = cells_body(c(team_logo)),
    fn = function(x) {
      #Return an image of set dimensions
      web_image(
        url = x,
        height = 24
      )
    }
  ) %>% 
  #Hide column header flag_URL and reduce width
  cols_width(c(team_logo) ~ px(30)) %>% 
  cols_label(team_logo = "")

