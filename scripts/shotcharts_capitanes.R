#capitanes <- read.csv('data/gleagueleaders.csv')
#capitanes <- subset(capitanes, TEAM=="MXC")
library(ggplot2)
library(ggtext)
library(rayshader)
library(rayrender)

masonjones_shotchart <- read.csv('data/masonjones_shotchart.csv')

# Hexbin chart with default option
mj <- ggplot(masonjones_shotchart, aes(x=LOC_X, y=LOC_Y) ) +
  geom_hex(binwidth = c(7, 7), show.legend = FALSE) +
  scale_fill_gradient(low="#FFFFDD",high="#F9EB44",trans="log10") +
  theme_bw() +
  labs(x=NULL,
       y=NULL,
       title = "Mason Jones #13",
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

mj

plot_gg(mj, width = 5, height = 4, scale = 300, raytrace = FALSE, preview = TRUE)
plot_gg(mj, width = 5, height = 4, scale = 300, multicore = TRUE, windowsize = c(1000, 800))
#render_camera(fov = 70, zoom = 0.5, theta = 130, phi = 35)

{
  png_outfile <- 'plots/masonjones_shotchart.png'
  render_camera(theta = -20, phi = 45, zoom = .8)
  
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if (!file.exists(png_outfile)) {
    png::writePNG(matrix(1), target = png_outfile)
  }
  render_highquality(
    filename = png_outfile,
    interactive = FALSE,
    lightdirection = 280,
    lightaltitude = c(20, 80),
    lightcolor = c("#FFFFE6", "white"),
    lightintensity = c(600, 100),
    samples = 300,
    width = 2000,
    height = 2000
  )
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}

#render_camera(theta = -20, phi = 45, zoom = .6)
#render_snapshot(clear = TRUE)
