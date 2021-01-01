# Load packages -----------------------------------------------------------
library(tidyverse)
library(lubridate)
library(ggtext)
library(gtrendsR)
library(showtext)

# Load font ---------------------------------------------------------------
font_add_google("Playfair Display", bold.wt = 700)
showtext_auto()

# Get data ----------------------------------------------------------------
trend_raw <- gtrends("Coronavirus", time = "2020-01-01 2020-12-31")

# Wrangle -----------------------------------------------------------------
trend <- trend_raw %>% 
  collapse::unlist2d() %>% 
  as_tibble() %>% 
  filter(.id == "interest_over_time") %>% 
  select(date, hits) %>% 
  mutate(
    tiles = 1,
    date = as_date(date),
    hits = hits %>% 
      str_replace("<1", "0") %>% 
      as.numeric()
  )

# Plot --------------------------------------------------------------------
trend %>% 
  ggplot(aes(date, tiles, height = 50*tiles, fill = hits)) + 
  geom_tile() + 
  scale_x_date(date_breaks = "months", date_labels = month.abb) +
  scale_y_continuous(limits = c(-100, NA)) +
  coord_polar(start = 0) + 
  annotate("segment", x = ymd("2020-01-01"), xend = ymd("2020-01-01"), 
           y = -24, yend = 26, colour = "yellow", size = 1) +
  annotate("text", x = ymd("2020-01-01"), y = -95, label = "2020", size = 18, family = "Playfair Display", fontface = "bold") +
  scale_fill_gradientn(colors = c("#011532", "#afd3e9", "#ebf3fa", "#ed2412", "#660005")) + 
  guides(fill = guide_colorbar(barwidth = 12, barheight = 0.4, unit = "cm")) + 
  labs(
    title = "Coronavirus",
    subtitle = "<br>The Most Searched Word<br> on <span style='color:#4285F4'>G</span><span style='color:#EA4335'>o</span><span style='color:#FBBC05'>o</span><span style='color:#4285F4'>g</span><span style='color:#34A853'>l</span><span style='color:#EA4335'>e</span><br>",
    caption = "\n\n Data by Google Trends \n Visualization by @botanagin"
  ) + 
  theme_void() +
  theme(
    axis.text.x = element_text(family = "Playfair Display", face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(family = "Playfair Display", face = "bold"),
    legend.key.size = unit(2, "cm"),
    legend.position = "bottom",
    legend.box.margin = margin(t = 0.8, unit = "cm"),
    plot.title = element_text(hjust = 0.5, size = 24,
                              family = "Playfair Display", face = "bold"),
    plot.subtitle = element_markdown(hjust = 0.5, size = 18,
                                  family = "Playfair Display", face = "bold"),
    plot.caption = element_text(hjust = 0.5, 
                                family = "Playfair Display", face = "bold"),
    plot.background = element_rect(fill = "gray99"),
    plot.margin = margin(1, 1, 1, 1, unit = "cm")
  ) 
ggsave(here::here("plots", "trend-2020.png"), width = 6, height = 9, units = "in")
knitr::plot_crop(here::here("plots", "trend-2020.png"))
