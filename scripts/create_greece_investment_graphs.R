# Updated on 2025-01-27: Reformatted R script to create stacked graphs of Athens Stock Exchange and search volumes for magazine publication

# Load required libraries
library(scales)
library(tidyverse)
library(patchwork)
source("scripts/funcs.R")

# Load and prepare search data
gr_search_data <- load_search_data_gkp_csv("data/raw/search_data_greekstocks_investment_opportunities_greece.csv")

gr_search_data_f <- gr_search_data %>%
  filter(date >= as.Date("2023-09-01")) %>%
  mutate(search_term = str_c('"', search_term, '"'))

# Load and prepare Athens Stock Exchange data
athex_data <- read_csv("data/raw/Athens General Composite Historical Data month2.csv") %>%
  mutate(
    date = as.Date(Date, format = "%m/%d/%Y"),
    atg_index = as.numeric(gsub(",", "", Price))
  ) %>%
  filter(date < as.Date("2025-08-01")) %>%
  select(date, atg_index)

# Create search volume plot
search_plot <- ggplot(gr_search_data_f, aes(x = date, y = search_volume)) +
  geom_point() +
  geom_line() +
  facet_wrap(nrow = 2, facets = "search_term", scales = "free_y") +
  #theme_minimal() +
  labs(
    x = NULL,
    y = "Monthly Search Volume - Global",
    caption = "\nData sources: Google Search Data accessed via Keyword Planner, Investing.com \n Data & analysis: github.com/papageorgiou/invest-greece | @alex_papageo"
  ) +
  geom_rect(
    data = data.frame(
      xmin = as.Date("2024-07-01"),
      xmax = as.Date("2024-12-01"),
      ymin = 254.93191101188,
      ymax = 2507L,
      search_term = '"greek stocks"'
    ),
    mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    colour = "black",
    fill = "red1",
    alpha = 0.1,
    inherit.aes = FALSE
  ) +
  geom_rect(
    data = data.frame(
      xmin = as.Date("2024-07-01"),
      xmax = as.Date("2024-12-01"),
      ymin = 20.330417114557,
      ymax = 334.9999999994,
      search_term = '"investment opportunities in greece"'
    ),
    mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    colour = "black",
    fill = "red1",
    alpha = 0.1,
    inherit.aes = FALSE
  )  +
 # ggthemes::theme_fivethirtyeight() +

  theme(
    plot.caption = element_text(
      hjust = 0,
      size = 11,
      face = "italic",
      color = "grey60"
    ),
    plot.caption.position = "plot"
  )

# Create Athens Stock Exchange plot
athex_plot <- ggplot(athex_data, aes(x = date, y = atg_index)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(
    limits = c(1000, NA),
    breaks = seq(1000, 2000, by = 250)
  ) +
  labs(title = "Greek Stock Market: From Search to Surge", subtitle= "Athens stock exchange rally followed a surge in search interest") +
  expand_limits(y = 1000) +
  geom_rect(
    data = data.frame(
      xmin = as.Date("2024-11-30"),
      xmax = as.Date("2025-07-05"),
      ymin = 1201.3213634532,
      ymax = 2026.6330623351
    ),
    mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    colour = "black",
    fill = "green3",
    alpha = 0.25,
    inherit.aes = FALSE
  ) +
  labs(x = NULL, y = "Athens Stock Exchange Index") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )  +
  #ggthemes::theme_fivethirtyeight()+
  theme(
    plot.caption = element_text(hjust = 0, size=9, face="italic"),   # 0 = left, 0.5 = center, 1 = right
    plot.caption.position = "plot",           # place caption against the plot edge
    #plot.title = element_markdown(lineheight = 1.05)
  )

# Combine plots
final_plot <- athex_plot / search_plot +
  plot_layout(nrow = 2, heights = c(1, 1))

# Display and save
final_plot

ggsave("outputs/greece_investment_graphs.png", final_plot, width = 6.3, height = 8)
