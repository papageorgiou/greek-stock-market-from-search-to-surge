# Created on 2025-01-27: R script to create stacked graphs of Athens Stock Exchange and search volumes for magazine publication

# Load required libraries

library(scales)
library(tidyverse)
library(patchwork)
source("funcs.R")

floor

# Function to load Athens Stock Exchange data from CSV

gr_search_data = load_search_data_gkp_csv("search_data_greekstocks_investment_opportunities_greece.csv")

gr_search_data_f <-  gr_search_data %>% filter(date>=as.Date("2023-09-01")) %>%
  mutate(search_term = str_c('"', search_term, '"'))


athex_data <- read_csv("Athens General Composite Historical Data month2.csv") %>%
  mutate(
    date = as.Date(Date, format = "%m/%d/%Y"),
    atg_index = as.numeric(gsub(",", "", Price))
  ) %>% filter(date<as.Date("2025-08-01")) %>%
  select(date, atg_index)




search <- ggplot(gr_search_data_f, aes(x=date, y =search_volume)) + geom_point() +geom_line()+
facet_wrap(nrow=2, facets = "search_term", scales = "free_y") + theme_minimal() + labs(x = NULL, y= "Monthly Search Volume -Global", caption = "\nSource: Google Search Data, Investing.com") +
theme(
    plot.caption = element_text(
      hjust = 0,
      size = 11,
      face = "italic",
      color = "grey60"
    ),
    # 0 = left, 0.5 = center, 1 = right
    plot.caption.position = "plot") +
geom_rect(data = data.frame(xmin = as.Date("2024-07-01"),
                            xmax = as.Date("2024-12-01"),
                            ymin = 254.93191101188, ymax = 2507L, search_term = '"greek stocks"'),
          mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
          colour = "black", fill = "red1", alpha = 0.1, inherit.aes = FALSE) +
  geom_rect(data = data.frame(xmin = as.Date("2024-07-01"),
                              xmax = as.Date("2024-12-01"),
                              ymin = 20.330417114557, ymax = 334.9999999994, search_term = '"investment opportunities in greece"'),
            mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            colour = "black", fill = "red1", alpha = 0.1, inherit.aes = FALSE)


# From Search to Surge: Greek Market Rally in 2025
# Search Interest in Greek Stocks Preceded the 2025 ATHEX Rally
# Surge in Search interest preceded the 2025 Athens Stock Exchange Rally
athx <- ggplot(athex_data, aes(x = date, y = atg_index)) +
  geom_point() +
  geom_line() + scale_y_continuous(
    limits = c(1000, NA),                  # force y-axis to include 1000
    breaks = seq(1000, 2000, by = 250)     # tick marks every 250 points
  )+
  theme_minimal() + ggtitle("Athens Stock Exchange Rally Followed a Surge in Search Interest") + expand_limits(y = 1000) +
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
  ) + labs(x = NULL, y="Athens Stock Exchange Index") +    theme(
    axis.text.x  = element_blank(),  # remove text labels (months)
    axis.ticks.x = element_blank()   # remove tick marks
  )
  # geom_label(
  #   data = data.frame(
  #     x = as.Date("2025-03-23"),
  #     y = 1963.05651223322,
  #     label = "Athens Stock Exchange \n2025 Rally "
  #   ),
  #   mapping = aes(x = x, y = y, label = label),
  #   size = 1.7,  # <<-- shrink font to ~1/3 default
  #   label.padding = unit(0.15, "lines"),
  #   label.r = unit(0.15, "lines"),
  #   inherit.aes = FALSE
  # )

################################3

final <- athx/search + plot_layout(nrow = 2, heights=c(1,1))


final

