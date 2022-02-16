library(tidyverse)
library(scales)
library(ggeasy)
library(lubridate)
library(hrbrthemes)
library(plotly)
library(tsibble)



theme_set(theme_ipsum_rc())


##-------------------------------------------------

cpi_data <- read_csv("cpi-data.csv")



cpi_data <- cpi_data %>% 
  mutate(period = mdy(period))



cpi_data_ts <- cpi_data %>% 
  select(period, cpi, cpi_food, cpi_energy, cpi_less) %>% 
  as_tsibble(index = period) %>% 
  pivot_longer(c(cpi, cpi_food, cpi_energy, cpi_less), names_to = "series")



new_labels <- c("cpi" = "All items",
                "cpi_energy" = "Energy",
                "cpi_food" = "Food",
                "cpi_less" = "Commodities less food and energy")




p1 <- cpi_data_ts %>% 
  filter(series %in% c("cpi", "cpi_food")) %>% 
  ggplot(aes(x = period, y = value)) +
  geom_line(size = 0.7, aes(color = series), alpha = 0.6) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent_format(scale=1)) +
  theme(legend.position = "bottom") +
  scale_color_discrete(labels = c("All items", "Food and beverages")) +
  labs(y = "",
       x = "",
       title = "Consumer Price Index by category",
       subtitle = "Change from a Year ago",
       caption = "Source: BLS.") +
  easy_x_axis_labels_size(17) +
  easy_y_axis_labels_size(17) +
  easy_plot_title_size(30) +
  easy_plot_caption_size(17) +
  easy_plot_subtitle_size(22) +
  easy_plot_legend_size(17) +
  easy_remove_legend_title()


p1 %>% ggplotly() %>%
  layout(title = list(text = paste0('Consumer Price Index by category',
                                    '<br>',
                                    '<sup>',
                                    'Change from a Year ago',
                                    '</sup>')))



p2 <- cpi_data_ts %>% 
  filter(series %in% c("cpi", "cpi_energy")) %>% 
  ggplot(aes(x = period, y = value)) +
  geom_line(size = 0.7, aes(color = series), alpha = 0.6) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent_format(scale=1)) +
  theme(legend.position = "bottom") +
  scale_color_discrete(labels = c("All items", "Energy")) +
  labs(y = "",
       x = "",
       title = "Consumer Price Index by category",
       subtitle = "Change from a Year ago",
       caption = "Source: BLS.") +
  easy_x_axis_labels_size(17) +
  easy_y_axis_labels_size(17) +
  easy_plot_title_size(30) +
  easy_plot_caption_size(17) +
  easy_plot_subtitle_size(22) +
  easy_plot_legend_size(17) +
  easy_remove_legend_title()


p2 %>% ggplotly() %>%
  layout(title = list(text = paste0('Consumer Price Index by category',
                                    '<br>',
                                    '<sup>',
                                    'Change from a Year ago',
                                    '</sup>')))
