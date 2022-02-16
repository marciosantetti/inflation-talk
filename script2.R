library(tidyverse)
library(scales)
library(ggeasy)
library(lubridate)
library(hrbrthemes)
library(plotly)
library(tsibble)
library(gganimate)
library(zoo)


theme_set(theme_ipsum_rc())

##-------------------------------------------------

gdp_data <- read_csv("gdp-data.csv")


gdp_data <- gdp_data %>% 
  mutate(period = mdy(period)) %>% 
  mutate(qtr =  as.yearqtr(period, format = "%Y-%m-%d"))



gdp_data <- gdp_data %>%
  mutate(gdp = as.double(gdp)) 



gdp_data %>% 
  ggplot(aes(y = pot, x = qtr)) +
  geom_line()


gdp_ts <- gdp_data %>% 
  select(qtr, pot, gdp) %>% 
  pivot_longer(c(pot, gdp), names_to = "series")



gdp_ts %>% 
  ggplot(aes(x = qtr, y = value)) +
  geom_line(aes(color = series), size = 0.8, alpha = 0.7) +
  scale_y_continuous(labels = comma) +
  labs(y = "Billions of Chained 2012 Dollars",
       x = "",
       title = "Actual vs. Potential GDP: 2015-2022",
       caption = "Data from FRED St. Louis.") +
  scale_color_discrete(labels = c("Actual GDP", "Potential GDP")) +
  easy_y_axis_title_size(15) +
  easy_plot_title_size(20) +
  easy_remove_legend_title()
