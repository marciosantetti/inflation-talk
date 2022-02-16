library(tidyverse)
library(scales)
library(ggeasy)
library(lubridate)
library(hrbrthemes)
library(plotly)
library(tsibble)
library(gganimate)


theme_set(theme_ipsum_rc())


##-------------------------------------------------

cpi_data <- read_csv("cpi-data.csv")



cpi_data <- cpi_data %>% 
  mutate(period = mdy(period))



p1 <- cpi_data %>% 
  ggplot(aes(x = period, y = cpi)) +
  geom_line(color = "#993333", size = 1) +
  scale_y_continuous(labels = percent_format(scale=1)) +
  geom_line(aes(y = cpi_energy), color = "blue") +
  geom_hline(yintercept = 0) +
  expand_limits(y = c(-80, 75)) 
  

  
p1 %>% 
  ggplotly()



#####


p2 <- cpi_data %>% 
  as_tsibble(index = period) %>% 
  pivot_longer(c(cpi, cpi_food, cpi_energy, cpi_gasoline, cpi_less), names_to = "Series") %>% 
  ggplot(aes(x = period, y = value)) +
  geom_line(aes(color = Series)) +
  geom_hline(yintercept = 0) +
  expand_limits(y = c(-80, 75)) 


######


cpi_data_ts <- cpi_data %>% 
  select(period, cpi, cpi_food, cpi_energy, cpi_less) %>% 
  as_tsibble(index = period) %>% 
  pivot_longer(c(cpi, cpi_food, cpi_energy, cpi_less), names_to = "series")



p3 <- cpi_data_ts %>% 
  ggplot(aes(x = period, y = value)) +
  geom_line(aes(color = series)) +
  geom_hline(yintercept = 0)


new_labels <- c("cpi" = "All items",
                "cpi_energy" = "Energy",
                "cpi_food" = "Food",
                "cpi_less" = "Commodities less food and energy")

pp <- cpi_data_ts %>% 
  ggplot(aes(x = period, y = value)) +
  geom_line(size = 0.7, aes(color = series), alpha = 0.6) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent_format(scale=1)) +
  theme(legend.position = "bottom") +
  scale_color_viridis_d() +
  labs(y = "",
       x = "",
       title = "Consumer Price Index: 12-month change by category") +
  easy_x_axis_labels_size(13) +
  easy_y_axis_labels_size(13) 


pp %>% ggplotly()


pp + transition_reveal(period)


anim_save("test2.gif")




####


plt <- cpi_data %>% 
  ggplot(aes(x = period, y = cpi)) +
  geom_point(aes(frame=year(period)))

plt %>% ggplotly() 



cpi_data_ts %>% 
  ggplot(aes(x = period, y = value, color = series)) +
  geom_line(size = 0.7) +
  geom_hline(yintercept = 0)



cpi_data %>% 
  mutate(t = yearquarter(period))
