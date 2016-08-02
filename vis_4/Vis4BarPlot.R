library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

citydf = read_csv("updated_four_cities.csv") %>% 
  mutate(city = factor(city, levels = c("la", "ch", "sf", "nyc"), 
                       labels = c("Los Angeles", "Chicago", "San Francisco", "New York"))) %>% 
  group_by(Room.type, city) %>% 
  mutate(dailyavg = mean(price))

ggplot(citydf)+
  geom_bar(aes(x = Room.type, 
               y = dailyavg, 
               fill = Room.type), 
           color = "black",
           stat="identity", 
           position = position_dodge(), 
           width = 0.2) + 
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  facet_grid(~city) +
  labs(title = "Average daily rate of Airbnb rentals by type of room",
       x = "Type of Room",
       y = "Price") +
  scale_fill_discrete(name = "Type of Room")
  