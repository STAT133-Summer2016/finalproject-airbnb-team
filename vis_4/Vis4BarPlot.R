library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(scales)
library(ggthemes)

#room.type
citydf = read_csv("updated_cleaned_four_cities.csv") %>% 
  mutate(city = factor(City, levels = c("la", "ch", "sf", "ny"), 
                       labels = c("Los Angeles", "Chicago", "San Francisco", "New York"))) %>% 
  filter(Room.Type %in% c("Shared room","Private room","Entire home/apt")) %>%
  group_by(City, Room.Type) %>% 
  mutate(dailyavg = mean(Price))

#property.type
citydf_1 = read_csv("updated_cleaned_four_cities.csv") %>% 
  mutate(city = factor(City, levels = c("la", "ch", "sf", "ny"), 
                       labels = c("Los Angeles", "Chicago", "San Francisco", "New York"))) %>% 
  filter(Room.Type %in% c("Shared room","Private room","Entire home/apt")) %>%
  group_by(City, Property.Type) %>% 
  mutate(dailyavg = mean(Price))


#ggplot for room type
ggplot(citydf)+
  geom_bar(aes(x = Room.Type, 
               y = dailyavg, 
               fill = Room.Type), 
           color = "black",
           stat="identity", 
           position = position_dodge(), 
           width = 0.2) + 
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "$"),
                     limits = c(0,300),
                     breaks = seq(0,300,100)) +
  facet_grid(~city) +
  labs(title = "Average Daily Price of Airbnb by Room Type",
       x = "Type of Room",
       y = "Price") +
  scale_fill_discrete(name = "Type of Room") +
  theme_wsj()

#ggplot for property type
ggplot(citydf_1)+
  geom_bar(aes(x = Property.Type, 
               y = dailyavg, 
               fill = Property.Type), 
           color = "black",
           stat="identity", 
           position = position_dodge(), 
           width = 0.2) + 
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "$"),
                     limits = c(0,300),
                     breaks = seq(0,300,100)) +
  facet_grid(~city) +
  labs(title = "Average Daily Price of Airbnb by Property Type",
       x = "Type of Room",
       y = "Price") +
  scale_fill_discrete(name = "Type of Property") +
  theme_wsj()


  