library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(scales)
library(ggthemes)



#property.type
citydf_1 = read_csv("updated_cleaned_four_cities.csv") %>% 
  #Relabel cities to full names
  mutate(city = factor(City, levels = c("la", "ch", "sf", "ny"), 
                       labels = c("Los Angeles", 
                                  "Chicago", 
                                  "San Francisco", 
                                  "New York"))) %>%
  group_by(City, Property.Type) %>% 
  mutate(dailyavg = mean(Price)) %>% 
  #filter property type so that it only contains the common properties among
  #all four cities
  filter(Property.Type %in% c("Townhouse", 
                              "Loft", 
                              "House", 
                              "Condominium", 
                              "Bed & Breakfast", 
                              "Apartment"))


#ggplot for property type
ggplot(citydf_1)+
  geom_bar(aes(x = Property.Type, 
               y = dailyavg, 
               fill = city), 
           color = "black",
           stat="identity", 
           position = position_dodge(), 
           width = 0.8) + 
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "$"),
                     limits = c(0,300),
                     breaks = seq(0,300,100)) +
  labs(title = "Average Daily Price of Airbnb by Property Type",
       x = "Type of Room",
       y = "Price") +
  scale_fill_discrete(name = "Type of Property") +
  theme_wsj()


  