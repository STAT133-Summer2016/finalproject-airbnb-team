library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(scales)
library(ggthemes)


#property.type
citydf_1 = read_csv("updated_cleaned_four_cities.csv") %>% 
  group_by(City, Property.Type) %>% 
  mutate(dailyavg = mean(Price)) 


#ggplot for property type
ggplot(citydf_1)+
  geom_bar(aes(x = Property.Type, 
               y = dailyavg, 
               fill = Property.Type), 
           color = "black",
           stat="identity", 
           position = position_dodge(), 
           width = 0.8) + 
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "$"),
                     limits = c(0,300),
                     breaks = seq(0,300,100)) +
  labs(title = str_wrap(
    "Figure 4: Average Daily Price of Airbnb by Property Type"),
       x = "Type of Room",
       y = "Price") +
  scale_fill_discrete(name = "Type of Property") +
  theme_gdocs() +
  theme(
    axis.title.x = element_text(size = 13, face = "bold"),
    axis.title.y = element_text(size = 13, face = "bold"),
    plot.title = element_text(size = 15, face = "bold")) +
  facet_grid(~City)



