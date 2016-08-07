library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(scales)
library(ggthemes)
library(scales)


#property.type
citydf_1 = read_csv("updated_cleaned_four_cities.csv") %>% 
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
               fill = City), 
           color = "black",
           stat="identity", 
           position = position_dodge(), 
           width = 0.8) + 
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "$"),
                     limits = c(0,300),
                     breaks = seq(0,300,100)) +
  labs(title = str_wrap("Figure 4: Average Daily Price of Airbnb by Property Type"),
       x = "Type of Room",
       y = "Price") +
  scale_fill_discrete(name = "Type of Property") +
  theme(
    axis.title.x = element_text(size = 13, face = "bold"),
    axis.title.y = element_text(size = 13, face = "bold"),
    plot.title = element_text(size = 15, face = "bold")) +
  theme_gdocs()


  