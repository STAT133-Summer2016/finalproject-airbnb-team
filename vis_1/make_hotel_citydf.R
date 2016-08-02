library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

hoteldf = read_csv("cleaned_four_hotels.csv") %>% 
  mutate(Property.Form = "hotel",
         Price = parse_number(Price),
         City = factor(City, 
                       levels = c("la","sf","ch","nyc"),
                       labels= c("Los Angeles","San Francisco","Chicago","New York City")))
 hoteldf =  hoteldf %>%
  group_by(City, `Bed(s)`) %>%
  mutate(Avg = mean(Price, na.omit = TRUE)) %>%
  select(Room.Type, City, `Bed(s)`, Property.Form, Avg)
  
  
citydf = read_csv("updated_cleaned_four_cities.csv") %>% 
  mutate(Property.Form = "airbnb",
         City = factor(City, 
                       levels = c("la","sf","ch","ny"),
                       labels= c("Los Angeles","San Francisco","Chicago","New York City"))) %>% 
  select(Room.Type, City, `Bed(s)`, Property.Form) %>% 
  filter(`Bed(s)` < 3) %>% 
  group_by(City, `Bed(s)`) %>% 
  mutate(Avg = mean(Price))
  

hotel_citydf = rbind(hoteldf, citydf)

write_csv(hotel_citydf, "hotel_citydf.csv")

