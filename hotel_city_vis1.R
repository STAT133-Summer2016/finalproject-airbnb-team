hoteldf = read_csv("cleaned_four_hotels.csv") %>% 
  mutate(Property.Form = "hotel") %>% 
  select(Price, Room.Type, City, `Bed(s)`, Property.Form, Avg)
  
citydf = read_csv("cleaned_four_cities.csv") %>% 
  mutate(Property.Form = "airbnb") %>% 
  select(Price, Room.Type, City, `Bed(s)`, Property.Form) %>% 
  filter(`Bed(s)` < 3) %>% 
  group_by(City, `Bed(s)`) %>% 
  mutate(Avg = mean(Price))
  

hotel_citydf = rbind(hoteldf, citydf)

write_csv(hotel_citydf, "hotel_citydf.csv")

