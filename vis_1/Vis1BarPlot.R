hotel_citydf = read_csv("hotel_citydf.csv")

ggplot(hotel_citydf, aes(`Bed(s)`, Avg, fill = Property.Form))+
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid(~City)+
  scale_x_continuous(breaks = c(1,2))