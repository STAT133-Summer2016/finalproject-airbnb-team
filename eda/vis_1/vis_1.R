library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggthemes)

hotel_citydf = read_csv("hotel_citydf.csv")

ggplot(hotel_citydf, aes(`Bed(s)`, Avg, fill = Property.Form))+
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid(~City)+
  scale_x_continuous(breaks = c(1,2)) +
  scale_y_continuous(limits =  c(0,500),
                     breaks = seq(0,300,100)) +
  scale_fill_discrete(name = "Property Form", 
                      labels = c("Airbnb","Hotels")) +
  labs(x = "Number of Beds", 
       y = "Average Price Per Night", 
       title = "Price Comparisons Between Airbnb & Hotels") +
  theme_calc() +
  scale_color_calc()

#avg daily price for airbnb seems to be significantly cheaper compared to 
#that of hotels

#need to explore variability/SD in prices for airbnb & hotels; add 
#another figure depicting findings about SD in airbnb & hotel prices
