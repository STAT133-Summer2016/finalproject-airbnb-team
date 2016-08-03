library(ggplot2)
library(ggthemes)

cities_table <- read.csv("updated_four_cities.csv")

airbnb_hist <- ggplot(cities_table) +
  geom_histogram(aes(x = price,
                     y = ..density..,
                     fill = city),
                 binwidth = 10) +
  scale_x_continuous(limits = c(0, 800)) +
  labs(x = "Price",
       y = "Density",
       fill = "City",
       title = "Distribution of Airbnb Listing Prices by City") +
  theme_minimal() +
  scale_fill_manual(values = c("yellow", "red", "blue", "green4"))

cities_deviations <- cities_table %>% 
  group_by(city) %>% 
  dplyr::summarise(Deviation = sd(price))

sd(cities_table$price) / mean(cities_table$price) 

ggsave("airbnb_hist.pdf", airbnb_hist)
