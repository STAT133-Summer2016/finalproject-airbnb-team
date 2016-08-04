library(ggplot2)
library(ggthemes)

cities_table <- read.csv("updated_four_cities.csv")

airbnb_hist <- ggplot(cities_table) +
  geom_histogram(aes(x = Price,
                     y = ..density..,
                     fill = City),
                 binwidth = 10,
                 position = "identity",
                 alpha = .5) +
  scale_x_continuous(limits = c(0, 800)) +
  labs(x = "Price",
       y = "Density",
       fill = "City",
       title = "Distribution of Airbnb Listing Prices by City") +
  theme_minimal() +
  scale_fill_manual(values = c("yellow", "red", "blue", "green4"))

cities_deviations <- cities_table %>% 
  mutate(SU = (Price - mean(cities_table$Price)) / sd(cities_table$Price)) %>% 
  group_by(City) %>% 
  dplyr::summarise(sd(SU))

SDmean_all <- sd(cities_table$Price) / mean(cities_table$Price) 

ggsave("airbnb_hist.pdf", airbnb_hist)
ggsave("airbnb_hist.png", airbnb_hist)

hotels <- read_csv("cleaned_four_hotels.csv") %>% 
  mutate(price.num = as.numeric(parse_number(Price)))

hotel_hist <- ggplot(hotels) +
  geom_histogram(aes(x = price.num, 
                     y = ..density..,
                     fill = City),
                 binwidth = 10,
                 position = "identity",
                 alpha = .5) +
  scale_x_continuous(limits = c(0, 800)) +
  labs(x = "Price",
       y = "Density",
       title = "Distribution of Hotel Pricing by City") + 
  theme_minimal() +
  scale_fill_manual(values = c("yellow", "red", "blue", "green4"))

hotel_deviations <- hotels %>% 
  mutate(SU = (price.num - mean(price.num)) / sd(price.num)) %>% 
  group_by(City) %>% 
  dplyr::summarise(sd(SU))

sd(hotels$price.num) / mean(hotels$price.num)

ggsave("hotel_hist.pdf", hotel_hist)
ggsave("hotel_hist.png", hotel_hist)