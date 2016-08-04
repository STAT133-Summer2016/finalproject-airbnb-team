library(ggplot2)
library(dplyr)
library(magrittr)
library(ggthemes)

hotels <- read_csv("cleaned_four_hotels.csv") %>% 
  mutate(price.num = as.numeric(parse_number(Price)))

hotel_hist <- ggplot(hotels) +
  geom_histogram(aes(x = price.num, 
                     y = ..density..,
                     fill = City),
                 binwidth = 10) +
  scale_x_continuous(limits = c(0, 800)) +
  labs(x = "Price",
       y = "Density",
       title = "Distribution of Hotel Pricing by City") + 
  theme_minimal() +
  scale_fill_manual(values = c("yellow", "red", "blue", "green4"))

hotel_deviations <- hotels %>% 
  group_by(City) %>% 
  dplyr::summarise(Deviations = sd(price.num))

sd(hotels$price.num) / mean(hotels$price.num)

ggsave("hotel_hist.pdf", hotel_hist)
