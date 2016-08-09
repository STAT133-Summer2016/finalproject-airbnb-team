## San Francisco
sf_dbl = read_csv("sf_dbl.csv")


sf_dbl <- sf_dbl %>%
  mutate(Room.Type ="Private room") %>%
  mutate(City = "San Francisco") %>%
  mutate(Bed = 2)
colnames(sf_dbl)[6] <- "Bed(s)"

sf_sgl <- read_csv("sf_sgl.csv")
sf_sgl <- sf_sgl %>%
  mutate(Room.Type ="Private room") %>%
  mutate(City = "San Francisco") %>%
  mutate(Bed = 1)
colnames(sf_sgl)[6] <- "Bed(s)"

sf_final <- rbind(sf_sgl,sf_dbl)

## NYC
ny_dbl <- read_csv("ny_dbl.csv")
ny_dbl <- ny_dbl %>%
  mutate(Room.Type ="Private room") %>%
  mutate(City = "New York") %>%
  mutate(Bed = 2)
colnames(ny_dbl)[6] <- "Bed(s)"

ny_sgl <- read_csv("ny_sgl.csv")
ny_sgl <- ny_sgl %>%
  mutate(Room.Type ="Private room") %>%
  mutate(City = "New York") %>%
  mutate(Bed = 1)
colnames(ny_sgl)[6] <- "Bed(s)"

ny_final <- rbind(ny_sgl,ny_dbl)

## Los Angeles
la_dbl <- read_csv("la_dbl.csv")
la_dbl <- la_dbl %>%
  mutate(Room.Type ="Private room") %>%
  mutate(City = "Los Angeles") %>%
  mutate(Bed = 2)
colnames(la_dbl)[6] <- "Bed(s)"

la_sgl <- read_csv("la_sgl.csv")
la_sgl <- la_sgl %>%
  mutate(Room.Type ="Private room") %>%
  mutate(City = "Los Angeles") %>%
  mutate(Bed = 1)
colnames(la_sgl)[6] <- "Bed(s)"

la_final <- rbind(la_sgl,la_dbl)

## Chicago
ch_dbl <- read_csv("ch_dbl.csv")
ch_dbl <- ch_dbl %>%
  mutate(Room.Type ="Private room") %>%
  mutate(City = "Chicago") %>%
  mutate(Bed = 2)
colnames(ch_dbl)[6] <- "Bed(s)"

ch_sgl <- read_csv("ch_sgl.csv")
ch_sgl <- ch_sgl %>%
  mutate(Room.Type ="Private room") %>%
  mutate(City = "Chicago") %>%
  mutate(Bed = 1)
colnames(ch_sgl)[6] <- "Bed(s)"

ch_final <- rbind(ch_sgl,ch_dbl)


#aggregate data frame for all four cities

cleaned_four_hotels = rbind(la_final, ch_final, ny_final, sf_final)

write_csv(cleaned_four_hotels, "cleaned_four_hotels.csv")
