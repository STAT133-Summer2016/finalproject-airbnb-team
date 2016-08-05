library(readr)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)

four_cities <- read.csv("updated_cleaned_four_cities.csv")


##CLEANING CHECKED IN

#disregarding those observations with an unspecified/flexible check in or check out time
four_cities <- four_cities %>% 
  filter(!Check.In == "Unspecified") %>% 
  filter(!Check.In == "Flexible") %>% 
  filter(!is.na(Check.In)) %>% 
  filter(!Check.Out == "unspecified")

#only concerned with earliest check in times; removing ranges
four_cities$Check.In <- str_replace_all(four_cities$Check.In, "[ ]-.*", "")


#filters only pm check in values, removes pm, converts into numeric value and adds 12
#to put time into a 0-24hr interval for graphing
PM_only <- four_cities %>% 
  filter(str_detect(Check.In, "PM")) %>% 
  filter(!str_detect(Check.In, "12")) %>% 
  mutate(Check.In = as.numeric(str_replace_all(Check.In, "PM", ""))) %>% 
  mutate(Check.In = Check.In + 12)

#filters only am check in values, remvoes am, converts into numeric value
AM_only <- four_cities %>% 
  filter(str_detect(Check.In, "AM")) %>% 
  filter(!str_detect(Check.In, "12")) %>% 
  mutate(Check.In = as.numeric(str_replace_all(Check.In, "AM", "")))

#for one case of midnight and noontimes
twelve_pm <- four_cities %>% 
  filter(str_detect(Check.In, "12PM")) %>% 
  mutate(Check.In = as.numeric(str_replace_all(Check.In, "PM", "")))

twelve_am <- four_cities %>% 
  filter(str_detect(Check.In, "12AM")) %>% 
  mutate(Check.In = as.numeric(str_replace_all(Check.In, "AM", ""))) %>% 
  mutate(Check.In = Check.In + 12)
  
dfs <- list(PM_only, AM_only, twelve_pm, twelve_am)

cleaned_time_df <- join_all(dfs, type = "full")


##CLEANING CHECKED OUT

no_twelves_out <- cleaned_time_df %>% 
  filter(!str_detect(Check.Out, "12")) %>% 
  mutate(Check.Out = ifelse(str_detect(Check.Out, "AM"),
                            as.numeric(str_replace_all(Check.Out, "AM", "")), #as.numeric coerces NAs
                            as.numeric(str_replace_all(Check.Out, "PM", ""))+12))

cleaned_time_df <- cleaned_time_df %>% 
  filter(str_detect(Check.Out, "12")) %>% 
  mutate(Check.Out = ifelse(str_detect(Check.Out, "AM"),
                            as.numeric(str_replace_all(Check.Out, "AM", "")),
                            as.numeric(str_replace_all(Check.Out, "PM", ""))+12))

cleaned_time_df <- full_join(no_twelves_out, cleaned_time_df)

#mutating a factorized four_cities price_range column
cleaned_time_df <- cleaned_time_df %>% 
  mutate(Price.Range = (Price)) %>% 
  mutate(Price.Range = ifelse(Price < 50, "0-49", Price)) %>% 
  mutate(Price.Range = ifelse(Price >= 50 & Price < 100, "50-99", as.character(Price.Range))) %>% 
  mutate(Price.Range = ifelse(Price >= 100 & Price < 200, "100-199", as.character(Price.Range))) %>% 
  mutate(Price.Range = ifelse(Price >= 200 & Price < 300, "200-299", as.character(Price.Range))) %>% 
  mutate(Price.Range = ifelse(Price >= 300, "300+", as.character(Price.Range)))
cleaned_time_df$Price.Range <- factor(cleaned_time_df$Price.Range, levels = c("0-49", "50-99", "100-199", "200-299", "300+"))


#check in time density plot 
#justification for filling by price range - are cheaper price ranges more accomodating
#in terms of check in times

city_names <- c(
  `ch` = "Chicago",
  `la` = "Los Angeles",
  `ny` = "New York",
  `sf` = "San Francisco"
)

ggplot(cleaned_time_df) +
  geom_density(alpha = 0.6, aes(Check.In, fill = Price.Range)) +
  labs(x= "Time of Day",
       y = "Density",
       title = "Density Plot of Airbnb Check In Times") +
  facet_wrap(~City) +
  scale_x_continuous(limits=c(6,24), 
                     breaks=seq(6,24,4)) +
  guides(color = "none") +
  theme_gdocs() +
  theme(text = element_text(size=17))


#check out time density plot
ggplot(cleaned_time_df) +
  geom_density(alpha = 0.6, aes(Check.Out, fill = Price.Range)) +
  labs(x= "Time of Day",
       y = "Density",
       title = "Density Plot of Airbnb Check Out Times") +
  facet_wrap(~City) +
  scale_x_continuous(limits=c(6,24), 
                     breaks=seq(6,24,4)) +
  guides(color = "none") +
  theme_gdocs() +
  theme(text = element_text(size=17))


