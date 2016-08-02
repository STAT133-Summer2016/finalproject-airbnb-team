library(readr)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)

four_cities <- read.csv("updated_cleaned_four_cities.csv")


##CLEANING CHECKED IN

View(unique(four_cities$Check.In))

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


#check in time density plot                            
ggplot(cleaned_time_df, aes(Check.In, fill = City, colour = City)) +
  geom_density(alpha = 0.4) +
  ggtitle("Density Plot of AirBNB Check In Times") +
  xlab("Time of Day") +
  ylab("Density") +
  scale_x_continuous(limits=c(0,24), breaks=seq(0,24,4))

#check out time density plot
ggplot(cleaned_time_df, aes(Check.Out, fill = City, colour = City)) +
  geom_density(alpha = 0.4) +
  ggtitle("Density Plot of AirBNB Check Out Times") +
  xlab("Time of Day") +
  ylab("Density") +
  scale_x_continuous(limits=c(0,24), breaks=seq(0,24,4))
