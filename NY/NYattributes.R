library(rvest, lib.loc = "~/Documents/rpackages")
library(dplyr)
library(stringr)
library(readr)
library(magrittr)
library(selectr, lib.loc = "~/Documents/rpackages")
library(WriteXLS, lib.loc = "~/Documents/rpackages")


IDs<- read_csv("NYids")
pricelistings <- read_csv("NYprices")

attr_list <- list()
for(i in 1:length(IDs$IDs)) {
  attr_list[[i]] <- read_html(
    str_c("https://www.airbnb.com/rooms/",
          IDs$IDs[i],
          "?s=TPHZXwbi")) %>% 
    html_nodes("div.col-md-6 div") %>% 
    html_text()
  if(i %in% c(50, 101, 149, 202, 252, 299, 348, 403, 451, 502, 554, 604, 650, 703, 751, 804,
              851, 899, 949, 1001, 1055, 1101, 1150, 1200, 1251, 1305, 1355, 1404, 1452, 1501,
              1554, 1601, 1650, 1702, 1751)) {
    Sys.sleep(180)
    print(i)
  } else {
    Sys.sleep(sample(c(4, 4.1, 4.2, 4.25, 5, 5.1, 5.2, 5.32, 6, 6.22, 6.11, 8.1, 8.24, 9.89, 10.12),
                     size = 1, 
                     replace = T))
    print(i)
  }
}
#initialize empty lists to fill 
colnames <- list()
colvals <- list()
#possible variable names...
collumns <- c("Accommodates", "Bedrooms", "Bathrooms", "Bed type",
              "Property type", "Room type", "Weekly discount", 
              "Monthly discount", "Cancellation", "ID", "price", 
              "Check In", "Check Out", "Cleaning Fee", "Security Deposit",
              "Response time", "Weekly Price", "Pet Owner", "Monthly Price", 
              "Beds", "Check.In", "Check.Out", "Property.type", "Room.type", 
              "Extra.people", "Weekly.discount", "Monthly.discount", 
              "Response.time")
#initialize empty list to fill with single row data frames (as loop proceeds)
df_list <- list()
#removing variable names from first element of attr_list to start loop 
colnames[[1]] <- attr_list[[1]] %>% 
  str_replace_all("(.*):.*", "\\1")
#values for variables of first element to start loop 
colvals[[1]] <- attr_list[[1]] %>% 
  str_replace_all(".*:(.*)", "\\1")
#generate first data frame to join
super_table <- as.data.frame(
  structure(colvals[[1]], 
            names = colnames[[1]],
            class = "list")) %>% 
  mutate(ID = IDs$IDs[1], price = pricelistings$pricelistings[1])
df_list[[1]] <- super_table

for(i in 2:length(attr_list)) {
  #removes the attribute name and creates list 
  colnames[[i]] <- attr_list[[i]] %>% 
    str_replace_all("(.*):.*", "\\1") 
  
  if(length(colnames[[i]]) < 30) { 
    #removes attribute values and creates list
    colvals[[i]] <- attr_list[[i]] %>% 
      str_replace_all(".*:(.*)", "\\1")
    #creates list of single rows data frames,
    #row consists of values, column names come from colnames
    df_list[[i]]  <- as.data.frame(
      structure(colvals[[i]], 
                names = colnames[[i]], 
                class = "list")) %>% 
      mutate(ID = IDs$IDs[i], price = pricelistings$pricelistings[i]) 
    df_list[[i]] <- df_list[[i]][1, colnames(df_list[[i]]) %in% collumns]
    print(i)
  } else {
    attr_list[[i]] <- list("Accommodates : NA")
    df_list[[i]] <- data.frame("Accommodates" = NA) %>% 
      mutate(ID = IDs$IDs[i], price = pricelistings$pricelistings[i])
    print(i)
  }
}

#join current element with the aggregate table 
#comprised of previous elements 
super_table <- bind_rows(df_list)

WriteXLS(super_table, "NYdf")
