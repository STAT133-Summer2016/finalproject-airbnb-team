
library(rvest, lib.loc = "~/Documents/rpackages")
library(dplyr)
library(stringr)
library(readr)
library(jsonlite)
library(magrittr)
library(selectr, lib.loc = "~/Documents/rpackages")

pricelistings <- read_html(
  str_c("https://www.airbnb.com/s/chicago?page=1")) %>% 
  html_nodes(".price-amount") %>% 
  html_text()

for(i in 2:100) {
  pricelistings <- c(
    pricelistings,
    read_html(
      str_c("https://www.airbnb.com/s/chicago?page=", 
            i)) %>% 
      html_nodes(".price-amount") %>% 
      html_text()
  ) 
  print(i)
  Sys.sleep(rnorm(mean = 6, n = 1))
}

write_csv(as.data.frame(pricelistings), "Chicagoprices")
Sys.sleep(120)
#Initialize first page of IDs then read in subsequent pages and add to 
#vector containing IDs 
IDs <- read_html(
  str_c("https://www.airbnb.com/s/chicago?page=1")) %>% 
  html_nodes("a.media-photo.media-cover") %>% 
  html_attr("data-reactid") %>% 
  str_extract_all("\\$[0-9]*\\.") %>% 
  str_replace_all("\\$([0-9]*)\\.", "\\1")

for(i in 97:100) {
  IDs<- c(
    IDs,
    read_html(
      str_c("https://www.airbnb.com/s/chicago?page=", 
            i)) %>% 
      html_nodes("a.media-photo.media-cover") %>% 
      html_attr("data-reactid") %>% 
      str_extract_all("\\$[0-9]*\\.") %>% 
      str_replace_all("\\$([0-9]*)\\.", "\\1"))
  
  
  print(i)
}

write_csv(as.data.frame(IDs), "Chicagids")
Sys.sleep(120)
#uses relevant nodes from listing pages to create vector composed of 
#elements consisting of the variable/ listing detail, followed by the 
#value 
pause_indices <- c(82, 130, 201, 289, 310, 
                   390, 435, 502, 578, 624, 
                   698, 737, 822, 903, 981,
                   1008, 1072, 1120, 1199, 
                   1277, 1352, 1411, 1501,
                   1583, 1633, 1702, 1780,
                   1834)
attr_list <- list()
for(i in 1:length(IDs)) {
  attr_list[[i]] <- read_html(
    str_c("https://www.airbnb.com/rooms/",
          IDs[i],
          "?s=TPHZXwbi")) %>% 
    html_nodes("div.col-md-6 div") %>% 
    html_text()
  
  Sys.sleep(rnorm(mean = 8, n = 1))
  print(i)
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
  mutate(ID = IDs[1], price = pricelistings[1])
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
      mutate(ID = IDs[i], price = pricelistings[i]) 
    df_list[[i]] <- df_list[[i]][1, colnames(df_list[[i]]) %in% collumns]
    print(i)
  } else {
    attr_list[[i]] <- list("Accommodates : NA")
    df_list[[i]] <- data.frame("Accommodates" = NA) %>% 
      mutate(ID = IDs[i], price = pricelistings[i])
    print(i)
  }
}

#join current element with the aggregate table 
#comprised of previous elements 
super_table <- bind_rows(df_list)

WriteXLS(super_table, "Chicagodf")




