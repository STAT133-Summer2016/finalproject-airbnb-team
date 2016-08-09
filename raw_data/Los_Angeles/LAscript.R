
library(rvest, lib.loc = "~/Documents/rpackages")
library(dplyr)
library(stringr)
library(readr)
library(jsonlite)
library(magrittr)
library(selectr, lib.loc = "~/Documents/rpackages")


#function that scrapes prices for each listing for the first 100 pages
PriceFunction <- function(x) {
  print(x)
  Sys.sleep(sample(seq(10, 15, .1), size = 1))
  read_html(
    str_c("https://www.airbnb.com/s/Los-Angeles--CA--United-States?page=", 
          x)) %>% 
    html_nodes(".price-amount") %>% 
    html_text() %>% 
    as.numeric()
}


#scrapes the IDs from relevant attributes from each page for page 1 to 100
IDsFunction <- function(x) {
  print(x)
  Sys.sleep(sample(seq(10, 15, .1), size = 1))
  read_html(
    str_c("https://www.airbnb.com/s/Los-Angeles--CA--United-States?page=", 
          x)) %>% 
    html_nodes("a.media-photo.media-cover") %>% 
    html_attr("data-reactid") %>% 
    str_extract_all("\\$[0-9]*\\.") %>% 
    str_replace_all("\\$([0-9]*)\\.", "\\1")
}


#function that scrapes the attribute list for each listing 
AttributeFunction <- function(x) {
  print(x)
  if(x %in% pause_indices) {
    Sys.sleep(180)
    read_html(
      str_c("https://www.airbnb.com/rooms/",
            IDs[x],
            "?s=TPHZXwbi")) %>% 
      html_nodes("div.col-md-6 div") %>% 
      html_text()
  } else {
    Sys.sleep(sample(seq(10, 12, .1), size = 1))
    read_html(
      str_c("https://www.airbnb.com/rooms/",
            IDs[x],
            "?s=TPHZXwbi")) %>% 
      html_nodes("div.col-md-6 div") %>% 
      html_text()
  }
}


pricelistings <- lapply(as.list(1:100), PriceFunction) %>% 
  Reduce(c, .)


write_csv(as.data.frame(pricelistings), "LAprices")
Sys.sleep(120)



IDs <- lapply(as.list(1:100), IDsFunction) %>% 
  Reduce(c, .)

write_csv(as.data.frame(IDs), "LAids")
Sys.sleep(120)

#gives indices for which the function pauses (3 minutes)
pause_indices <- c(82, 130, 201, 289, 310, 
                   390, 435, 502, 578, 624, 
                   698, 737, 822, 903, 981,
                   1008, 1072, 1120, 1199, 
                   1277, 1352, 1411, 1501,
                   1583, 1633, 1702, 1780,
                   1834)

#function that scrapes the attribute list for each listing 
AttributeFunction <- function(x) {
  print(x)
  if(x %in% pause_indices) {
    Sys.sleep(180)
    read_html(
      str_c("https://www.airbnb.com/rooms/",
            IDs[x],
            "?s=TPHZXwbi")) %>% 
      html_nodes("div.col-md-6 div") %>% 
      html_text()
  } else {
    Sys.sleep(sample(seq(10, 12, .1), size = 1))
    read_html(
      str_c("https://www.airbnb.com/rooms/",
            IDs[x],
            "?s=TPHZXwbi")) %>% 
      html_nodes("div.col-md-6 div") %>% 
      html_text()
  }
}


#converts each element of attr_list into single row dataframe
#data frames are then combined by row, and unlisted columns are replaced with NA
i <- 0
DataFrameList <- function(x) {
  i <- i + 1
  print(i)
  Att <- x %>% 
    str_replace_all("(.*):.*", "\\1")
  
  if(length(Att) < 30) {
    as.data.frame(
      structure(x %>% str_replace_all(".*:(.*)", "\\1"),
                names = Att,
                class = "list")) %>% 
      mutate(ID = IDs[i], price = pricelistings[i]) %>% 
      .[1, colnames(.) %in% collumns]
  } else {
    data.frame("Accommodates" = NA) %>% 
      mutate(ID = IDs[i], price = pricelistings[i])
  }
}


attr_list <- lapply(as.list(1:length(IDs)), AttributeFunction)

#possible variable names...
collumns <- c("Accommodates", "Bedrooms", "Bathrooms", "Bed type",
              "Property type", "Room type", "Weekly discount", 
              "Monthly discount", "Cancellation", "ID", "price", 
              "Check In", "Check Out", "Cleaning Fee", "Security Deposit",
              "Response time", "Weekly Price", "Pet Owner", "Monthly Price", 
              "Beds", "Check.In", "Check.Out", "Property.type", "Room.type", 
              "Extra.people", "Weekly.discount", "Monthly.discount", 
              "Response.time")


df_list <- lapply(attr_list, DataFrameList) 

super_table <- bind_rows(df_list)

WriteXLS(super_table, "LAdf")




