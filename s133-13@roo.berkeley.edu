library(rvest, lib.loc = "~/Documents/rpackages")
library(dplyr)
library(stringr)
library(readr)
library(jsonlite)
library(magrittr)
library(selectr, lib.loc = "~/Documents/rpackages")
library(WriteXLS, lib.loc = "~/Documents/rpackages")


pricelistings <- read_html(
  str_c("https://www.airbnb.com/s/New-York--NY--United-States?ss_id=bzd8jajr&page=1&s_tag=1cat3lzi")) %>% 
  html_nodes(".price-amount") %>% 
  html_text()

for(i in 2:100) {
  pricelistings <- c(
    pricelistings,
    read_html(
      str_c("https://www.airbnb.com/s/New-York--NY--United-States?ss_id=bzd8jajr&page=", 
            i,
            "&s_tag=1cat3lzi")) %>% 
      html_nodes(".price-amount") %>% 
      html_text()
  )
  print(i)
  Sys.sleep(sample(seq(7, 10, .1), size = 1))
}

write_csv(as.data.frame(pricelistings), "NYprices")
#Initialize first page of IDs then read in subsequent pages and add to 
#vector containing IDs 
IDs <- read_html(
  str_c("https://www.airbnb.com/s/New-York--NY--United-States?ss_id=bzd8jajr&page=1&s_tag=1cat3lzi")) %>% 
  html_nodes("a.media-photo.media-cover") %>% 
  html_attr("data-reactid") %>% 
  str_extract_all("\\$[0-9]*\\.") %>% 
  str_replace_all("\\$([0-9]*)\\.", "\\1")

for(i in 2:100) {
  IDs<- c(
    IDs,
    read_html(
      str_c("https://www.airbnb.com/s/New-York--NY--United-States?ss_id=bzd8jajr&page=", 
            i,
            "&s_tag=1cat3lzi")) %>% 
      html_nodes("a.media-photo.media-cover") %>% 
      html_attr("data-reactid") %>% 
      str_extract_all("\\$[0-9]*\\.") %>% 
      str_replace_all("\\$([0-9]*)\\.", "\\1"))
  print(i)
  Sys.sleep(sampe(seq(10, 15, .1), size = 1))
}

write_csv(as.data.frame(IDs), "NYids")
#uses relevant nodes from listing pages to create vector composed of 
#elements consisting of the variable/ listing detail, followed by the 
#value 
attr_list <- list()
for(i in 1:length(IDs)) {
  attr_list[[i]] <- read_html(
    str_c("https://www.airbnb.com/rooms/",
          IDs[i],
          "?s=Cv-p6rL_")) %>% 
    html_nodes("div.col-md-6 div") %>% 
    html_text()
  print(i)
  Sys.sleep(sample(seq(7, 11, .1), size = 1))
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


WriteXLS(super_table, "NYdf")




