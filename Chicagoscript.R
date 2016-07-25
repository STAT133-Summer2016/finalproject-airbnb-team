
library(rvest)
library(dplyr)
library(stringr)
library(readr)
library(jsonlite)
library(magrittr)
library(selectr)

pricelistings <- read_html(
  str_c("https://www.airbnb.com/s/chicago?ss_id=4rih9757&page=1&s_tag=TPHZXwbi")) %>% 
  html_nodes(".price-amount") %>% 
  html_text()

for(i in 2:100) {
  pricelistings <- c(
    pricelistings,
    read_html(
      str_c("https://www.airbnb.com/s/chicago?ss_id=4rih9757&page=", 
            i,
            "&s_tag=TPHZXwbi")) %>% 
      html_nodes(".price-amount") %>% 
      html_text()
  )
}
#Initialize first page of IDs then read in subsequent pages and add to 
#vector containing IDs 
IDs <- read_html(
  str_c("https://www.airbnb.com/s/chicago?ss_id=4rih9757&page=1&s_tag=TPHZXwbi")) %>% 
  html_nodes("a.media-photo.media-cover") %>% 
  html_attr("data-reactid") %>% 
  str_extract_all("\\$[0-9]*\\.") %>% 
  str_replace_all("\\$([0-9]*)\\.", "\\1")

for(i in 2:100) {
  IDs<- c(
    IDs,
    read_html(
      str_c("https://www.airbnb.com/s/chicago?ss_id=4rih9757&page=", 
            i,
            "&s_tag=TPHZXwbi")) %>% 
      html_nodes("a.media-photo.media-cover") %>% 
      html_attr("data-reactid") %>% 
      str_extract_all("\\$[0-9]*\\.") %>% 
      str_replace_all("\\$([0-9]*)\\.", "\\1"))
}

#uses relevant nodes from listing pages to create vector composed of 
#elements consisting of the variable/ listing detail, followed by the 
#value 
attr_list <- list()
for(i in 1:length(IDs)) {
  attr_list[[i]] <- read_html(
    str_c("https://www.airbnb.com/rooms/",
          IDs[i],
          "?s=TPHZXwbi")) %>% 
    html_nodes("div.col-md-6 div") %>% 
    html_text()
}
#initialize empty lists to fill 
colnames <- list()
colvals <- list()
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
}
#join current element with the aggregate table 
#comprised of previous elements 
for(i in 2:length(df_list)) {
  super_table <- full_join(df_list[[i]], super_table) 
}

super_table <- super_table %>% 
  mutate(price = pricelistings)

WriteXLS(super_table, "Chicagodf")




