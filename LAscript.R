
library(rvest)
library(dplyr)
library(stringr)
library(readr)
library(jsonlite)

#creates list of listing IDs by selecting node attribute whose value contains
#the ID and then splicing it out of the attribute 
list <- list() 
for(i in 1:100) {
  list[[i]]<- c(read_html(
    str_c("https://www.airbnb.com/s/Los-Angeles--CA--United-States?zoom=10&search_by_map=true&sw_lat=33.67235947194487&sw_lng=-118.63454013866334&ne_lat=34.21075767955577&ne_lng=-118.12092929881959&ss_id=6ftue19q&page=",
          i,
          "&s_tag=Cv-p6rL_")) %>% 
      html_nodes("a.media-photo.media-cover") %>% 
      html_attr("data-reactid") %>% 
      str_extract_all("\\$[0-9]*\\.") %>% 
      str_replace_all("\\$([0-9]*)\\.", "\\1"))
}

#uses relevant nodes from listing pages to create vector composed of 
#elements consisting of the variable/ listing detail, followed by the 
#value 
attr_list <- list()
for(i in 1:100) {
  for(a in 1:18) {
  attr_list[[(i-1)*18 + a]] <- read_html(
          str_c("https://www.airbnb.com/rooms/", 
                list[[i]][a], 
                "?s=Cv-p6rL_")) %>% 
    html_nodes("div.col-md-6 div") %>% 
    html_text()
  }
}
#initialize empty lists to fill 
colnames <- list()
colvals <- list()
#initialize empty list to fill with single row data frames (as loop proceeds)
df_list <- list()
#removing variable names from first element of guest_number to start loop 
colnames[[1]] <- attr_list[[1]] %>% 
  str_replace_all("(.*):.*", "\\1")
#values for variables of first element to start loop 
colvals[[1]] <- attr_list[[1]] %>% 
  str_replace_all(".*:(.*)", "\\1")
#generate first data frame to join
super_table <- as.data.frame(
  structure(colvals[[1]], 
            names = colnames[[1]],
            class = "list"))
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
              class = "list"))
  #join current element with the aggregate table 
  #comprised of previous elements 
  super_table <- full_join(df_list[[i]], super_table) 
}


