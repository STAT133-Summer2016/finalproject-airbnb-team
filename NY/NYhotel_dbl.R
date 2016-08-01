library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(xml2)

#Page 1
dbl_url1 = read_html("http://www.trivago.com/?cpt=3481203&r=&iRoomType=7&aHotelTestClassifier=&iIncludeAll=0&aPartner=&iPathId=34812&aDateRange%5Barr%5D=2016-08-28&aDateRange%5Bdep%5D=2016-08-29&iGeoDistanceItem=0&iViewType=0&bIsSeoPage=false&bIsSitemap=false&")

##Name of Hotel
dbl_name1 = dbl_url1 %>% 
  html_nodes(".item__details .item__name__copytext") %>% 
  html_text() %>% 
  as.data.frame()

names(dbl_name1) = "Name"

dbl_name1 = dbl_name1 %>%
  mutate(a = c(1:25))

##Rating of Hotel
dbl_rating1 = dbl_url1 %>% 
  html_nodes(".item__review .rating-number__value") %>%
  html_text() %>% 
  as.data.frame()

names(dbl_rating1) = "Rating"

dbl_rating1 = dbl_rating1 %>% 
  mutate(a = c(1:25))


##Price of Hotel
dbl_price1 = dbl_url1 %>% 
  html_nodes(".item__best-details .item__best-price") %>% 
  html_text() %>% 
  as.data.frame()

names(dbl_price1) = "Price"

dbl_price1 = dbl_price1 %>% 
  mutate(a = c(1:25))

##Combine all into one data frame
page1 = full_join(dbl_name1, dbl_price1) %>% 
  full_join(dbl_rating1)

#Page 2
dbl_url2 = read_html("http://www.trivago.com/?iPathId=34812&bDispMoreFilter=false&aDateRange%5Barr%5D=2016-08-28&aDateRange%5Bdep%5D=2016-08-29&aCategoryRange=0%2C1%2C2%2C3%2C4%2C5&iRoomType=7&sOrderBy=relevance%20desc&aPartner=&aOverallLiking=1%2C2%2C3%2C4%2C5&iOffset=25&iLimit=25&iIncludeAll=0&bTopDealsOnly=false&iViewType=0&aPriceRange%5Bfrom%5D=0&aPriceRange%5Bto%5D=0&aGeoCode%5Blng%5D=-73.985161&aGeoCode%5Blat%5D=40.75885&bIsSeoPage=false&mgo=false&th=false&aHotelTestClassifier=&bSharedRooms=false&bIsSitemap=false&rp=&cpt=3481203&iFilterTab=0&")

##Name of hotel
dbl_name2 = dbl_url2 %>% 
  html_nodes(".item__details .item__name__copytext") %>% 
  html_text() %>% 
  as.data.frame()

names(dbl_name2) = "Name"

dbl_name2 = dbl_name2 %>%
  mutate(a = c(1:25))

##Rating of Hotel
dbl_rating2 = dbl_url2 %>% 
  html_nodes(".item__review .rating-number__value") %>%
  html_text() %>% 
  as.data.frame()

names(dbl_rating2) = "Rating"

dbl_rating2 = dbl_rating2 %>% 
  mutate(a = c(1:25))

##Price of Hotel
dbl_price2 = dbl_url2 %>% 
  html_nodes(".item__best-details .item__best-price") %>% 
  html_text() %>% 
  as.data.frame()

names(dbl_price2) = "Price"

dbl_price2 = dbl_price2 %>% 
  mutate(a = c(1:25))

##Combine all into one data frame
page2 = full_join(dbl_name2, dbl_price2) %>% 
  full_join(dbl_rating2)


#Page 3
dbl_url3 = read_html("http://www.trivago.com/?iPathId=34812&bDispMoreFilter=false&aDateRange%5Barr%5D=2016-08-28&aDateRange%5Bdep%5D=2016-08-29&aCategoryRange=0%2C1%2C2%2C3%2C4%2C5&iRoomType=7&sOrderBy=relevance%20desc&aPartner=&aOverallLiking=1%2C2%2C3%2C4%2C5&iOffset=50&iLimit=25&iIncludeAll=0&bTopDealsOnly=false&iViewType=0&aPriceRange%5Bto%5D=0&aPriceRange%5Bfrom%5D=0&aGeoCode%5Blng%5D=-73.985161&aGeoCode%5Blat%5D=40.75885&bIsSeoPage=false&mgo=false&th=false&aHotelTestClassifier=&bSharedRooms=false&bIsSitemap=false&rp=&cpt=3481203&iFilterTab=0&")

##Name of hotel
dbl_name3 = dbl_url3 %>% 
  html_nodes(".item__details .item__name__copytext") %>% 
  html_text() %>% 
  as.data.frame()

names(dbl_name3) = "Name"

dbl_name3 = dbl_name3 %>%
  mutate(a = c(1:25))

##Rating of Hotel
dbl_rating3 = dbl_url3 %>% 
  html_nodes(".item__review .rating-number__value") %>%
  html_text() %>% 
  as.data.frame()

names(dbl_rating3) = "Rating"

dbl_rating3 = dbl_rating3 %>% 
  mutate(a = c(1:25))


##Price of Hotel
dbl_price3 = dbl_url3 %>% 
  html_nodes(".item__best-details .item__best-price") %>% 
  html_text() %>% 
  as.data.frame()

names(dbl_price3) = "Price"

dbl_price3 = dbl_price3 %>% 
  mutate(a = c(1:25))

##Combine all into one data frame
page3 = full_join(dbl_name3, dbl_price3) %>% 
  full_join(dbl_rating2)


dbl_url4 = read_html("http://www.trivago.com/?iPathId=34812&bDispMoreFilter=false&aDateRange%5Barr%5D=2016-08-28&aDateRange%5Bdep%5D=2016-08-29&aCategoryRange=0%2C1%2C2%2C3%2C4%2C5&iRoomType=7&sOrderBy=relevance%20desc&aPartner=&aOverallLiking=1%2C2%2C3%2C4%2C5&iOffset=75&iLimit=25&iIncludeAll=0&bTopDealsOnly=false&iViewType=0&aPriceRange%5Bto%5D=0&aPriceRange%5Bfrom%5D=0&aGeoCode%5Blng%5D=-73.985161&aGeoCode%5Blat%5D=40.75885&bIsSeoPage=false&mgo=false&th=false&aHotelTestClassifier=&bSharedRooms=false&bIsSitemap=false&rp=&cpt=3481203&iFilterTab=0&")

##Name of hotel
dbl_name4 = dbl_url4 %>% 
  html_nodes(".item__details .item__name__copytext") %>% 
  html_text() %>% 
  as.data.frame()

names(dbl_name4) = "Name"

dbl_name4 = dbl_name4 %>%
  mutate(a = c(1:25))

##Rating of Hotel
dbl_rating4 = dbl_url4 %>% 
  html_nodes(".item__review .rating-number__value") %>%
  html_text() %>% 
  as.data.frame()

names(dbl_rating4) = "Rating"

dbl_rating4 = dbl_rating4 %>% 
  mutate(a = c(1:25))


##Price of Hotel
dbl_price4 = dbl_url4 %>% 
  html_nodes(".item__best-details .item__best-price") %>% 
  html_text() %>% 
  as.data.frame()

names(dbl_price4) = "Price"

dbl_price4 = dbl_price4 %>% 
  mutate(a = c(1:25))


##Combine all into one data frame
page4 = full_join(dbl_name4, dbl_price4) %>% 
  full_join(dbl_rating4)

dbl_final = rbind(page1, page2, page3, page4) %>% 
  select(-a)

write_csv(dbl_final, "ny_dbl.csv")

