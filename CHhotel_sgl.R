library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(xml2)

#Page 1
sgl_url1 = read_html("http://www.trivago.com/?cpt=3447003&r=&iRoomType=1&aHotelTestClassifier=&iIncludeAll=0&aPartner=&iPathId=34470&aDateRange%5Barr%5D=2016-08-28&aDateRange%5Bdep%5D=2016-08-29&iGeoDistanceItem=0&iViewType=0&bIsSeoPage=false&bIsSitemap=false&")

##Name of Hotel
sgl_name1 = sgl_url1 %>% 
  html_nodes(".item__details .item__name__copytext") %>% 
  html_text() %>% 
  as.data.frame()

names(sgl_name1) = "Name"

sgl_name1 = sgl_name1 %>%
  mutate(a = c(1:25))

##Rating of Hotel
sgl_rating1 = sgl_url1 %>% 
  html_nodes(".item__review .rating-number__value") %>%
  html_text() %>% 
  as.data.frame()

names(sgl_rating1) = "Rating"

sgl_rating1 = sgl_rating1 %>% 
  mutate(a = c(1:25))


##Price of Hotel
sgl_price1 = sgl_url1 %>% 
  html_nodes(".item__best-details .item__best-price") %>% 
  html_text() %>% 
  as.data.frame()

names(sgl_price1) = "Price"

sgl_price1 = sgl_price1 %>% 
  mutate(a = c(1:25))

##Combine all into one data frame
page1 = full_join(sgl_name1, sgl_price1) %>% 
  full_join(sgl_rating1)

#Page 2
sgl_url2 = read_html("http://www.trivago.com/?iPathId=34470&bDispMoreFilter=false&aDateRange%5Barr%5D=2016-08-28&aDateRange%5Bdep%5D=2016-08-29&aCategoryRange=0%2C1%2C2%2C3%2C4%2C5&iRoomType=1&sOrderBy=relevance%20desc&aPartner=&aOverallLiking=1%2C2%2C3%2C4%2C5&iOffset=25&iLimit=25&iIncludeAll=0&bTopDealsOnly=false&iViewType=0&aPriceRange%5Bfrom%5D=0&aPriceRange%5Bto%5D=0&aGeoCode%5Blng%5D=-87.624039&aGeoCode%5Blat%5D=41.890442&bIsSeoPage=false&mgo=false&th=false&aHotelTestClassifier=&bSharedRooms=false&bIsSitemap=false&rp=&cpt=3447003&iFilterTab=0&")

##Name of hotel
sgl_name2 = sgl_url2 %>% 
  html_nodes(".item__details .item__name__copytext") %>% 
  html_text() %>% 
  as.data.frame()

names(sgl_name2) = "Name"

sgl_name2 = sgl_name2 %>%
  mutate(a = c(1:25))

##Rating of Hotel
sgl_rating2 = sgl_url2 %>% 
  html_nodes(".item__review .rating-number__value") %>%
  html_text() %>% 
  as.data.frame()

names(sgl_rating2) = "Rating"

sgl_rating2 = sgl_rating2 %>% 
  mutate(a = c(1:25))

##Price of Hotel
sgl_price2 = sgl_url2 %>% 
  html_nodes(".item__best-details .item__best-price") %>% 
  html_text() %>% 
  as.data.frame()

names(sgl_price2) = "Price"

sgl_price2 = sgl_price2 %>% 
  mutate(a = c(1:25))

##Combine all into one data frame
page2 = full_join(sgl_name2, sgl_price2) %>% 
  full_join(sgl_rating2)


#Page 3
sgl_url3 = read_html("http://www.trivago.com/?iPathId=34470&bDispMoreFilter=false&aDateRange%5Barr%5D=2016-08-28&aDateRange%5Bdep%5D=2016-08-29&aCategoryRange=0%2C1%2C2%2C3%2C4%2C5&iRoomType=1&sOrderBy=relevance%20desc&aPartner=&aOverallLiking=1%2C2%2C3%2C4%2C5&iOffset=50&iLimit=25&iIncludeAll=0&bTopDealsOnly=false&iViewType=0&aPriceRange%5Bto%5D=0&aPriceRange%5Bfrom%5D=0&aGeoCode%5Blng%5D=-87.624039&aGeoCode%5Blat%5D=41.890442&bIsSeoPage=false&mgo=false&th=false&aHotelTestClassifier=&bSharedRooms=false&bIsSitemap=false&rp=&cpt=3447003&iFilterTab=0&")

##Name of hotel
sgl_name3 = sgl_url3 %>% 
  html_nodes(".item__details .item__name__copytext") %>% 
  html_text() %>% 
  as.data.frame()

names(sgl_name3) = "Name"

sgl_name3 = sgl_name3 %>%
  mutate(a = c(1:25))

##Rating of Hotel
sgl_rating3 = sgl_url3 %>% 
  html_nodes(".item__review .rating-number__value") %>%
  html_text() %>% 
  as.data.frame()

names(sgl_rating3) = "Rating"

sgl_rating3 = sgl_rating3 %>% 
  mutate(a = c(1:25))


##Price of Hotel
sgl_price3 = sgl_url3 %>% 
  html_nodes(".item__best-details .item__best-price") %>% 
  html_text() %>% 
  as.data.frame()

names(sgl_price3) = "Price"

sgl_price3 = sgl_price3 %>% 
  mutate(a = c(1:25))

##Combine all into one data frame
page3 = full_join(sgl_name3, sgl_price3) %>% 
  full_join(sgl_rating2)


sgl_url4 = read_html("http://www.trivago.com/?iPathId=34470&bDispMoreFilter=false&aDateRange%5Barr%5D=2016-08-28&aDateRange%5Bdep%5D=2016-08-29&aCategoryRange=0%2C1%2C2%2C3%2C4%2C5&iRoomType=1&sOrderBy=relevance%20desc&aPartner=&aOverallLiking=1%2C2%2C3%2C4%2C5&iOffset=75&iLimit=25&iIncludeAll=0&bTopDealsOnly=false&iViewType=0&aPriceRange%5Bto%5D=0&aPriceRange%5Bfrom%5D=0&aGeoCode%5Blng%5D=-87.624039&aGeoCode%5Blat%5D=41.890442&bIsSeoPage=false&mgo=false&th=false&aHotelTestClassifier=&bSharedRooms=false&bIsSitemap=false&rp=&cpt=3447003&iFilterTab=0&")

##Name of hotel
sgl_name4 = sgl_url4 %>% 
  html_nodes(".item__details .item__name__copytext") %>% 
  html_text() %>% 
  as.data.frame()

names(sgl_name4) = "Name"

sgl_name4 = sgl_name4 %>%
  mutate(a = c(1:25))

##Rating of Hotel
sgl_rating4 = sgl_url4 %>% 
  html_nodes(".item__review .rating-number__value") %>%
  html_text() %>% 
  as.data.frame()

names(sgl_rating4) = "Rating"

sgl_rating4 = sgl_rating4 %>% 
  mutate(a = c(1:25))


##Price of Hotel
sgl_price4 = sgl_url4 %>% 
  html_nodes(".item__best-details .item__best-price") %>% 
  html_text() %>% 
  as.data.frame()

names(sgl_price4) = "Price"

sgl_price4 = sgl_price4 %>% 
  mutate(a = c(1:25))

##Combine all into one data frame
page4 = full_join(sgl_name4, sgl_price4) %>% 
  full_join(sgl_rating4)

sgl_final = rbind(page1, page2, page3, page4) %>% 
  select(-a)

write_csv(sgl_final, "ny_sgl.csv")
