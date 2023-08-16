library(tidyverse)
library(dplyr)
library(scales)
library(fmsb)
library(ggrepel)
setwd("D:/Year2Sem2/Data Science/AayushPradhanDataSciAssignment")
#---------------house rank------
Town = read_csv("cleanData/Cleaned_Town_Population.csv")%>% 
  select(shortPostcode, Town, District, County)
House_price = read_csv("cleanData/Cleaned_House_Pricing_2019-2022.csv")
#house rank
Houseprice= House_price %>%
  left_join(Town,by="shortPostcode") %>% 
  na.omit()
housePrices=Houseprice  %>% 
  filter(Year=="2020") %>% 
  group_by(Town) %>% 
  summarise(Price=mean(Price)) %>% 
  arrange(Price) %>% 
  mutate(HouseScore=10-(Price/120000)) %>% 
  select(Town, HouseScore)
housePrices
view(housePrices)

#download rank

speed_downloads = read_csv("cleanData/Cleaned_Broadband_Speed.csv")

Speed_Download = speed_downloads %>%
  left_join(Town,by="shortPostcode") %>% 
  na.omit()

download_speed=Speed_Download%>%
  group_by(Town) %>%
  summarise(downloadSpeed=Avgdownload) %>%
  arrange(downloadSpeed) %>%
  mutate(DownloadScore=10-(downloadSpeed/120000)) %>%
  select(Town,DownloadScore) %>%
  distinct(Town, .keep_all = TRUE)

view(download_speed)

#crime score rank
crime_score=read_csv("cleanData/Cleaned_Crime_Data.csv")
crime_rank = crime_score %>%
  left_join(Town,by="shortPostcode") %>% 
  na.omit()


crime_rank=crime_rank%>% 
  group_by(Town) %>% 
  summarise(score=mean(CrimeCount)) %>% 
  arrange(desc(score)) %>% 
  mutate(score=10-(score/1200)) %>% 
  select(Town,score)
view(crime_rank)

#school score
school_score=read_csv("cleanData/Cleaned_School_Data.csv")
school_rank = school_score %>%
  left_join(Town,by="shortPostcode") %>% 
  na.omit()
view(school_rank)

school_rank=school_rank%>% 
  group_by(District) %>% 
  slice(1:3) %>% 
  mutate(score=mean(Attainment8Score)) %>% 
  arrange(score) %>% 
  mutate(score=10-(score/1800)) %>% 
  select(District,score,SchoolName) %>% 
  distinct()
school_rank
view(school_rank)




