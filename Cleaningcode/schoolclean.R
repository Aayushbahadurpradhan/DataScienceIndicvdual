# Load required libraries
library(tidyverse)
library(dplyr)
library(stringi)
library(scales)
library(data.table)
# Set working directory
setwd("D:/Year2Sem2/Data Science/AayushPradhanDataSciAssignment")
# Reading and combining House Price Data
HousePrices <- read.csv("cleanData/Combined_House_Pricing_2019-2022.csv")
# Reading and combining School Data
# School_2017_2018
School_2017_2018 <- fread("Datasets/School_2017-2018/2017-2018/england_ks4final.csv", fill = TRUE) %>% 
  mutate(Year = 2017) %>% 
  select(Year, PCODE, SCHNAME, ATT8SCR) %>%  
  na.omit() %>% 
  distinct()
# ... Similar steps for School_2018_2019, School_2019_2020, School_2021_2022, School_2022_2023 ...
# Combining School Data from different years
SchoolData <- rbind(School_2017_2018, School_2018_2019, School_2019_2020, School_2021_2022, School_2022_2023)
write.csv(SchoolData, "cleanData/Combined_School_Data.csv", row.names = FALSE)
# Pattern for removing part of the postcode
pattern <- ' .*$'
# Cleaning School Data
CleanedSchoolData <- SchoolData %>% 
  mutate(ID = row_number()) %>% 
  mutate(shortPostcode = gsub(pattern, "", PCODE)) %>%
  filter(ATT8SCR != "NE" & ATT8SCR != "SUPP") %>% 
  filter(ATT8SCR != "" & shortPostcode != "" & PCODE != "") %>% 
  select(ID, Year, PCODE, shortPostcode, SCHNAME, ATT8SCR) %>% 
  na.omit() %>% 
  distinct()
# ... Similar steps for cleaning House Price Data ...
# Extracting County information from House Price Data
Post <- read.csv("cleanData/Combined_House_Pricing_2019-2022.csv") %>% 
  select(PostCode, County) %>% 
  mutate(shortPostcode = gsub(pattern, "", PostCode)) %>% 
  select(County, shortPostcode)
# Cleaning School Data for OXFORDSHIRE
OXFORSSHIRESchoolData <- CleanedSchoolData %>% 
  left_join(Post, by = "shortPostcode") %>% 
  select(ID, Year, PostCode, shortPostcode, SCHNAME, ATT8SCR, County) %>% 
  filter(County == "OXFORDSHIRE") %>% 
  na.omit() %>% 
  distinct() %>% 
  mutate(ID = row_number()) %>% 
  select(ID, Year, PostCode, shortPostcode, SCHNAME, ATT8SCR)
# Cleaning School Data for YORKSHIRE
YORKSHIRESchoolData <- CleanedSchoolData %>% 
  left_join(Post, by = "shortPostcode") %>% 
  select(ID, Year, PostCode, shortPostcode, SCHNAME, ATT8SCR, County) %>% 
  filter(County %in% c("YORK", "WEST YORKSHIRE", "SOUTH YORKSHIRE", "NORTH YORKSHIRE")) %>% 
  na.omit() %>% 
  distinct() %>% 
  mutate(ID = row_number()) %>% 
  select(ID, Year, PostCode, shortPostcode, SCHNAME, ATT8SCR)
