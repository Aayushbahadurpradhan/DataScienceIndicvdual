# Load required libraries
library(tidyverse)
library(dplyr)
library(stringi)
library(scales)
library(data.table)
# Set working directory
setwd("D:/Year2Sem2/Data Science/AayushPradhanDataSciAssignment")
# Importing CSV files for different years
House_Price_2019 = fread("Datasets\\House Price-2019.csv")
House_Price_2020 = fread("Datasets\\House Price-2020.csv")
House_Price_2021 = fread("Datasets\\House Price-2021.csv")
House_Price_2022 = fread("Datasets\\House Price-2022.csv")

# Merging and selecting relevant columns
House_Price = rbind(House_Price_2019, House_Price_2020, House_Price_2021, House_Price_2022) %>% 
  select(c(1, 2, 3, 4, 12, 13, 14, 15, 16)) %>% 
  as_tibble()
# Naming Columns
names(House_Price) = c("ID", "PRICE", "DATE", "POSTCODE", "TOWN", "DISTRICT", "COUNTY", "TYPE 1", "TYPE 2")
# Cleaning the dataset
Cleaned_House_Price <- House_Price %>%
  filter(COUNTY %in% c("NORTH YORKSHIRE","WEST YORKSHIRE","SOUTH YORKSHIRE","EAST RIDING OF YORKSHIRE", "OXFORDSHIRE")) %>%
  mutate(SHORTPOSTCODE = gsub(" .*$", "", POSTCODE),
         YEAR = year(parse_date_time(DATE, orders = c("%m/%d/%Y %H:%M", "%Y-%m-%d %H:%M")))) %>%
  filter(!is.na(SHORTPOSTCODE) & !is.na(YEAR)) %>%
  group_by(COUNTY, SHORTPOSTCODE, TOWN, DISTRICT, YEAR) %>%
  summarize(AVGPRICE = mean(PRICE, na.rm = TRUE)) %>%
  ungroup()
# Save the cleaned dataset to a CSV file
write.csv(Cleaned_House_Price,"CleanData/Clean_Housepricing2019-2022.csv", row.names = FALSE)

