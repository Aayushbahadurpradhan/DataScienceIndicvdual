# Loading necessary libraries
library(tidyverse)
library(stringr)
library(lubridate)
library(data.table)
# Setting the working directory
setwd("D:/Year2Sem2/Data Science/AayushPradhanDataSciAssignment")
# Cleaning crime data
crimedata <- fread('Datasets/CombinedCrime_Data.csv') %>% 
  select(Month, `LSOA code`, `Crime type`)
# Renaming column names for clarity
colnames(crimedata) <- c("Year", "LSOA.code", "CrimeType")
# Defining a regex pattern
pattern <- ' .*$'
# Reading LSOA to Postcode mapping data
LsoaToPostcode <- fread('CleanData/Cleaned_LSOA.csv')
colnames(LsoaToPostcode) <- c("LSOA.code", "shortPostcode", "Town", "District", "County")
# Cleaning and transforming crime data
crimedataCleaned <- crimedata %>%
  slice(1:900000) %>% 
  left_join(LsoaToPostcode, by = "LSOA.code") %>% 
  mutate(Year = str_trim(substring(Year, 1, 4))) %>% 
  group_by(shortPostcode, CrimeType, Year) %>% 
  select(shortPostcode, Year, CrimeType) %>% 
  na.omit() %>% 
  tally() %>% 
  as_tibble()
# Display cleaned crime data
View(crimedataCleaned)
# Writing cleaned crime data to a CSV file
write.csv(crimedataCleaned, "CleanData/Cleaned_Crime_Data.csv", row.names = FALSE)
