# Load required libraries
library(data.table)
library(tidyverse)
library(dplyr)
# Set working directory
setwd("D:/Year2Sem2/Data Science/AayushPradhanDataSciAssignment")
# Read cleaned house prices data
Cleaned_HP <- read.csv("cleanData/Cleaned_Town_population.csv")
# Read LSOA data
LSOA <- fread("Datasets/Postcode to LSOA.csv")
# Remove part of the postcode using pattern
pattern <- ' .*$'
# Clean and organize LSOA data
LSOA_Cleaned <- LSOA %>%
  select(lsoa11cd, pcds) %>% 
  mutate(shortPostcode = gsub(pattern, "", pcds)) %>% 
  right_join(Cleaned_HP, by = "shortPostcode")  %>% 
  group_by(lsoa11cd) %>% 
  select(lsoa11cd, shortPostcode, Town, District, County) 
# Rename the column
colnames(LSOA_Cleaned)[1] <- "LSOA code"
# Display cleaned LSOA data
View(LSOA_Cleaned)
# Write the cleaned LSOA data to a CSV file
write.csv(LSOA_Cleaned, "cleanData/Cleaned_LSOA.csv", row.names = FALSE, col.names = FALSE)
