# Loading necessary libraries
library(tidyverse)
library(dplyr)
library(stringi)
library(scales)
# Setting the working directory
setwd("D:/Year2Sem2/Data Science/AayushPradhanDataSciAssignment")
# Reading the CSV file into the 'Broadband' dataframe
Broadband = read_csv("Datasets/Broadband_Speed/201805_fixed_pc_performance_r03.csv", show_col_types = FALSE)
# Defining a regex pattern to match a space followed by any characters until the end of the string
pattern = ' .*$'
# Creating a new dataframe 'BroadbandData' by performing various data manipulation tasks using dplyr functions
BroadbandData = Broadband %>%
  # Creating a new column 'shortPostcode' by removing the matched pattern from the 'postcode_space' column
  mutate(shortPostcode = gsub(pattern, "", postcode_space)) %>% 
  # Adding a new column 'ID' with row numbers
  mutate(ID = row_number()) %>% 
  # Selecting specific columns and renaming them for clarity
  select(`ID`, `postcode area`, shortPostcode, `Average download speed (Mbit/s)`,
         `Average upload speed (Mbit/s)`, `Minimum download speed (Mbit/s)`,
         `Minimum upload speed (Mbit/s)`) %>% 
  # Removing rows with missing values
  na.omit()
# Renaming the column names of 'BroadbandData'
colnames(BroadbandData) = c("ID", "postcode area", "shortPostcode", "Avgdownload",
                            "Average upload speed (Mbit/s)", "Mindownload",
                            "Minimum upload speed (Mbit/s)")
# Writing the cleaned dataframe 'BroadbandData' to a new CSV file
write.csv(BroadbandData, "cleanData/Cleaned_Broadband_Speed.csv", row.names = FALSE)
