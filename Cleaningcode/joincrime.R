library(data.table)
library(tidyverse)
library(dplyr)


setwd("D:/Year2Sem2/Data Science/AayushPradhanDataSciAssignment/Datasets/CrimeDatacombined/CombinedCrime_Data.csv")
dir <- getwd()
folders <- list.dirs()
folders <- folders[-1]
file_list <- list()

for (i in folders) {
  formatted_i <- gsub(pattern = '^.|/', replacement = '', x = i)
  inside_dir <- file.path(dir, formatted_i)
  files <- list.files(path = inside_dir, pattern = ".csv", full.names = TRUE)
  file_list <- c(file_list, files)
}

CombinedCrime_Data <- data.table()  # Create an empty data.table

for (i in file_list) {
  print(i)
  temp <- fread(i)
  CombinedCrime_Data <- rbindlist(list(CombinedCrime_Data, temp))
}

if (!file.exists("combined")) {
  dir.create("combined")
}

# Write the combined data to the CSV file
fwrite(CombinedCrime_Data, "combined/CombinedCrime_Data.csv", row.names = FALSE)







