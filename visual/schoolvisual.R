# Load required libraries
library(tidyverse)
library(scales)
library(ggrepel)

# Set working directory
setwd("D:/Year2Sem2/Data Science/AayushPradhanDataSciAssignment")


# Read data
schoolData <- read_csv('cleanData/Cleaned_School_Data.csv', show_col_types = FALSE)
OXFORDSHIREschool <- read_csv("cleanData/OXFORSSHIRESchoolData.csv")
YORKSHIREschool <- read_csv('cleanData/YORKSHIRESchoolData.csv')

# Read Town data
Town <- read_csv("cleanData/Cleaned_Town_population.csv") %>% 
  select(shortPostcode, District)

# Join Town data with school data
schoolData <- schoolData %>% 
  left_join(Town, by = "shortPostcode") %>% 
  na.omit() 


ggplot(schoolData)+
  geom_boxplot(aes(x=Attainment8Score,y=District)) + 
  ggtitle("Boxplot of Average Total Attaintment per District")

# Line chart: Oxfordshire District Average Attainment Score from 2021 - 2022
oxford_chart <- OXFORDSHIREschool %>%
  inner_join(Town, by = "shortPostcode") %>% 
  na.omit() %>% 
  filter(Year %in% c(2021, 2022)) %>%
  group_by(District, Year) %>%
  summarise(AverageAttainment = mean(Attainment8Score), .groups = "drop") %>%
  
  ggplot(aes(x = Year, y = AverageAttainment, group = District, color = District)) +
  geom_line(size = 1) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(data = . %>% filter(Year == max(Year)), aes(label = AverageAttainment),
                  nudge_x = 0.3, nudge_y = 1, size = 4) +
  labs(title = "Oxfordshire District Average Attainment Score from 2021 to 2022") +
  theme_minimal() +
  theme(legend.position = "none")

# Print the Oxfordshire line chart
print(oxford_chart)

# Line chart: Yorkshire District Average Attainment Score from 2020 - 2022
merged_data_york <- YORKSHIREschool %>%
  inner_join(Town, by = "shortPostcode") %>% 
  ungroup() # Ungroup to remove grouping

filtered_data_york <- merged_data_york %>%
  filter(Year %in% c(2021, 2022)) %>%
  group_by(District, Year) %>%
  summarise(AverageAttainment = mean(Attainment8Score), .groups = "drop") # Specify .groups argument

york_chart <- filtered_data_york %>%
  ggplot(aes(x = Year, y = AverageAttainment, group = District, color = District)) +
  geom_line(size = 1) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(data = . %>% filter(Year == max(Year)), aes(label = AverageAttainment),
                  nudge_x = 0.3, nudge_y = 1, size = 4) +
  labs(title = "Yorkshire District Average Attainment Score from 2021 to 2022") +
  theme_minimal() +
  theme(legend.position = "none")

# Print the Yorkshire line chart
print(york_chart)
