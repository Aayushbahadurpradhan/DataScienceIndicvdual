# Load required libraries
library(tidyverse)
library(dplyr)
library(scales)
library(fmsb)
library(ggrepel)

# Set working directory
setwd("D:/Year2Sem2/Data Science/AayushPradhanDataSciAssignment")

# Read data
Town <- read_csv("cleanData/Cleaned_Town_population.csv") %>% 
  select(-Year)
crime_Data <- read_csv("cleanData/Cleaned_Crime_Data.csv")

# Join data and handle missing values
crimeData <- crime_Data %>% 
  left_join(Town, by = "shortPostcode") %>% 
  na.omit()

# Update County names
crimeData$County[crimeData$County %in% c("WEST YORKSHIRE", "NORTH YORKSHIRE", "SOUTH YORKSHIRE")] <- "YORKSHIRE"

# Drug Offence Rate per 10000 in 2021-2022
filtered_data <- crimeData %>%
  filter(County %in% c("OXFORDSHIRE", "YORKSHIRE"),
         Year %in% c(2021, 2022),
         CrimeType == "Drugs") %>% 
  mutate(DrugOffenceRate = (CrimeCount / (Population2021 + Population2022)) * 10000)

ggplot(data = filtered_data, aes(x = District, y = DrugOffenceRate, fill = County)) +
  geom_boxplot() +
  labs(title = "Drug Offence Rate per 10000 in 2021-2022") +
  coord_flip()

# Piechart for 2021 Robbery
RobberyData <- crimeData %>% 
  filter(CrimeType == "Robbery", Year == 2022) %>%
  group_by(District) %>%
  summarise(sumCount = sum(CrimeCount)) %>% 
  mutate(perc = sumCount / sum(sumCount),
         labels = scales::percent(perc))

ggplot(RobberyData, aes(x = "", y = perc, fill = District)) +
  geom_col(color = "white") +
  geom_label(aes(label = labels), color = "black",
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "2022 Robbery by District")

# Filter and calculate drug offense rate per county for years 2021 and 2022
Drugs_2021_2022 <- crimeData %>%
  filter(County %in% c("OXFORDSHIRE", "YORKSHIRE"),
         Year %in% c(2021, 2022),
         CrimeType == "Drugs") %>% 
  mutate(DrugOffenceRate = (CrimeCount / (Population2021 + Population2022)) * 10000)

# Create a line chart
ggplot(Drugs_2021_2022, aes(x = Year, y = DrugOffenceRate, color = County)) +
  geom_line() +
  labs(x = "Year", y = "Drug Offense Rate per 10000 People",
       title = "Drug Offense Rate per 10000 People from 2021 to 2022",
       color = "County")

# Filter and calculate vehicle crime count per district for years 2021 and 2022
crime_totals <- crimeData %>%
  filter(CrimeType == "Vehicle crime", Year %in% c(2021, 2022)) %>%
  group_by(District, Year) %>%
  summarise(TotalCrime = sum(CrimeCount)) %>%
  ungroup()  # This line removes the grouping, which might have caused the issue

# Pivot the data to have years as columns
pivot_data <- crime_totals %>%
  pivot_wider(names_from = Year, values_from = TotalCrime)

# Create the radar chart
radar_chart <- radarchart(pivot_data,
                          axistype = 1,
                          seg = 4,
                          pcol = c("blue", "red"),
                          plty = c(1, 2),
                          plwd = c(2, 2),
                          vlabels = colnames(pivot_data)[-1],  # Exclude the "District" label
                          title = "Vehicle Crime Rate 2021-2022")

# Print the radar chart
print(radar_chart)
