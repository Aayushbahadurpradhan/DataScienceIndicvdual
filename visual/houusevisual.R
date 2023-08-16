# Install and load required packages
install.packages("fmsb")
install.packages("ggrepel")
library(tidyverse)
library(dplyr)
library(scales)
library(fmsb)
library(ggrepel)
library(data.table)
# Set working directory
setwd("D:/Year2Sem2/Data Science/AayushPradhanDataSciAssignment")
# Define currency format
euro <- dollar_format(prefix = "\u20ac", big.mark = ",")
# Import the cleaned house prices CSV
Cleaned_HP <- fread("CleanData/Clean_Housepricing2019-2022.csv")
# House Price average of each town from 2019 to 2022 (Box Plot)
boxplot_plot <- ggplot(data = Cleaned_HP,
                       aes(x = DISTRICT, y = AVGPRICE, fill = DISTRICT)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma, limits = c(0, 1000000)) +
  labs(title = "House Pricing in Different DISTRICT: 2019-2022 (Box Plot)") +
  theme_minimal()
# Average House Price of each town from 2019 to 2022 (Bar Chart)
barplot_plot <- ggplot(data = Cleaned_HP,
                       aes(x = DISTRICT, y = AVGPRICE, fill = DISTRICT)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma, limits = c(0, 1000000)) +
  labs(title = "Average House Prices by DISTRICT: 2019-2022 (Bar Chart)") +
  theme_minimal()
# Line Graph of average house price of the 2 counties from 2019 to 2022 (Line Graph)
lineplot_plot <- ggplot(Cleaned_HP %>% 
                          group_by(DISTRICT, YEAR) %>% 
                          summarize(AVGPRICE = mean(AVGPRICE, na.rm = TRUE)),
                        aes(x = YEAR, y = AVGPRICE, color = DISTRICT, group = DISTRICT)) +
  geom_line(size = 2) +
  scale_x_continuous(breaks = c(2019, 2020, 2021, 2022)) +
  labs(title = "Average House Prices by County: 2019-2022 (Line Graph)",
       x = "Year", y = "Average Price") +
  scale_color_discrete(name = "DISTRICT") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal"
  ) +
  scale_color_brewer(palette = "Paired") +
  labs(fill = "County") +
  geom_point(size = 4, shape = 21, fill = "white", stroke = 2) +
  geom_label_repel(
    aes(label = euro(AVGPRICE), fill = DISTRICT),
    nudge_y = 10000,
    nudge_x = 0.2,
    size = 3,
    show.legend = FALSE
  )

# Displaying the plots
print(boxplot_plot)
print(barplot_plot)
print(lineplot_plot)

