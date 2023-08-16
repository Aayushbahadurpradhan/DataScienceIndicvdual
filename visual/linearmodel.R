library(tidyverse)
library(dplyr)
library(scales)
library(fmsb)
library(ggrepel)

Towns = read_csv("cleanData/Cleaned_Town_population.csv")%>%
  select(shortPostcode, Town, District, County)

prices = read_csv("cleanData/Cleaned_House_Pricing_2019-2022.csv")

speeds = read_csv("cleanData/Cleaned_Broadband_Speed.csv") %>% 
  na.omit()  

crime=read_csv("cleanData/Cleaned_Crime_Data.csv")

schools=read_csv("cleanData\\Cleaned_School_Data.csv") %>% 
  na.omit()


#------------------------------House prices vs Download Speed----------------------------------------

options(scipen=999)

HousePrices = prices %>%
  filter(Year=="2021" | Year=="2022") %>%
  left_join(Towns,by="shortPostcode") %>%  
  group_by(Town,County) %>%
  summarise(Price=mean(Price))


BroardbandSpeeds = speeds %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,County) %>%
  summarise(AverageDownload=mean(Avgdownload))

lm_res = HousePrices %>% left_join(BroardbandSpeeds,by="Town")
model = lm(data= lm_res, Price~AverageDownload)
summary(model)

color= c("OXFORDSHIRE" = "red", "YORKSHIRE" = "blue")

ggplot(lm_res,aes(x=AverageDownload,y=Price)) +
  geom_point(data = filter(lm_res,County.x=="YORKSHIRE"),aes(color="YORKSHIRE"))+
  geom_point(data = filter(lm_res,County.x=="OXFORDSHIRE"), aes(color="OXFORDSHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="lightgreen")+
  labs(x="Download Speed (Mbit/s)",y="Price",title="House Prices vs Download Speed",color="County")


#-----------------------------------------------------------------------------------------
#----------------------------------House price and drug offence--------------------------------------------------

HousePrices = prices %>%
  filter(Year=="2021" | Year=="2022" ) %>%
  left_join(Towns,by="shortPostcode") %>%  
  group_by(Town,County) %>%
  summarise(Price=mean(Price))

Drugs = crime %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,County) %>%
  filter(CrimeType=="Drugs") %>% 
  na.omit()

lm_res1 = HousePrices %>% left_join(Drugs ,by="Town") %>% 
  na.omit()
model1 = lm(data= lm_res1, Price~CrimeCount)
summary(model1)

color= c("OXFORDSHIRE" = "yellow", "YORKSHIRE" = "Green")

ggplot(lm_res1,aes(x=CrimeCount,y=Price)) +
  geom_point(data = filter(lm_res1,County.x=="YORKSHIRE"),aes(color="YORKSHIRE"))+
  geom_point(data = filter(lm_res1,County.x=="OXFORDSHIRE"), aes(color="OXFORDSHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="peachpuff4")+
  labs(x="count",y="Price",title="House Prices vs Drug Rate",color="County")






#==================   average attainment  vs house prices ==================


attainment= schools %>%
  left_join(Towns,by="shortPostcode") %>%  
  group_by(Town,County) %>%
  summarise(meanAttainment=mean(Attainment8Score))

attainment

HousePrices = prices %>%
  left_join(Towns,by="shortPostcode") %>%  
  group_by(Town,County) %>%
  summarise(Price=mean(Price))

lm_res2 = HousePrices %>% left_join(attainment ,by="Town") %>% 
  na.omit()
lm_res2
model1 = lm(data= lm_res2, Price~meanAttainment)
summary(model1)

color= c("OXFORDSHIRE" = "yellow", "YORKSHIRE" = "Green")

ggplot(lm_res2,aes(x=meanAttainment,y=Price)) +
  geom_point(data = filter(lm_res2,County.x=="YORKSHIRE"),aes(color="YORKSHIRE"))+
  geom_point(data = filter(lm_res2,County.x=="OXFORDSHIRE"), aes(color="OXFORDSHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="peachpuff4")+
  labs(x="attainment",y="Price",title="average attainment  vs house prices",color="County")




#===========   download vs drug offence per 10000 people ========


# Read data files
Towns_Populations <- read_csv("cleanData/Cleaned_Town_population.csv")
speeds <- read_csv("cleanData/Cleaned_Broadband_Speed.csv")
crime <- read_csv("cleanData/Cleaned_Crime_Data.csv")

# Calculate average download speed per town and county
download_speeds <- speeds %>%
  left_join(Towns_Populations, by = "shortPostcode") %>% 
  group_by(Town, County) %>%
  summarise(meanDownloadSpeed = mean(Avgdownload))

# Filter and calculate drug offense rate per town and county
Drugs <- crime %>%
  left_join(Towns_Populations, by = "shortPostcode") %>%
  group_by(Town, County) %>%
  filter(CrimeType == "Drugs") %>% 
  mutate(DrugOffenceRate = (CrimeCount / (Population2019 + Population2020 + Population2021 + Population2022)) * 10000) %>%
  na.omit()

# Merge the data frames
lm_res3 <- Drugs %>%
  left_join(download_speeds, by = "Town")

# Fit linear regression model
model3 <- lm(DrugOffenceRate ~ meanDownloadSpeed, data = lm_res3)

# Create scatter plot
color <- c("OXFORDSHIRE" = "yellow", "YORKSHIRE" = "Green")

ggplot(lm_res3, aes(x = meanDownloadSpeed, y = DrugOffenceRate)) +
  geom_point(data = filter(lm_res3, County.x == "YORKSHIRE"), aes(color = "YORKSHIRE")) +
  geom_point(data = filter(lm_res3, County.x == "OXFORDSHIRE"), aes(color = "OXFORDSHIRE")) +
  geom_smooth(method = lm, se = FALSE, color = "peachpuff4") +
  labs(x = "Average Download Speed", y = "Drugs per 10,000 People", title = "Average Download Speed vs Drug Offense Rate", color = "County") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_manual(values = color) +
  annotate("text", x = 20, y = 250, label = "YORKSHIRE", color = color["YORKSHIRE"], hjust = 0, size = 4) +
  annotate("text", x = 20, y = 220, label = "OXFORDSHIRE", color = color["OXFORDSHIRE"], hjust = 0, size = 4) +
  geom_text_repel(data = subset(lm_res3, County.x %in% c("YORKSHIRE", "OXFORDSHIRE")), aes(label = Town), box.padding = unit(0.35, "lines"))

#=============drug rate vs average attenment core per 10000 people =======

attainment <- schools %>%
  left_join(Towns, by = "shortPostcode") %>%  
  group_by(Town, County) %>%
  summarise(meanAttainment = mean(Attainment8Score))

# Filter and calculate drug offense rate per town and county
Drugs <- crime %>%
  left_join(Towns_Populations, by = "shortPostcode") %>%
  group_by(Town, County) %>%
  filter(CrimeType == "Drugs") %>% 
  mutate(DrugOffenceRate = (CrimeCount / (Population2019 + Population2020 + Population2021 + Population2022)) * 10000) %>% 
  as_tibble() %>% 
  na.omit()

# Merge the data frames and remove rows with missing values
lm_res4 <- Drugs %>%
  left_join(attainment, by = "Town") %>% 
  na.omit()

# Fit linear regression model
model4 <- lm(DrugOffenceRate ~ meanAttainment, data = lm_res4)

summary(model4)

# Create a scatter plot
color <- c("OXFORDSHIRE" = "yellow", "YORKSHIRE" = "Green")

ggplot(lm_res4, aes(x = meanAttainment, y = DrugOffenceRate)) +
  geom_point(data = filter(lm_res4, County.x == "YORKSHIRE"), aes(color = "YORKSHIRE")) +
  geom_point(data = filter(lm_res4, County.x == "OXFORDSHIRE"), aes(color = "OXFORDSHIRE")) +
  geom_smooth(method = lm, se = FALSE, color = "peachpuff4") +
  labs(x = "Attainment", y = "Drugs per 10000", title = "Average Attainment Score vs Drug Offense per 10000 People", color = "County")



#========= average download speed vs average attainment score ======

# Calculate mean attainment score per town and county
attainment <- schools %>%
  left_join(Towns, by = "shortPostcode") %>%  
  group_by(Town, County) %>%
  summarise(meanAttainment = mean(Attainment8Score))


# Calculate average download speed per town and county
download_speeds <- speeds %>%
  left_join(Towns_Populations, by = "shortPostcode") %>% 
  group_by(Town, County) %>%
  summarise(meanDownloadSpeed = mean(Avgdownload)) %>%
  na.omit()

# Merge the data frames and remove rows with missing values
lm_res5 <- attainment %>%
  left_join(download_speeds, by = "Town") %>% 
  na.omit()

model <- lm(meanDownloadSpeed ~ meanAttainment, data = lm_res5)

summary(model)

# Create a scatter plot
color <- c("OXFORDSHIRE" = "yellow", "YORKSHIRE" = "Green")

ggplot(lm_res5, aes(x = meanAttainment, y = meanDownloadSpeed)) +
  geom_point(data = filter(lm_res5, County.x == "YORKSHIRE"), aes(color = "YORKSHIRE")) +
  geom_point(data = filter(lm_res5, County.x == "OXFORDSHIRE"), aes(color = "OXFORDSHIRE")) +
  geom_smooth(method = lm, se = FALSE, color = "peachpuff4") +
  labs(x = "Average Attainment Score", y = "Average Download Speed", title = "Average Attainment Score vs Average Download Speed", color = "County")

