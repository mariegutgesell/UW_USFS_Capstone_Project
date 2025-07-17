### PLOTS FOR: ####
#Estimated_Total_Pounds_Harvested
#Mean_Pounds_Per_Household
#PerCapita_Pounds_Harvested
#Estimated_Amount_Harvested
###########################

library(tidyverse)
library(ggplot2)
library(tidyverse)
library(readr)
library(tidyr)
library(dplyr)
library(scales)

#Estimated_Total_Pounds_Harvested
Hoonah_data <- comprehensive_data%>% 
  filter(Site == "Hoonah", !is.na(Estimated_Total_Pounds_Harvested))

Hoonah_totallbs_plot <- Hoonah_data %>%
  group_by(Taxa_lvl1,Year) %>%
  summarize(Estimated_Total_Pounds_Harvested=sum(Estimated_Total_Pounds_Harvested, na.rm = TRUE))

ggplot(Hoonah_totallbs_plot, aes(x = Year, y = Estimated_Total_Pounds_Harvested, color=Taxa_lvl1)) +
  geom_line() +
  geom_point() +
  labs(title = "Hoonah: Estimated Total Pounds Harvested Over Time", x = "Year",
    y = "Estimated Total Pounds Harvested", color = "Taxa_lvl1" ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

#Mean_Pounds_Per_Household
Hoonah_data <- comprehensive_data %>%
  filter(Site == "Hoonah", !is.na(Mean_Pounds_Per_Household))

Hoonah_meanhousehold_plot <- Hoonah_data %>%
  group_by(Year, Taxa_lvl1) %>%
  summarize(Avg_Mean_Pounds = mean(Mean_Pounds_Per_Household, na.rm = TRUE))

ggplot(Hoonah_meanhousehold_plot, aes(x = Year, y = Avg_Mean_Pounds, color = Taxa_lvl1)) +
  geom_line() + 
  geom_point() +
  labs(title = "Hoonah: Mean Pounds Per Household Over Time",
       x = "Year", y = "Mean Pounds Per Household", color = "Taxa_lvl1") +
  theme_minimal()

#Percapita_Pounds_Harvested
Hoonah_data <- comprehensive_data %>%
  filter(Site == "Hoonah", !is.na(Percapita_Pounds_Harvested))

Hoonah_percapita <- Hoonah_data %>%
  group_by(Year, Taxa_lvl1) %>%
  summarize(Avg_Percapita = mean(Percapita_Pounds_Harvested, na.rm = TRUE)) 

ggplot(Hoonah_percapita, aes(x = Year, y = Avg_Percapita, color = Taxa_lvl1)) +
  geom_line() + 
  geom_point() +
  labs(title = "Hoonah: Per Capita Pounds Harvested Over Time",
       x = "Year", y = "Per Capita Pounds", color = "Taxa") +
  theme_minimal()

#Estimated_Amount_Harvested
Hoonah_data <- comprehensive_data %>%
  filter(Site == "Hoonah", !is.na(Estimated_Amount_Harvested))

Hoonah_estimatedamt <- Hoonah_data %>%
  group_by(Year, Taxa_lvl1) %>%
  summarize(Total_Estimated = sum(Estimated_Amount_Harvested, na.rm = TRUE)) 

ggplot(Hoonah_estimatedamt, aes(x = Year, y = Total_Estimated, color = Taxa_lvl1)) +
  geom_line() + geom_point() +
  labs(title = "Hoonah: Estimated Number of Units Harvested Over Time",
       x = "Year", y = "Estimated Number of Units Harvested", color = "Taxa") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

#GROUPED PLOTS 

#Estimated_Total_Pounds_Harvested
totallbs_data <- comprehensive_data %>%
  filter(Site %in% c("Sitka", "Angoon", "Hoonah"), !is.na(Estimated_Total_Pounds_Harvested)) %>%
  group_by(Site, Taxa_lvl1, Year) %>%
  summarize(Estimated_Total_Pounds = sum(Estimated_Total_Pounds_Harvested, na.rm = TRUE))

ggplot(totallbs_data, aes(x = Year, y = Estimated_Total_Pounds, color = Taxa_lvl1)) +
  geom_line() + geom_point() +
  facet_wrap(~Site) +
  labs(title = "Estimated Total Pounds Harvested Over Time",
       x = "Year", y = "Estimated Total Pounds", color = "Taxa") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

#Mean_Pounds_Per_Household
meanhousehold_data <- comprehensive_data %>%
  filter(Site %in% c("Sitka", "Angoon", "Hoonah"), !is.na(Mean_Pounds_Per_Household)) %>%
  group_by(Site, Taxa_lvl1, Year) %>%
  summarize(Avg_Mean_Household = mean(Mean_Pounds_Per_Household, na.rm = TRUE))

ggplot(meanhousehold_data, aes(x = Year, y = Avg_Mean_Household, color = Taxa_lvl1)) +
  geom_line() + geom_point() +
  facet_wrap(~Site) +
  labs(title = "Mean Pounds Per Household Over Time",
       x = "Year", y = "Mean Pounds Per Household", color = "Taxa") +
  theme_minimal()

#Per_Capita_Pounds_Harvested
percapita_data <- comprehensive_data %>%
  filter(Site %in% c("Sitka", "Angoon", "Hoonah"), !is.na(Percapita_Pounds_Harvested)) %>%
  group_by(Site, Taxa_lvl1, Year) %>%
  summarize(Avg_Percapita = mean(Percapita_Pounds_Harvested, na.rm = TRUE))

ggplot(percapita_data, aes(x = Year, y = Avg_Percapita, color = Taxa_lvl1)) +
  geom_line() + geom_point() +
  facet_wrap(~Site) +
  labs(title = "Per Capita Pounds Harvested Over Time",
       x = "Year", y = "Per Capita Pounds", color = "Taxa") +
  theme_minimal()

#Estimated_Amount_Harvested
estimatedamt_data <- comprehensive_data %>%
  filter(Site %in% c("Sitka", "Angoon", "Hoonah"), !is.na(Estimated_Amount_Harvested)) %>%
  group_by(Site, Taxa_lvl1, Year) %>%
  summarize(Total_Estimated_Units = sum(Estimated_Amount_Harvested, na.rm = TRUE))

ggplot(estimatedamt_data, aes(x = Year, y = Total_Estimated_Units, color = Taxa_lvl1)) +
  geom_line() + geom_point() +
  facet_wrap(~Site) +
  labs(title = "Estimated Number of Units Harvested Over Time",
       x = "Year", y = "Estimated Number of Units Harvested", color = "Taxa") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()