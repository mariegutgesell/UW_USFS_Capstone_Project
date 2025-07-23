##Example code to test github functions 

##Date: July 16, 2025

library(tidyverse)
library(readxl)
library(ggplot2)

##Read in comprehensive harvest data
df <- read.csv("data/UW_tongass_harvest_data_clean_comprehensive.csv")


##Some sample code

##Select communities of interest for case studies 
df_sub <- df %>%
  filter(Site %in% c("Haines", "Klukwan", "Yakutat", "Angoon", "Hoonah", "Sitka", "Hydaburg", "Klawock", "Wrangell", "Kake"))


##Calculate total percapita harvest per community

df_percap_total <- df_sub %>%
  group_by(Site, Year) %>%
  filter(!is.na(Percapita_Pounds_Harvested)) %>%
  summarise_at(vars(Percapita_Pounds_Harvested), list(Percap_sum_lb = sum))


##Plot percapita harvest over time
ggplot(df_percap_total, aes(x = Year, y = Percap_sum_lb)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1983, 2015, by = 5)) +
  labs(x = "Year", y = "Total Annual Percapita Pounds Harvested") +
  theme_classic() +
  facet_wrap(~Site)


##make some change to my script 

##make another change that i dont know if i want tomerge 
## Test Anne
#Test Josh

#test Marissa

#testing
  