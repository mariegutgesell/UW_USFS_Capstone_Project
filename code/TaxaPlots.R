###############################
##### Taxa Level 2/5 Graphs #####
###############################

library(tidyverse)
library(ggplot2)
library(tidyverse)
library(readr)
library(tidyr)
library(dplyr)
library(scales)

comprehensive_data <- read.csv("UW_tongass_harvest_data_clean_comprehensive (1).csv")

Hoonah_recent <- comprehensive_data %>%
  filter(Site == "Hoonah") %>%
  filter(Year == max(Year, na.rm = TRUE))

### Taxa Level 2 ###

#See most recent year
view(Hoonah_recent)

Hoonah_taxa2_plot <- Hoonah_recent %>%
  filter(!is.na(Taxa_lvl2), !is.na(Estimated_Total_Pounds_Harvested)) %>%
  group_by(Taxa_lvl2) %>%
  summarize(Total_Pounds = sum(Estimated_Total_Pounds_Harvested, na.rm = TRUE))

ggplot(Hoonah_taxa2_plot, aes(x = reorder(Taxa_lvl2, Total_Pounds), y = Total_Pounds)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  #Change year based on most recent!
  labs(title = "Hoonah: Harvest by Taxa Level 2 in 2016",
       x = "Taxa", y = "Total Pounds Harvested") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  theme_minimal()


###Most Recent Year Top 10 Taxa Level 5###

Hoonah_taxa5_top10 <- Hoonah_recent %>%
  filter(!is.na(Taxa_lvl5), !is.na(Estimated_Total_Pounds_Harvested)) %>%
  group_by(Taxa_lvl5) %>%
  summarize(Total_Pounds = sum(Estimated_Total_Pounds_Harvested, na.rm = TRUE)) %>%
  arrange(desc(Total_Pounds)) %>%
  slice_head(n = 10)

ggplot(Hoonah_taxa5_top10, aes(x = reorder(Taxa_lvl5, Total_Pounds), y = Total_Pounds)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Hoonah: Top 10 Resources (Taxa Level 5) in 2016",
       x = "Resource", y = "Total Pounds Harvested") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

#Grouped Plots

#Taxa 2 Percentages 
selected_sites <- c("Sitka", "Angoon", "Hoonah")

recent_years <- comprehensive_data %>%
  filter(Site %in% selected_sites) %>%
  group_by(Site) %>%
  summarize(Recent_Year = max(Year, na.rm = TRUE))

recent_data <- comprehensive_data %>%
  inner_join(recent_years, by = c("Site" = "Site", "Year" = "Recent_Year"))

taxa2_percentages <- recent_data %>%
  filter(!is.na(Taxa_lvl2), !is.na(Estimated_Total_Pounds_Harvested)) %>%
  group_by(Site, Taxa_lvl2) %>%
  summarize(Total_Pounds = sum(Estimated_Total_Pounds_Harvested, na.rm = TRUE)) %>%
  group_by(Site) %>%
  mutate(Percent_of_Harvest = Total_Pounds / sum(Total_Pounds))

ggplot(taxa2_percentages, aes(x = reorder(Taxa_lvl2, Percent_of_Harvest), y = Percent_of_Harvest, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~Site) +
  labs(title = "Percent of Total Harvest by Taxa Level 2",
       x = "Taxa", y = "Percent of Town's Total Harvest") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_minimal()

###Most Recent Year Top 10 Taxa Level 5 Percentages###
taxa5_top10_pct <- recent_data %>%
  filter(!is.na(Taxa_lvl5), !is.na(Estimated_Total_Pounds_Harvested)) %>%
  group_by(Site, Taxa_lvl5) %>%
  summarize(Total_Pounds = sum(Estimated_Total_Pounds_Harvested, na.rm = TRUE)) %>%
  group_by(Site) %>%
  mutate(Percent_of_Town = Total_Pounds / sum(Total_Pounds)) %>%
  slice_max(Total_Pounds, n = 10)

ggplot(taxa5_top10_pct, aes(x = reorder(Taxa_lvl5, Percent_of_Town), y = Percent_of_Town, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~Site, scales = "free_y") +
  labs(title = "Top 10 Resources (Taxa Level 5) by Percent of Total Harvest per Town",
       x = "Resource", y = "Percent of Town's Total Harvest") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  coord_flip() +
  theme_minimal()
