###############################################
##    Subsistence Harvest Data Summaries     ##
##        Anne Beaudreau | June 2025         ##
###############################################

##### LOAD LIBRARIES #####
library(tidyr)
library(dplyr)
library(ggplot2)

##### SET WORKING DIRECTORY #####
## CHANGE THIS TO THE CORRECT FILE PATH ON YOUR COMPUTER
setwd("C:/Users/annebeau/Documents/Projects/Subsistence Tongass_USFS Capstone/")

##### READ IN DATA #####
# Read files and compile data frame
data <- read.csv("UW_tongass_harvest_data_clean_comprehensive.csv")

#######################
##### HARVEST COMPOSITION #####
## Data visualization - bar plots

# Filter to specific community
data %>% 
  filter(Site == "Kake") -> data_filter

# By Year - composition
data_filter %>%
  group_by(Taxa_lvl3,Year) %>%
  summarize(Estimated_Total_Pounds_Harvested=sum(Estimated_Total_Pounds_Harvested)) -> data_plot

ggplot(data_plot, aes(x = factor(Year), Estimated_Total_Pounds_Harvested, 
                      fill = Taxa_lvl3)) +
  geom_bar(position = "fill", stat = "identity", color = "black") +
  ylab("Proportion of est. total pounds harvested") + xlab("Year") +
  theme_bw(base_size = 14)

ggplot(data_plot, aes(x = factor(Year), Estimated_Total_Pounds_Harvested, 
                      fill = Taxa_lvl3)) +
  geom_bar(stat = "identity", color = "black") +
  ylab("Proportion of est. total pounds harvested") + xlab("Year") +
  theme_bw(base_size = 14)
