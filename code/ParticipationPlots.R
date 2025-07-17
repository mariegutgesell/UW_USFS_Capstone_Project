#Feel free to edit/improve!:) If you can think of a better/different way to visualize participation metrics please share!
#So far I've only done using but will re-upload with more metrics
#You can see the examples of the plots in my figures on the Drive

library(tidyverse)
library(ggplot2)
library(tidyverse)
library(readr)
library(tidyr)
library(dplyr)
library(scales)

# Load data
comprehensive_data <- read.csv("UW_tongass_harvest_data_clean_comprehensive (1).csv")

#Filter for Community
Angoon_filter <- comprehensive_data %>%
  filter(Site == "Angoon")

###########################################
####PARTICIPATION METRIC: Percent_Using####

#Plot Filter: Using
#Grouped by Taxa_lvl1 and Year
#Using average Percent_Using across all records in each group (per Taxa/per Yr)
Angoon_filter %>%
  group_by(Taxa_lvl1, Year) %>%
  summarize(Avg_Percent_Using = mean(Percent_Using, na.rm = TRUE)) -> Angoon_using_plot  

#Avg % Using by Taxa Over Time
ggplot(Angoon_using_plot, aes(x = Year, y = Avg_Percent_Using, color = Taxa_lvl1)) +
  geom_line() +
  geom_point() +
  labs(title = "Angoon: Average Percent Using by Taxa Over Time",
       y = "Avg Percent Using", x = "Year") +
  #Space Years by 5
  scale_x_continuous(breaks = seq(min(Angoon_filter$Year), max(Angoon_filter$Year), by = 5)) +
 #Space y-axis to range from 0 to 0.3 in increments of 0.05 (EDIT if participation goes above 30%!)
   scale_y_continuous(breaks = seq(0, 0.5, by = 0.05)) + 
  theme_minimal()

####One Using Plot ^ for 3 Communities Comparison####

#Filter for the three towns/sites
towns_data <- comprehensive_data %>%
  filter(Site %in% c("Angoon", "Hoonah", "Sitka"))

#Summarize average Percent_Using per Taxa, Year, and Site
towns_using_summary <- towns_data %>%
  group_by(Site, Taxa_lvl1, Year) %>%
  summarize(Avg_Percent_Using = mean(Percent_Using, na.rm = TRUE)) %>%
#removes groupings so future steps apply to whole dataset
    ungroup()

#Using Plot Faceted by Site
ggplot(towns_using_summary, aes(x = Year, y = Avg_Percent_Using, color = Taxa_lvl1)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Site) +
  labs(title = "Average Percent Using by Taxa Over Time (Angoon, Angoon, Angoon)",
       y = "Average Percent Using",
       x = "Year",
       color = "Taxa Level 1") +
  scale_x_continuous(breaks = seq(min(towns_using_summary$Year), max(towns_using_summary$Year), by = 10)) +
  scale_y_continuous(breaks = seq(0, 0.5, by = 0.05)) +
  theme_minimal()

###HEAT MAP###
# Create summary data again (just in case)
Angoon_using_plot <- comprehensive_data %>%
  filter(Site == "Angoon") %>%
  group_by(Taxa_lvl1, Year) %>%
  summarize(Avg_Percent_Using = mean(Percent_Using, na.rm = TRUE)) %>%
  filter(!is.na(Avg_Percent_Using))

#Convert Year to a factor so it displays only the actual years with data (spaced more evenly)
Angoon_using_plot$Year <- as.factor(Angoon_using_plot$Year)

#Heatmap Plot: Gradient (less contrast)
ggplot(Angoon_using_plot, aes(x = Year, y = Taxa_lvl1, fill = Avg_Percent_Using)) +
  geom_tile(color = "white") +
  scale_fill_gradient(labels = percent,low = "white", high = "steelblue") +
labs(title = "Angoon: Average Percent Using",
     x = "Year", y = "Taxa", fill = "Avg % Using") +
  theme_minimal()

#Heatmap Plot: Values more contrasted
#create new column that bins numeric values into categories
Angoon_using_plot_binned <- Angoon_using_plot %>%
  mutate(Binned_Using = cut(
    Avg_Percent_Using,
    #Change based on maximum % from binned values!
    breaks = c(0, 0.05, 0.10, 0.15, 0.20, 0.30, .40, .50, Inf),
    labels = c("<5%", "5–10%", "10–15%", "15–20%", "20–30%", "30-40%", "40-50%",
               ">50%"),
    include.lowest = TRUE
  ))

#use Binned_Using variable for fill 
ggplot(Angoon_using_plot_binned, aes(x = as.factor(Year), y = Taxa_lvl1, fill = Binned_Using)) +
  geom_tile(color = "white") +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Angoon: Percent Using",
       x = "Year", y = "Taxa", fill = "Avg % Using") +
  theme_minimal()

###########################################################
####PARTICIPATION METRIC: Percent_Attempting_to_Harvest####

#Filter for Community
Angoon_filter <- comprehensive_data %>%
  filter(Site == "Angoon")

#Plot Filter: Attempting
#Grouped by Taxa_lvl1 and Year
#Using average Percent_Attempting_to_Harvest across all records in each group (per Taxa/per Yr)
Angoon_attempting_plot <- Angoon_filter %>%
  group_by(Taxa_lvl1, Year) %>%
  summarize(Avg_Percent_Attempting = mean(Percent_Attempting_to_Harvest, na.rm = TRUE)) %>%
  filter(!is.na(Avg_Percent_Attempting)) -> Angoon_attempting_plot

#Avg % Attempting by Taxa Over Time
ggplot(Angoon_attempting_plot, aes(x = Year, y = Avg_Percent_Attempting, color = Taxa_lvl1)) +
  geom_line() +
  geom_point() +
  labs(title = "Angoon: Average Percent Attempting to Harvest by Taxa Over Time",
       y = "Avg Percent Attempting to Harvest", x = "Year") +
  #Space Years by 5
  scale_x_continuous(breaks = seq(min(Angoon_filter$Year), max(Angoon_filter$Year), by = 5)) +
  theme_minimal()

###One Using Plot ^ for 3 Communities Comparison###
towns_data <- comprehensive_data %>%
  filter(Site %in% c("Angoon", "Hoonah", "Sitka"))

#Summarize average Percent_Using per Taxa, Year, and Site
towns_attempting_summary <- towns_data %>%
  group_by(Site, Taxa_lvl1, Year) %>%
  summarize(Avg_Percent_Attempting = mean(Percent_Attempting_to_Harvest, na.rm = TRUE)) %>%
  drop_na(Avg_Percent_Attempting) %>%  
  ungroup()
  
#Using Plot Faceted by Site
ggplot(towns_attempting_summary, aes(x = Year, y = Avg_Percent_Attempting, color = Taxa_lvl1)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Site) +
  labs(title = "Average Percent Attempting to Harvest by Taxa Over Time (Angoon, Angoon, Angoon)",
       y = "Average Percent Attempting to Harvest",
       x = "Year",
       color = "Taxa Level 1") +
  scale_x_continuous(breaks = seq(min(towns_using_summary$Year), max(towns_using_summary$Year), by = 10)) +
  scale_y_continuous(breaks = seq(0, 0.5, by = 0.05)) +
  theme_minimal()

###HEAT MAP###
Angoon_attempting_plot <- comprehensive_data %>%
  filter(Site == "Angoon") %>%
  group_by(Taxa_lvl1, Year) %>%
  summarize(Avg_Percent_Attempting = mean(Percent_Attempting_to_Harvest, na.rm = TRUE)) %>%
  filter(!is.na(Avg_Percent_Attempting))

#Convert Year to a factor so it displays only the actual years with data (spaced more evenly)
Angoon_attempting_plot$Year <- as.factor(Angoon_attempting_plot$Year)

#Heatmap Plot: Gradient (less contrast)
ggplot(Angoon_attempting_plot, aes(x = Year, y = Taxa_lvl1, fill = Avg_Percent_Attempting)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Angoon: Average Percent Attempting to Harvest",
       x = "Year", y = "Taxa", fill = "Avg % Attempting") +
  theme_minimal() 

#Heatmap Plot: Values more contrasted
  Angoon_attempting_plot_binned <- Angoon_attempting_plot %>%
    mutate(Binned_Attempting = cut(
      Avg_Percent_Attempting,
      breaks = c(0, 0.05, 0.10, 0.15, 0.20, Inf),
      labels = c("<5%", "5–10%", "10–15%", "15–20%", ">20%"),
      include.lowest = TRUE
    ))
  
ggplot(Angoon_attempting_plot_binned, aes(x = as.factor(Year), y = Taxa_lvl1, fill = Binned_Attempting)) +
  geom_tile(color = "white") +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Angoon: Percent Attempting to Harvest",
       x = "Year", y = "Taxa", fill = "Avg % Attempting") +
  theme_minimal() 

################################################
####PARTICIPATION METRIC: Percent_Harvesting####

#Filter for Community
Angoon_filter <- comprehensive_data %>%
  filter(Site == "Angoon")

#Plot Filter: % Harvesting
Angoon_harvesting_plot <- Angoon_filter %>%
  group_by(Taxa_lvl1, Year) %>%
  summarize(Avg_Percent_Harvesting = mean(Percent_Harvesting, na.rm = TRUE)) %>%
  filter(!is.na(Avg_Percent_Harvesting)) -> Angoon_harvesting_plot

#Avg % Harvesting by Taxa Over Time
ggplot(Angoon_harvesting_plot, aes(x = Year, y = Avg_Percent_Harvesting, color = Taxa_lvl1)) +
  geom_line() +
  geom_point() +
  labs(title = "Angoon: Average Percent Harvesting by Taxa Over Time",
       y = "Avg Percent Harvesting", x = "Year") +
  #Space Years by 5
  scale_x_continuous(breaks = seq(min(Angoon_filter$Year), max(Angoon_filter$Year), by = 5)) +
  theme_minimal()

###One Using Plot ^ for 3 Communities Comparison###
towns_data <- comprehensive_data %>%
  filter(Site %in% c("Angoon", "Hoonah", "Sitka"))

#Summarize average Percent_Using per Taxa, Year, and Site
towns_harvesting_summary <- towns_data %>%
  group_by(Site, Taxa_lvl1, Year) %>%
  summarize(Avg_Percent_Harvesting = mean(Percent_Harvesting, na.rm = TRUE)) %>%
  drop_na(Avg_Percent_Harvesting) %>%
  ungroup()

#Using Plot Faceted by Site
ggplot(towns_harvesting_summary, aes(x = Year, y = Avg_Percent_Harvesting, color = Taxa_lvl1)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Site) +
  labs(title = "Average Percent Harvesting Taxa Over Time (Angoon, Angoon, Angoon)",
       y = "Average Percent Harvesting",
       x = "Year",
       color = "Taxa Level 1") +
  scale_x_continuous(breaks = seq(min(towns_harvesting_summary$Year), max(towns_harvesting_summary$Year), by = 10)) +
  scale_y_continuous(breaks = seq(0, 0.5, by = 0.05)) +
  theme_minimal()

###HEAT MAP###
Angoon_harvesting_plot <- comprehensive_data %>%
  filter(Site == "Angoon") %>%
  group_by(Taxa_lvl1, Year) %>%
  summarize(Avg_Percent_Harvesting = mean(Percent_Harvesting, na.rm = TRUE)) %>%
  filter(!is.na(Avg_Percent_Harvesting))

Angoon_harvesting_plot$Year <- as.factor(Angoon_harvesting_plot$Year)

#Heatmap Plot: Gradient (less contrast)
ggplot(Angoon_harvesting_plot, aes(x = Year, y = Taxa_lvl1, fill = Avg_Percent_Harvesting)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Angoon: Average Percent Harvesting",
       x = "Year", y = "Taxa", fill = "Avg % Harvesting") +
  theme_minimal() 

#Heatmap Plot: Values more contrasted
Angoon_harvesting_plot_binned <- Angoon_harvesting_plot %>%
  mutate(Binned_Harvesting = cut(
    Avg_Percent_Harvesting,
    breaks = c(0, 0.05, 0.10, 0.15, 0.20, 0.30, .40, Inf),
    labels = c("<5%", "5–10%", "10–15%", "15–20%", "20-30%", "30-40%", ">40%"),
    include.lowest = TRUE
  ))

ggplot(Angoon_harvesting_plot_binned, aes(x = as.factor(Year), y = Taxa_lvl1, fill = Binned_Harvesting)) +
  geom_tile(color = "white") +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Angoon: Percent Harvesting",
       x = "Year", y = "Taxa", fill = "Avg % Harvesting") +
  theme_minimal() 


################################################
####PARTICIPATION METRIC: Percent_Receiving####

#Filter for Community
Angoon_filter <- comprehensive_data %>%
  filter(Site == "Angoon")

#Plot Filter: % Receiving
Angoon_receiving_plot <- Angoon_filter %>%
  group_by(Taxa_lvl1, Year) %>%
  summarize(Avg_Percent_Receiving = mean(Percent_Receiving, na.rm = TRUE)) %>%
  filter(!is.na(Avg_Percent_Receiving)) -> Angoon_receiving_plot

#Avg % Receiving by Taxa Over Time
ggplot(Angoon_receiving_plot, aes(x = Year, y = Avg_Percent_Receiving, color = Taxa_lvl1)) +
  geom_line() +
  geom_point() +
  labs(title = "Angoon: Average Percent Receiving by Taxa Over Time",
       y = "Avg Percent Receiving", x = "Year") +
  #Space Years by 5
  scale_x_continuous(breaks = seq(min(Angoon_filter$Year), max(Angoon_filter$Year), by = 5)) +
  theme_minimal()

###One Using Plot ^ for 3 Communities Comparison###
towns_data <- comprehensive_data %>%
  filter(Site %in% c("Angoon", "Hoonah", "Sitka"))

#Summarize average Percent_Using per Taxa, Year, and Site
towns_receiving_summary <- towns_data %>%
  group_by(Site, Taxa_lvl1, Year) %>%
  summarize(Avg_Percent_Receiving = mean(Percent_Receiving, na.rm = TRUE)) %>%
  drop_na(Avg_Percent_Receiving) %>%
  ungroup()

#Using Plot Faceted by Site
ggplot(towns_receiving_summary, aes(x = Year, y = Avg_Percent_Receiving, color = Taxa_lvl1)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Site) +
  labs(title = "Average Percent Receiving Taxa Over Time (Angoon, Angoon, Angoon)",
       y = "Average Percent Receiving",
       x = "Year",
       color = "Taxa Level 1") +
  scale_x_continuous(breaks = seq(min(towns_receiving_summary$Year), max(towns_receiving_summary$Year), by = 10)) +
  scale_y_continuous(breaks = seq(0, 0.7, by = 0.1)) +
  theme_minimal()

###HEAT MAP###
Angoon_receiving_plot <- comprehensive_data %>%
  filter(Site == "Angoon") %>%
  group_by(Taxa_lvl1, Year) %>%
  summarize(Avg_Percent_Receiving = mean(Percent_Receiving, na.rm = TRUE)) %>%
  filter(!is.na(Avg_Percent_Receiving))

Angoon_receiving_plot$Year <- as.factor(Angoon_receiving_plot$Year)

#Heatmap Plot: Gradient (less contrast)
ggplot(Angoon_receiving_plot, aes(x = Year, y = Taxa_lvl1, fill = Avg_Percent_Receiving)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Angoon: Average Percent Receiving",
       x = "Year", y = "Taxa", fill = "Avg % Receiving") +
  theme_minimal() 

#Heatmap Plot: Values more contrasted
Angoon_receiving_plot_binned <- Angoon_receiving_plot %>%
  mutate(Binned_Receiving = cut(
    Avg_Percent_Receiving,
    breaks = c(0, 0.05, 0.10, 0.15, 0.20, 0.30, Inf),
    labels = c("<5%", "5–10%", "10–15%", "15–20%", "20-30%", ">30%"),
    include.lowest = TRUE
  ))

ggplot(Angoon_receiving_plot_binned, aes(x = as.factor(Year), y = Taxa_lvl1, fill = Binned_Receiving)) +
  geom_tile(color = "white") +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Angoon: Percent Receiving",
       x = "Year", y = "Taxa", fill = "Avg % Receiving") +
  theme_minimal() 


################################################
####PARTICIPATION METRIC: Percent_Giving####

#Filter for Community
Angoon_filter <- comprehensive_data %>%
  filter(Site == "Angoon")

#Plot Filter: % Receiving
Angoon_giving_plot <- Angoon_filter %>%
  group_by(Taxa_lvl1, Year) %>%
  summarize(Avg_Percent_Giving = mean(Percent_Giving, na.rm = TRUE)) %>%
  filter(!is.na(Avg_Percent_Giving)) -> Angoon_giving_plot

#Avg % Receiving by Taxa Over Time
ggplot(Angoon_giving_plot, aes(x = Year, y = Avg_Percent_Giving, color = Taxa_lvl1)) +
  geom_line() +
  geom_point() +
  labs(title = "Angoon: Average Percent Giving by Taxa Over Time",
       y = "Avg Percent Giving", x = "Year") +
  #Space Years by 5
  scale_x_continuous(breaks = seq(min(Angoon_filter$Year), max(Angoon_filter$Year), by = 5)) +
  theme_minimal()

###One Using Plot ^ for 3 Communities Comparison###
towns_data <- comprehensive_data %>%
  filter(Site %in% c("Angoon", "Hoonah", "Sitka"))

#Summarize average Percent_Using per Taxa, Year, and Site
towns_giving_summary <- towns_data %>%
  group_by(Site, Taxa_lvl1, Year) %>%
  summarize(Avg_Percent_Giving = mean(Percent_Giving, na.rm = TRUE)) %>%
  drop_na(Avg_Percent_Giving) %>%
  ungroup()

#Using Plot Faceted by Site
ggplot(towns_giving_summary, aes(x = Year, y = Avg_Percent_Giving, color = Taxa_lvl1)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Site) +
  labs(title = "Average Percent Giving Taxa Over Time (Angoon, Angoon, Angoon)",
       y = "Average Percent Giving",
       x = "Year",
       color = "Taxa Level 1") +
  scale_x_continuous(breaks = seq(min(towns_giving_summary$Year), max(towns_giving_summary$Year), by = 10)) +
  scale_y_continuous(breaks = seq(0, 0.7, by = 0.1)) +
  theme_minimal()

###HEAT MAP###
Angoon_giving_plot <- comprehensive_data %>%
  filter(Site == "Angoon") %>%
  group_by(Taxa_lvl1, Year) %>%
  summarize(Avg_Percent_Giving = mean(Percent_Giving, na.rm = TRUE)) %>%
  filter(!is.na(Avg_Percent_Giving))

Angoon_giving_plot$Year <- as.factor(Angoon_giving_plot$Year)

#Heatmap Plot: Gradient (less contrast)
ggplot(Angoon_giving_plot, aes(x = Year, y = Taxa_lvl1, fill = Avg_Percent_Giving)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Angoon: Average Percent Giving",
       x = "Year", y = "Taxa", fill = "Avg % Giving") +
  theme_minimal() 

#Heatmap Plot: Values more contrasted
Angoon_giving_plot_binned <- Angoon_giving_plot %>%
  mutate(Binned_Giving = cut(
    Avg_Percent_Giving,
    breaks = c(0, 0.05, 0.10, 0.15, 0.20, 0.30, Inf),
    labels = c("<5%", "5–10%", "10–15%", "15–20%", "20-30%", ">30%"),
    include.lowest = TRUE
  ))

ggplot(Angoon_giving_plot_binned, aes(x = as.factor(Year), y = Taxa_lvl1, fill = Binned_Giving)) +
  geom_tile(color = "white") +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Angoon: Percent Giving",
       x = "Year", y = "Taxa", fill = "Avg % Giving") +
  theme_minimal() 

