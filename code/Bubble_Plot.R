# Libraries
library(ggplot2)
library(dplyr)
install.packages("MetBrewer") ##this is for the color blue/orange color scale
library(MetBrewer)

####### BUBBLE PLOT #######
##Read in comprehensive harvest data
df <- read.csv("data/UW_tongass_harvest_data_clean_comprehensive.csv")

#Setting the desired community 
df %>% 
  filter(Site == "Haines") -> data_filter 

#Averaging the desired participation metric and per capita metric + naming
data_filter %>%
  group_by(Taxa_lvl2, Year) %>%
  summarize(
    Avg_Percent_Using = mean(Percent_Using, na.rm = TRUE),
    Avg_Per_Capita_Harvest = mean(Percapita_Pounds_Harvested, na.rm = TRUE)
  ) %>%
  ungroup() -> Haines_using_bubl  

##Making the bubble plot, plug & chug for each different variable as you change things above
ggplot(Haines_using_bubl, aes(
  x = Year,
  y = Taxa_lvl2,
  size = Avg_Percent_Using,
  color = Avg_Per_Capita_Harvest
)) +
  geom_point(alpha = 0.8) +
  scale_color_gradientn(colors = met.brewer("Hiroshige")) +  # continuous color scale
  labs(
    title = "Haines Avg % Using by Year & Taxa",
    x = "Year",
    y = "Taxa Level 2",
    size = "Avg % Using",
    color = "Avg Per Capita Harvest (lbs)"
  ) +
  theme_minimal()




###One Using Plot for 3 Communities Comparison###

#Filter for the three towns/sites
towns_data <- df %>%
  filter(Site %in% c("Klukwan", "Yakutat", "Haines"))

#Summarize average Percent_Using per Taxa, Year, and Site
towns_using_summary <- towns_data %>%
  group_by(Site, Taxa_lvl2, Year) %>%
  summarize(
    Avg_Percent_Using = mean(Percent_Using, na.rm = TRUE),
    Avg_Per_Capita_Harvest = mean(Percapita_Pounds_Harvested, na.rm = TRUE)
  ) %>%
  #removes groupings so future steps apply to whole dataset
  ungroup()

#Using Plot Faceted by Site
ggplot(towns_using_summary, aes(x = factor(Year), y = Taxa_lvl2, size = Avg_Percent_Using, color = Avg_Per_Capita_Harvest)) +
  geom_point(alpha = 0.8) +
  scale_color_gradientn(colors = met.brewer("Hiroshige")) +
  facet_wrap(~Site) +
  labs(title = "Average Percent Using by Taxa Over Time (Klukwan, Yakutat, Haines)",
       y = "Taxa Level 2",
       x = "Year",
       size = "Avg % Using",
       color = "Avg Per Capita Harvest (lbs)") +
  theme_minimal()












