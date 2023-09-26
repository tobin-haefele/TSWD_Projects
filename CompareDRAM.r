 ###(Medium) Build a visualization that compares the performance of the two locations (Front and Central) across the entire history of the Dram Shop.

 #Load in libraries and dataset 
 #Load tidyverse
library(tidyverse)
library(lubridate)
#Load dataset in
BeerData = read.csv(file = "DRAMSHOPDATA.csv", header = TRUE)

# Filter data for 'The Dram Shop (front)'
Front_St <- BeerData %>%
  filter(location == "The Dram Shop Front St." | location == "The Dram Shop") %>%
  select(date, location,hour, net_sales) %>%
  mutate(year = year(date))

# Filter data for 'The Dram Shop Central' 
Central_Data <- BeerData %>%
  filter(location == "The Dram Shop Central") %>%
  select(date, location,hour, net_sales) %>%
  mutate(year = year(date))

# Aggregate sales by year

Central_aggregated <- Central_Data %>%
  group_by(year) %>%
  summarise(net_sales_for_year = sum(net_sales) / n())

Front_aggregated <- Front_St %>%
  group_by(year) %>%
  summarise(net_sales_for_year = sum(net_sales) / n())



#Graph the data over the years
ggplot() +
    geom_line(data = Central_aggregated, aes(x = year, y = net_sales_for_year, color = "Front St."), size = 1) +
    geom_line(data = Front_aggregated, aes(x = year, y = net_sales_for_year, color = "Central"), size = 1) +
    ylab("Sales") +
    xlab("Year") +
    ggtitle("Lifelong Comparison of Net Sales for Front and Central Locations") +
    scale_color_manual(name = "Location", values = c("Front St." = "blue", "Central" = "red")) +
    theme_minimal()