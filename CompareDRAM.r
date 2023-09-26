 ###(Medium) Build a visualization that compares the performance of the two locations (Front and Central) across the entire history of the Dram Shop.

 #Load in libraries and dataset 
 #Load tidyverse
library(tidyverse)
library(lubridate)
library(scales)
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
  summarise(net_sales_for_year = sum(net_sales))
 
Front_aggregated <- Front_St %>%
  group_by(year) %>%
  summarise(net_sales_for_year = sum(net_sales))

# Combine the data for plotting
combined_data <- bind_rows(
  mutate(Central_aggregated, Location = "Central"),
  mutate(Front_aggregated, Location = "Front St.")
)

# Plot side-by-side bar graphs with Y-axis labels in accounting format
ggplot(combined_data, aes(x = factor(year), y = net_sales_for_year, fill = Location)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylab("Net Sales") +
  xlab("Year") +
  ggtitle("Comparison of Net Sales: Front St. vs Central Locations") +
  scale_fill_manual(name = "Location", values = c("Central" = "red", "Front St." = "blue")) +
  scale_y_continuous(labels = scales::dollar_format(scale = 0.001, prefix = "$", suffix = "k")) +  # Y-axis labels in accounting format
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate X-axis labels for better readability

ggsave(
    "assets\\Plot4_DRAM.jpeg",
    plot = last_plot()
    )