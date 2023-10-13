#####(Easy) Build a visualization that illustrates the patterns in purchases by day of week for both the Front Street and Central locations.


#Load tidyverse
library(tidyverse)
library(lubridate)
#Load dataset in
BeerData = read.csv(file = "DRAMSHOPDATA.csv", header = TRUE)

# Filter data for 'The Dram Shop Front St.'
Front_St <- BeerData %>%
  filter(location == "The Dram Shop Front St." | location == "The Dram Shop") %>%
  select(date, location, gross_sales,hour) %>%
  mutate(day_of_week = wday(date, label = TRUE, abbr = FALSE))  # Add day of the week

# Filter data for 'The Dram Shop'
Central_Data <- BeerData %>%
  filter(location == "The Dram Shop Central") %>%
  select(date, location, gross_sales,hour) %>%
  mutate(day_of_week = wday(date, label = TRUE, abbr = FALSE))  # Add day of the week

# Aggregate sales by day and sum the sales for each day
Front_aggregated <- Front_St %>%
  group_by(date, day_of_week) %>%
  summarise(total_sales_per_day = sum(gross_sales))

Central_aggregated <- Central_Data %>%
  group_by(date, day_of_week) %>%
  summarise(total_sales_per_day = sum(gross_sales))

# Calculate average gross sales per day
Front_avg_sales <- Front_aggregated %>%
  group_by(day_of_week) %>%
  summarise(avg_sales_per_day = median(total_sales_per_day))
head(Front_avg_sales)
Central_avg_sales <- Central_aggregated %>%
  group_by(day_of_week) %>%
  summarise(avg_sales_per_day = median(total_sales_per_day))
head(Central_avg_sales)

# Combine the datasets
combined_avg_sales <- bind_rows(mutate(Front_avg_sales, location = "The Dram Shop Front St."),
                                mutate(Central_avg_sales, location = "The Dram Shop Central"))

combined_avg_sales

# Plotting using ggplot2
ggplot(combined_avg_sales, aes(x = day_of_week, y = avg_sales_per_day, fill = location)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  ylab("Average Sales ($)") +
  xlab("Day of Week") +
  ggtitle("Average Sales by Day of the Week") +
  scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "k")) +
  theme_minimal() +
  scale_fill_manual(values = c("The Dram Shop Front St." = "blue", "The Dram Shop Central" = "red"))

ggsave(
    "assets\\Plot3_DRAM.jpeg",
    plot = last_plot())