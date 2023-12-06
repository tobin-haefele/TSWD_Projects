#Evaluate the sources of funding and their impact on services provided.

# Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)

# Load csv  file
df <- read.csv("20231122-solutions-fund.csv")

head(df)

#transform Total Paid into numeric excluding the $ sign
df$Total.paid <- as.numeric(gsub("[^0-9.]", "", df$Total.paid))

tail(df)

#unique funding sources
unique(df$Funding.Source)

str(df$Total.paid)

# Calculate average spend per unique HMIS (patient number)
average_spend_per_HMIS <- df %>% 
  group_by(HMIS..) %>% 
  summarise(AveragePaid = mean(Total.paid, na.rm = TRUE))

# Group by Funding.Source and summarize
average_spend_per_HMIS_by_source <- average_spend_per_HMIS %>% 
  left_join(df, by = "HMIS..") %>% 
  group_by(Funding.Source) %>% 
  summarise(AveragePaidBySource = mean(AveragePaid, na.rm = TRUE))

# Display the result
print(average_spend_per_HMIS_by_source)

# Plot the result
ggplot(average_spend_per_HMIS_by_source, aes(x = Funding.Source, y = AveragePaidBySource)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Average Funding by Source", x = "Source", y = "Average Total Paid Funding") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
