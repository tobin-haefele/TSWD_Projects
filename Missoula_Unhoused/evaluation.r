#Evaluate the sources of funding and their impact on services provided.

# Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)

# Load csv  file
df <- read.csv("Missoula_Unhoused\\20231122-solutions-fund.csv")

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
  summarise(AveragePaid = sum(Total.paid, na.rm = TRUE))

# Group by Funding.Source and summarize
average_spend_per_HMIS_by_source <- average_spend_per_HMIS %>% 
  left_join(df, by = "HMIS..") %>% 
  group_by(Funding.Source) %>% 
  summarise(AveragePaidBySource = sum(AveragePaid, na.rm = TRUE)) %>%
  arrange(desc(AveragePaidBySource))

# Create a color scale based on the AveragePaidBySource values
color_scale <- scales::viridis_pal(option = "D")(length(unique(average_spend_per_HMIS_by_source$AveragePaidBySource)))

# Convert Funding.Source to ordered factor by AveragePaidBySource
average_spend_per_HMIS_by_source$Funding.Source <- factor(average_spend_per_HMIS_by_source$Funding.Source,
                                                          levels = average_spend_per_HMIS_by_source$Funding.Source[order(average_spend_per_HMIS_by_source$AveragePaidBySource, decreasing = TRUE)])

# Create the bar plot
ggplot(average_spend_per_HMIS_by_source, aes(x = Funding.Source, y = AveragePaidBySource, fill = AveragePaidBySource)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = color_scale[1], high = color_scale[length(color_scale)]) +
  labs(title = "Average Spend by Funding Source",
       x = "Funding Source",
       y = "Average Spend") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))