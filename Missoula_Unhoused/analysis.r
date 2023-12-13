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


# Load csv  file
solutions_fund <- read.csv("20231122-solutions-fund.csv")

#check data
head(solutions_fund)

#check data types
str(solutions_fund)

#convert to numeric
solutions_fund$Total.paid <- as.numeric(gsub("[\\$,]", "", solutions_fund$Total.paid))

library(stringr)

# Define categories and their respective regex patterns
categories <- c("Rent", "Utilities", "Applications", "Deposits", "Transportation")

# Define regex patterns for each category
patterns <- c(
  "(?i)\\brent\\b",  # Matches "rent" as a whole word
  "(?i)utility|electricity|water",  # Includes common utilities
  "(?i)applications|apply|fee|fees",  # Matches application-related terms
  "(?i)deposit",  # Matches terms related to deposits
  "(?i)transportation|bus|taxi|gas"  # Matches transportation-related terms
)

# Function to categorize based on patterns
categorize_category <- function(what_for) {
  for (i in seq_along(patterns)) {
    if (str_detect(what_for, patterns[i])) {
      return(categories[i])
    }
  }
  return("Other")
}

# Create a new column 'Category' based on similar words and patterns
solutions_fund <- solutions_fund %>%
  mutate(Category = sapply(What.for., categorize_category))

# Display the result
head(solutions_fund)

#calculate the total paid by What for
total_paid_by_what_for <- solutions_fund %>%
  group_by(Category) %>%
  summarize(Total.paid = sum(Total.paid)) %>%
  mutate(Percent.of.Total = Total.paid / sum(Total.paid)) %>%
  mutate(Percent.of.Total = scales::percent(Percent.of.Total)) %>%
  arrange(desc(Total.paid))
  
print(total_paid_by_what_for)

total_paid_by_what_for$Category <- factor(total_paid_by_what_for$Category, levels = total_paid_by_what_for$Category)


# Create the bar chart with custom legend labels
ggplot(total_paid_by_what_for, aes(x = Category, y = Total.paid, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Majority of funding is spent on prevention of homelessness",
       x = "What for",
       y = "Total Paid") +
  scale_fill_discrete(name = "What for",
                      labels = c("Prevention" = "Prevention",
                                 "Literally Homeless" = "Literally Homeless"))

#table of total paid by What for
total_paid_by_what_for <- solutions_fund %>%
  group_by(Category) %>%
  summarize(Total.paid = sum(Total.paid)) %>%
  mutate(Percent.of.Total = Total.paid / sum(Total.paid)) %>%
  mutate(Percent.of.Total = scales::percent(Percent.of.Total))

#display table
print(total_paid_by_what_for)


###Funding by source

# Group by Funding Source and Category, calculate total paid and percentage
total_paid_by_what_for_by_source <- solutions_fund %>%
  group_by(Funding.Source, Category) %>%
  summarize(Total.paid = sum(Total.paid)) %>%
  mutate(Percent.of.Total = Total.paid / sum(Total.paid))

# Arrange by Total Paid within each Category within each Funding Source
total_paid_by_what_for_by_source <- total_paid_by_what_for_by_source %>%
  arrange(Funding.Source, desc(Total.paid))

# Convert Funding.Source to a factor, sorting levels by Total Paid
total_paid_by_what_for_by_source$Funding.Source <- factor(
  total_paid_by_what_for_by_source$Funding.Source, 
  levels = unique(total_paid_by_what_for_by_source$Funding.Source[order(-total_paid_by_what_for_by_source$Total.paid)])
)

# Plot the stacked bar chart
ggplot(total_paid_by_what_for_by_source, aes(x = Funding.Source, y = Percent.of.Total, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of funding spent on different categories by source",
       x = "Funding Source",
       y = "Percentage of Total") +
  scale_fill_discrete(name = "What for",
                      labels = c("Prevention" = "Prevention",
                                 "Literally Homeless" = "Literally Homeless")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent) 


### Funding by source and referral from

unique(solutions_fund$Referral.from)
#remove any spaces in the referral from column
solutions_fund$Referral.from <- gsub(" ", "", solutions_fund$Referral.from)
unique(solutions_fund$Referral.from)

#convert all referral from to upper case
solutions_fund$Referral.from <- toupper(solutions_fund$Referral.from)
unique(solutions_fund$Referral.from)
# Group by Funding Source and Referral from, calculate total paid and percentage
total_paid_by_what_for_by_source_by_referral_from <- solutions_fund %>%
  group_by(Funding.Source, Referral.from) %>%
  summarize(Total.paid = sum(Total.paid)) %>%
  mutate(Percent.of.Total = Total.paid / sum(Total.paid))

# Arrange by Total Paid within each Referral from within each Funding Source
total_paid_by_what_for_by_source_by_referral_from <- total_paid_by_what_for_by_source_by_referral_from %>%
  arrange(Funding.Source, Referral.from, desc(Total.paid))

# Convert Funding.Source to a factor, sorting levels by Total Paid
total_paid_by_what_for_by_source_by_referral_from$Funding.Source <- factor(
  total_paid_by_what_for_by_source_by_referral_from$Funding.Source, 
  levels = unique(total_paid_by_what_for_by_source_by_referral_from$Funding.Source[order(-total_paid_by_what_for_by_source_by_referral_from$Total.paid)])
)

# Plot the stacked bar chart
ggplot(total_paid_by_what_for_by_source_by_referral_from, aes(x = Funding.Source, y = Percent.of.Total, fill = Referral.from)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of funding spent on different categories by source and referral from",
       x = "Funding Source",
       y = "Percentage of Total") +
  scale_fill_discrete(name = "Referral from",
                      labels = c("Prevention" = "Prevention",
                                 "Literally Homeless" = "Literally Homeless")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent)