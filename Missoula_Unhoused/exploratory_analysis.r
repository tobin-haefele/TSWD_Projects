#Missoula unhoused data, exploratory analysis
##4. Housing Solutions Fund Utilization and Funding Sources
###Analyze the allocation and utilization of funds dedicated to the prevention of houselessness to and help and for those currently unhoused.
###Evaluate the sources of funding and their impact on services provided.
###Compare the demographics of people receiving funding to those over the overall MCES data set

#load packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(rmarkdown)


#load data
read.csv("Missoula_Unhoused\\20231122-solutions-fund.csv") -> solutions_fund

#check data
head(solutions_fund)

#check data types
str(solutions_fund)

#summary statistics
summary(solutions_fund)

#convert total paid to decimal
solutions_fund$Total.paid <- as.numeric(gsub("[\\$,]", "", solutions_fund$Total.paid))

tail(solutions_fund)
#sum total paid based on prevention or literally homeless

# Summarize allocation of funds based on prevention or literally homeless
# Calculate Total.paid and Percent.of.Total
summary_data <- solutions_fund %>% 
  group_by(Prevention.or.Literally.Homeless) %>% 
  summarize(Total.paid = sum(Total.paid)) %>%
  mutate(Percent.of.Total = Total.paid / sum(Total.paid)) %>%
  mutate(Percent.of.Total = percent(Percent.of.Total))


# Create the pie chart with custom legend labels
ggplot(summary_data, aes(x = "", y = Total.paid, fill = Prevention.or.Literally.Homeless)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(x = NULL, y = NULL, fill = "Prevention or Currently Homeless", 
       title = "Allocation of Funds based to preventions or those currently homeless") +
  geom_text(aes(label = Percent.of.Total), position = position_stack(vjust = 0.5), size = 10)  +
  theme_void() +
  scale_fill_manual(labels = c("Prevention", "Currently Homeless"), 
                    values = c("blue", "red")) +  # Set your custom legend labels and colors
  theme(legend.position = "right")

## evaluate the sources of funding and their impact on services provided
