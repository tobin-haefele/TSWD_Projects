#Load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
#Load data
cdata = read.csv("Dashboard\\listing-data.csv")

#display column names
names(cdata)
#remove rows with missing price
cdata = cdata %>% filter(!is.na(price))
#select columns for price and time posted
cdata_filtered = cdata %>% select(price, time_posted)

#convert time_posted to date format and group by date
cdata_filtered$time_posted = as.Date(cdata_filtered$time_posted)
head(cdata_filtered)
cdata_filtered = cdata_filtered %>% group_by(time_posted) %>% summarise(price = median(price))

head(cdata_filtered)

#graph of price vs date over past 30 days
ggplot(cdata_filtered, aes(x = time_posted, y = price)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "Date", y = "Price", title = "Price vs Date over past 30 days")

