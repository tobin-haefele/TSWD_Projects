######(Easy) Build a visualization that illustrates the rate at which new cars are posted by day or by week. Optionally, add information on location.

#Load tidyverse
library(tidyverse)
library(lubridate)
#Load dataset in
CarData = read.csv(file = "dv-carbitrage-raw-data.csv", header = TRUE)
#Filter day posted column out
DateFiltered = CarData %>%
    select(time_posted) %>%
    separate(time_posted, into =c("DatePost", "TimePosted"),sep = " ") %>%
    select(DatePost)

#group dates by day posted
Date_grouped = DateFiltered %>%
    group_by(DatePost) %>%
    summarise(count = n())

Date_grouped$DatePost <- as.Date(Date_grouped$DatePost)

ggplot(Date_grouped, aes(x = DatePost, y = count)) +
  geom_line() +
  xlab("Date") +
  ylab("Number of New Listings") +
  ggtitle("Daily Posts from July to Sept")

ggsave(
    "assets\\Plot2_Car.jpeg",
    plot = last_plot())