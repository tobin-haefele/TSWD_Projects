#Load tidyverse
library(tidyverse)
library(lubridate)
#Load dataset in
CarData = read.csv(file = "dv-carbitrage-raw-data.csv", header = TRUE)
#Filter day posted column out
DateFiltered = CarData %>%
    select(time_posted) %>%
    separate(time_posted, into =c("DatePost", "TimePosted"),sep = " ") %>%
    as.date(select(DatePost))
Date_grouped = DateFiltered %>%
    group_by(DatePost) %>%
    summarise(count = n())

Date_grouped$DatePost <- as.Date(Date_grouped$DatePost)

ggplot(Date_grouped, aes(x = DatePost, y = count)) +
  geom_line() +
  xlab("Date") +
  ylab("Count") +
  ggtitle("Daily Rate of Posts")