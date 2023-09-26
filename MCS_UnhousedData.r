#add libraries
library(tidyverse)
library(lubridate)
library(scales)
#import dataset
HouseData = read.csv(file = "unhousedData.csv", header = TRUE)

#select columns of interest
HouseData <- HouseData %>%
  select(Client.ID, Date.Identified, Exit.Date,Primary.Race,Gender)

#filter out rows where exit date is blank
HouseData <- HouseData %>%
  filter(!is.na(Exit.Date) & Exit.Date != "")

#filter out rows where date identified is blank
HouseData <- HouseData %>%
  filter(!is.na(Date.Identified) & Date.Identified != "")

#convert date columns to date format
HouseData$Date.Identified <- as.Date(HouseData$Date.Identified, format = "%m/%d/%Y")
HouseData$Exit.Date <- as.Date(HouseData$Exit.Date, format = "%m/%d/%Y")

#group by client id and calcualate the length of time between entry and exit for each client
HouseData <- HouseData %>%
  group_by(Client.ID) %>%
  mutate(Length.of.Stay = difftime(Exit.Date, Date.Identified, units = "days"))
head(HouseData)
#sum total time spent in shelter for each client id
HouseData <- HouseData %>%
  group_by(Client.ID) %>%
  summarise(median.Length.of.Stay = median(Length.of.Stay, na.rm = TRUE))
#plot histogram of total length of stay, 
ggplot(data = HouseData, aes(x = median.Length.of.Stay, fill = ..count..)) +
  geom_histogram(binwidth = 50) +
  xlab("Median Length of Stay (Days)") +
  ylab("Count") +
  ggtitle("Median Length of Stay in Shelter") +
  scale_fill_gradient(low = "blue", high = "red") +  # Adjust colors here
  theme_minimal()
ggsave("assets\\TotalLengthofStay.jpeg", width = 10, height = 10, units = "cm")
