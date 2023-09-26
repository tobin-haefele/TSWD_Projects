#add libraries
library(tidyverse)
library(lubridate)
library(scales)
#import dataset
HouseData = read.csv(file = "unhousedData.csv", header = TRUE)

#filter out rows where exit date is blank
HouseData <- HouseData %>%
  filter(!is.na(Exit.Date) & Exit.Date != "")

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
  summarise(Total.Length.of.Stay = sum(Length.of.Stay, na.rm = TRUE))
head(HouseData)
#filter out outliers
HouseData <- HouseData %>%
  filter(Total.Length.of.Stay < 1000)
#plot histogram of total length of stay, 
ggplot(data = HouseData, aes(x = Total.Length.of.Stay, fill = ..count..)) +
  geom_histogram(binwidth = 10) +
  xlab("Total Length of Stay (Days)") +
  ylab("Count") +
  ggtitle("Total Length of Stay in Shelter") +
  scale_fill_gradient(low = "blue", high = "red") +  # Adjust colors here
  theme_minimal()
ggsave("assets\\TotalLengthofStay.jpeg", width = 10, height = 10, units = "cm")
