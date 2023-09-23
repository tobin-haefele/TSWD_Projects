#Load tidyverse
library(tidyverse)
#Load dataset in
CarData = read.csv(file = "dv-carbitrage-raw-data.csv", header = TRUE)
#filter so you only have make/model columns
CarData_filtered = CarData %>%
    select(make, model)
make_model_counts = make_model_counts %>%
  filter(!is.na(make) & !is.na(model))
#count the repeats of each make and model
make_model_counts = CarData_filtered %>%
    group_by(make,model) %>%
    summarise(count = n())
#sort by descending order
make_model_counts = make_model_counts %>%
    arrange(desc(count))
#filter by most popular
most_popular_models = make_model_counts %>%
  group_by(make) %>%
  slice(1)
#create a bar plot
ggplot(data = most_popular_models, aes(x = make, y = model, size = count)) +
  geom_point() +
  scale_size_area(max_size = 10) +
  xlab("Make") +
  ylab("Model") +
  ggtitle("Most Popular Model for Each Car Make")