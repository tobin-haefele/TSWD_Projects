########(Easy) Build a visualization that illustrates what makes and models of cars are most popular.


#Load tidyverse
library(tidyverse)
#Load dataset in
CarData = read.csv(file = "dv-carbitrage-raw-data.csv", header = TRUE)
#filter so you only have make/model columns
CarData_filtered = CarData %>%
    select(make, model)
# Filter out rows where make and model are not blank
CarData_filtered <- CarData_filtered %>%
  filter(!is.na(make) & make != "", !is.na(model) & model != "")

# Group by make and model, then count the occurrences of each combination
make_model_counts <- CarData_filtered %>%
  group_by(make, model) %>%
  summarise(count = n())

# Sort the data by count in descending order within each make
make_model_counts <- make_model_counts %>%
  arrange(make, desc(count))

# Select the most listings per model for each make
top_make_models <- make_model_counts %>%
  arrange(make, desc(count)) %>%
  group_by(make) %>%
  slice(1) %>%
  ungroup()

# Select the top 20 makes based on the count of their highest model
top_20_makes <- top_make_models %>%
  group_by(make) %>%
  summarise(max_count = max(count)) %>%
  arrange(desc(max_count)) %>%
  head(20)

# Filter the data to include only the top 20 makes and their highest models
top_20_make_models <- top_make_models %>%
  filter(make %in% top_20_makes$make)

# Combine make and model for the Y-axis labels
top_20_make_models$combined_label <- paste(top_20_make_models$make, top_20_make_models$model, sep = " - ")


# Create a horizontal bar chart for the top 20 makes with their highest model
ggplot(data = top_20_make_models, aes(x = count, y = reorder(combined_label, count), fill = model)) +
  geom_bar(stat = "identity") +
  ylab("Make - Model") +
  xlab("Count") +
  ggtitle("Top 20 listings by Make and Model") +
  theme(axis.text.y = element_text(size = 8, hjust = 0)) +
  guides(fill = FALSE)
  ggsave(
    "assets\\Plot1_CarData.jpeg",
    plot = last_plot()
  )