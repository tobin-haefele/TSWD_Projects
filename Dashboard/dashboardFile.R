#Load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
#Load data
cdata = read.csv("Dashboard\\listing-data.csv")

# Load data and clean out empty make and price values
cdata <- read.csv("Dashboard/listing-data.csv")
cdata <- cdata %>% filter(make != "" & price != "" & time_posted != "" & model != "", condition != "")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Kelly Blue Book"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
              
                box(
                  title = "Top 10 Most Popular Models",
                  plotOutput("plot2", height = 400)
                ),
                box(
                  title = "Age of Cars Posted",
                  plotOutput("plot3", height = 400)
                )
              ),
              fluidRow(
                width = 6,
                box(
                  title = "Median Price Over Time",
                  plotOutput("plot1", height = 400)
                ),
                box(
                  title = "Top 10 Most Popular Locations",
                  tableOutput("table1"),
                  width = 6
                ),
                box(
                  title = "Key Findings",
                  textOutput("text1")
                )
              ),
              )
                

              )
      ),
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({

    #Selecting data for price graph
    cprice_filtered <- cdata %>% select(price, time_posted,make, year)
    cprice_filtered$time_posted <- as.Date(cprice_filtered$time_posted)
    #filtering out the outliers price over 1 million or equal to 0
    cprice_filtered <- cprice_filtered %>% filter(price < 1000000 & price > 0)
    head(cprice_filtered)
    #group by date and price and get the median price for each day
    cprice_filtered <- cprice_filtered %>% group_by(time_posted) %>% summarise(median_price = median(price))
    head(cprice_filtered)
    #get max date for filtering
    max_date <- max(cprice_filtered$time_posted)
    #filter out data for the past quarter
    cprice_filtered <- cprice_filtered %>% filter(time_posted >= max_date - 91)
    #filter out data for models older than 1980
    cprice_filtered <- cprice_filtered %>% filter(time_posted >= 1980)
    #plot the graph
    ggplot(cprice_filtered, aes(x = time_posted, y = median_price)) + geom_line() + labs(x = "Date", y = "Median Price", title = "Median Price Over Time") + theme(plot.title = element_text(hjust = 0.5))

  
  })
  output$plot2 <- renderPlot({
    # Top 10 most popular models of cars posted on Craigslist in the past quarter (based on number of posts)
    cdata %>% 
      select(make, model) %>% 
      group_by(make, model) %>% 
      summarise(count = n()) %>% 
      arrange(desc(count)) %>% 
      head(10) %>% 
      ggplot(aes(x = reorder(paste(make, model, sep = " - "), count), y = count, fill = count))  +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(x = "Model - Make", y = "Number of Posts", title = "Top 10 Most Popular Models by Make") +  
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  # Render the plot based on user input
  output$plot3 <- renderPlot({
    # histogram of number of posts by year of car listed
    cdata %>%
      select(year) %>%
      group_by(year) %>%
      summarise(count = n()) %>%
      filter(year >= 1980) %>%
      ggplot(aes(x = year, y = count, fill = count)) +
      geom_bar(stat = "identity", width = 0.7, position = "dodge") +
      labs(x = "Year", y = "Number of Posts") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  #table of top 10 most popular locations based on percentage of posts
  output$table1 <- renderTable({
    cdata %>% 
      select(location) %>% 
      group_by(location) %>% 
      summarise(count = n()) %>% 
      mutate(percentage = 100 * count / sum(count)) %>% 
      arrange(desc(percentage)) %>% 
      head(10)
  })
  output$text1 <- renderText({
    #output key findings in bullet points
    "Key Findings:
    - The median price of cars posted on Craigslist has been increasing over the past quarter
    - The most popular models of cars posted on Craigslist are Ford F150s, Honda Civics and Toyota Camrys
    - The most popular locations for cars posted on Craigslist are San Francisco, Los Angeles, and Phoenix"

  })
}
# Run the Shiny app
shinyApp(ui, server)