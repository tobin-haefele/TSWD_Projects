#Load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)

# Load data and clean out empty make and price values
cdata <- read.csv("Dashboard/listing-data.csv")
cdata <- cdata %>% filter(make != "" & price != "" & time_posted != "" & model != "" & condition != "")

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = div(
      img(src = "Dashboard/kbbl.png", height = "50px", style = "vertical-align: middle;"),
      "Kelly Blue Book Dashboard"
        )
      ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Strategic Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = "Median Price Over The Past Quarter",
                  plotOutput("plot1", height = 400)
                ),
                box(
                  title = "Top 10 Most Popular Models",
                  plotOutput("plot2", height = 400)
                ),
                box(
                  title = "Top 10 Most Popular Locations",
                  tableOutput("table1")
                ),
                box(
                  title = "Key Takeaways",
                  textOutput("text1")
                )
                )
              )
      ),
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
         )
        )
      )
# Define server logic
server <- function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({

    #Selecting data for price graph
    cprice_filtered <- cdata %>% select(price, time_posted,make)
    cprice_filtered$time_posted <- as.Date(cprice_filtered$time_posted)
    #filtering out the outliers price over 1 million or equal to 0
    cprice_filtered <- cprice_filtered %>% filter(price < 1000000 & price > 0)
    #group by date and price and get the median price for each day
    cprice_filtered <- cprice_filtered %>% group_by(time_posted) %>% summarise(median_price = median(price))
    #get max date for filtering
    max_date <- max(cprice_filtered$time_posted)
    #filter out data for the past quarter
    cprice_filtered <- cprice_filtered %>% filter(time_posted >= max_date - 91)
    #plotting the graph
    plot(cprice_filtered$time_posted, cprice_filtered$median_price, type = "l", xlab = "Date", ylab = "Median Price") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      geom_smooth(method = "lm", se = FALSE) +
      geom_point() +
      scale_x_date(date_breaks = "1 week", date_labels = "%m/%d/%Y") +
      scale_y_continuous(labels = scales::dollar) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  output$plot2 <- renderPlot({
    # Top 10 most popular models of cars posted on Craigslist in the past quarter (based on number of posts)
    cdata %>% 
      select(make, model) %>% 
      group_by(make, model) %>% 
      summarise(count = n()) %>% 
      arrange(desc(count)) %>% 
      head(10) %>% 
      ggplot(aes(x = reorder(paste(make, model, sep = " - "), count), y = count, fill = make))  +
      geom_bar(stat = "identity") +
      labs(x = "Model - Make", y = "Number of Posts") +   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(plot.title = element_text(hjust = 0.5))
  
  })
  output$text1 <- renderText({
    # Key insights
    "This dashboard provides an a quarterly overview of the used car market based on listings collected from Craigslist. Included are the median price of all cars over the past quarter, the top 10 most popular models listed, and top 10 most popular locations based on number of posts in the past quarter."
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
}
# Run the Shiny app
shinyApp(ui, server)