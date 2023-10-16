#Load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
#Load data
cdata = read.csv("Dashboard\\listing-data.csv")

# Load data and clean out empty make and price values
cdata <- read.csv("Dashboard/listing-data.csv")
cdata <- cdata %>% filter(make != "" & price != "" & time_posted != "" & model != "")

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
                  title = "Median Price Over Time",
                  plotOutput("plot1", height = 250)
                ),
                box(
                  title = "Key Insights",
                  textOutput("text1")
                ),
                box(
                  title = "Top 10 Most Popular Makes",
                  plotOutput("plot2", height = 400)
                ),
                box(
                  title = "Select Make",
                  uiOutput("make_ui"),
                  plotOutput("plot3", height = 400)
                ),
                box(
                  title = "Top 10 Most Popular Locations",
                  tableOutput("table1")
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
    plot(cprice_filtered$time_posted, cprice_filtered$median_price, type = "l", xlab = "Date", ylab = "Median Price", main = "Median Price Over Last Quarter") +
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
      labs(x = "Model - Make", y = "Number of Posts", title = "Top 10 Most Popular Models by Make") +   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(plot.title = element_text(hjust = 0.5))
  
  })
  output$text1 <- renderText({
    # Key insights
    "This dashboard provides an a quarterly overview of the used car market based on listings collected from Craigslist. Included are the median price of all cars over time, the top 10 most popular makes and models, and top 10 most popular locations based on number of posts in the past quarter."
  })
  output$make_ui <- renderUI({
    # Dropdown menu for selecting make based on top 10 most popular makes
    selectInput("make", "Select Make", choices = c("Toyota", "Honda", "Ford", "Chevrolet", "Nissan", "Hyundai", "Jeep", "Kia", "Dodge", "Volkswagen"))
  })
  # Set a default value for the selectInput
  observe({
    updateSelectInput(session, "make", selected = "Toyota")
  })
  
  # Render the plot based on user input
  output$plot3 <- renderPlot({
    req(input$make_ui)  # Ensure input$make is available
    
    # histogram of number of posts by year of car listed
    cdata %>%
      select(year) %>%
      group_by(year) %>%
      summarise(count = n()) %>%
      filter(year >= 1980) %>%
      ggplot(aes(x = year,fill = ..count..)) +
      geom_histogram(binwidth = 1, fill = "blue", color = "black") +
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
}
# Run the Shiny app
shinyApp(ui, server)
