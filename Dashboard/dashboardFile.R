#Load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
#Load data
cdata = read.csv("Dashboard\\listing-data.csv")

<<<<<<< HEAD
# Load data and clean out empty make and price values
cdata <- read.csv("Dashboard/listing-data.csv")
cdata <- cdata %>% filter(make != "" & price != "" & time_posted != "" & model != "")
=======
#display column names
names(cdata)
#remove rows with missing price
cdata = cdata %>% filter(!is.na(price))
#select columns for price and time posted
cdata_filtered = cdata %>% select(price, time_posted)
>>>>>>> parent of 828658b (Halfway)

#convert time_posted to date format and group by date
cdata_filtered$time_posted = as.Date(cdata_filtered$time_posted)
head(cdata_filtered)
cdata_filtered = cdata_filtered %>% group_by(time_posted) %>% summarise(price = median(price))

head(cdata_filtered)

#graph of price vs date over past 30 days
ggplot(cdata_filtered, aes(x = time_posted, y = price)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "Date", y = "Price", title = "Price vs Date over past 30 days")

<<<<<<< HEAD
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
                  title = "Median Price Over Time for Selected Make",
                  uiOutput("make_ui"),
                  plotOutput("plot3", height = 500)
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
server <- function(input, output,session) {
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
      labs(x = "Model - Make", y = "Number of Posts", title = "Top 10 Most Popular Models by Make") +      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(plot.title = element_text(hjust = 0.5))
  
  })
  output$text1 <- renderText({
    # Key insights
    "The median price of posted cars has been steady over the past quarter with a dip in mid-August. The top 10 most popular makes are Toyota, Honda, Ford, Chevrolet, Nissan, Hyundai, Jeep, Kia, Dodge, and Volkswagen."
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
    
    # Line graph of median price for the selected make over time
    cprice_filtered <- cdata %>%
      select(price, time_posted, make) %>%
      mutate(time_posted = as.Date(time_posted)) %>%
      filter(price < 1000000 & price > 0) %>%
      group_by(time_posted, make) %>%
      summarise(median_price = median(price))
    
    max_date <- max(cprice_filtered$time_posted)
    
    cprice_filtered <- cprice_filtered %>%
      filter(time_posted >= max_date - 91, make == input$make_ui)
    
    ggplot(cprice_filtered, aes(x = time_posted, y = median_price)) +
      geom_line() +
      geom_smooth(method = "lm", se = FALSE) +
      geom_point() +
      scale_x_date(date_breaks = "1 week", date_labels = "%m/%d/%Y") +
      scale_y_continuous(labels = scales::dollar) +
      labs(x = "Date", y = "Median Price", title = paste("Median Price Over Last Quarter for", input$make_ui)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(plot.title = element_text(hjust = 0.5))
  })
}

# Run the Shiny app
shinyApp(ui, server)
=======
>>>>>>> parent of 828658b (Halfway)
