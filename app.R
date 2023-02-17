library(shiny)
library(ggplot2)
library(dplyr)

# Load dataset
data <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/weather_forecasts.csv")

# Define UI
ui <- fluidPage(
  # Add title
  titlePanel("Temperature or Precipitation Trends"),
  
  # Add sidebar layout
  sidebarLayout(
    # Add sidebar panel with inputs
    sidebarPanel(
      
      # Add slider input for selecting range of months
      sliderInput("month_range", "Select Month Range:",
                  min = 1, max = 12, value = c(1, 12)),
      

      # Add dropdown menu input for selecting state
      selectInput("state", "Select State:",
                  choices = unique(data$state)),
      
      # Add dropdown menu input for selecting city
      selectInput("city", "Select City:",
                  choices = unique(data$city)),
      
      # Add radio button input for selecting temperature or precipitation
      radioButtons("data_type", "Select Data Type:",
                   choices = c("Temperature", "Precipitation"),
                   selected = "Temperature")
    ),
    
    # Add main panel with plot output
    mainPanel(
      plotOutput("line_plot")
    )
  )
)

# Define server
server <- function(input, output) {
  # Filter data based on user inputs
  filtered_data <- reactive({
    data %>%
      filter(month >= input$month_range[1], month <= input$month_range[2]) %>%
      filter(state == input$state, city == input$city)
  })
  
  # Create line plot based on filtered data and user data type input
  output$line_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = month, y = ifelse(input$data_type == "Temperature", observed_temp, observed_precip))) +
      geom_line() +
      labs(x = "Month", y = input$data_type)
  })
}

# Run app
shinyApp(ui, server)
