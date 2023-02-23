library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(sf)
library(countrycode)
library(RColorBrewer)

# Load dataset
weather <- read.csv("data/processed/weather_pro.csv")

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
                  choices = unique(weather$state)),
      
      # Add dropdown menu input for selecting city
      selectInput("city", "Select City:",
                  choices = NULL),
      
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
server <- function(input, output, session) {
  
  observe({
    updateSelectInput(session, "city", 
                      choices = unique(weather$city[weather$state == input$state]))
  })
  
  # Filter data based on user inputs
  filtered_data <- reactive({
    weather %>%
      filter(month >= input$month_range[1], month <= input$month_range[length(input$month_range)]) %>%
      filter(state == input$state, city == input$city) %>%
      group_by(month) %>%
      summarise(observed_temp = mean(observed_temp, na.rm=TRUE),
                observed_precip = mean(observed_precip, na.rm=TRUE))
  })
  
  # Create line plot based on filtered data and user data type input
  output$line_plot <- renderPlot({
    if (input$data_type == "Temperature"){
      ggplot(filtered_data(), aes(x = month, y = observed_temp)) +
        geom_point(color="violetred") +
        geom_line(color="lightblue") +
        scale_x_continuous(breaks = seq(1, 12, by = 1)) +
        labs(x = "Month", y = "Temperature")
    }
    else{
      ggplot(filtered_data(), aes(x = month, y = observed_precip)) +
        geom_point(color="violetred") +
        geom_line(color="lightblue") +
        scale_x_continuous(breaks = seq(1, 12, by = 1)) +
        labs(x = "Month", y = "Precipitation")
    }
  })
  
}

# Run app
shinyApp(ui, server)
