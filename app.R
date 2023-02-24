library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(leaflet)
library(geosphere)
library(leaflet.extras)
library(sf)


# Load dataset
weather <- read.csv("data/processed/weather_pro.csv")
cities <- read.csv("data/processed/cities.csv")

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
                   selected = "Temperature"),
    ),
    
    # Add main panel with plot output
    mainPanel(
      plotOutput("line_plot"),
      leafletOutput("map")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Update city and state input based on map clicks
  observeEvent(input$map_marker_click, {
    updateSelectInput(session, "city", selected = input$map_marker_click$id)
    updateSelectInput(session, "state", selected = cities$state[cities$city == input$map_marker_click$id])
  })
  
  # Update city input possible values based on selected state
  observe({
    updateSelectInput(session, "city", 
                      choices = unique(weather$city[weather$state == input$state]))
    
  })
  # observe({
  #   updateSelectInput(session, "state", 
  #                     choices = unique(weather$state[weather$city == input$city]))
  # })
  
  
  
  # Filter data based on user inputs
  line_data <- reactive({
    weather %>%
      filter(month >= input$month_range[1], month <= input$month_range[length(input$month_range)]) %>%
      filter(state == input$state, city == input$city) %>%
      group_by(month, high_or_low) %>%
      summarise(observed_temp = mean(observed_temp, na.rm=TRUE),
                observed_precip = mean(observed_precip, na.rm=TRUE))
  })
  
  # Create line plot based on filtered data and user data type input
  output$line_plot <- renderPlot({
    if (input$data_type == "Temperature"){
      ggplot(line_data(), aes(x = month, y = observed_temp, col=high_or_low)) +
        geom_point() +
        geom_line() +
        scale_x_continuous(breaks = seq(1, 12, by = 1)) +
        labs(x = "Month", y = "Temperature (°F)", color="High/Low")
    }
    else{
      ggplot(line_data(), aes(x = month, y = observed_precip)) +
        geom_point(color="violetred") +
        geom_line(color="lightblue") +
        scale_x_continuous(breaks = seq(1, 12, by = 1)) +
        labs(x = "Month", y = "Precipitation")
    }
  })
  
  # Create leaflet map
  output$map <- renderLeaflet({
    leaflet(cities) |>
      addTiles() |>
      addCircleMarkers(~lon,
                       ~lat,
                       popup = paste0("City: ", cities$city, "<br>",
                                      "State: ", cities$state, "<br>",
                                      "Elevation: ", cities$elevation, " m<br>",
                                      "Distance to Coast: ", cities$distance_to_coast, " mi<br>",
                                      "Average Annual Precipitation: ", cities$avg_annual_precip, " in"),
                       layerId = cities$city,
                       label = cities$city,
                       color = "navy",
                       radius = 5,
                       stroke = FALSE,
                       fillOpacity = 0.4) |> 
      setView(-100, 40, zoom = 3.3)
  })
  
}

# Run app
shinyApp(ui, server)