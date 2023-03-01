library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(leaflet)
library(geosphere)
library(leaflet.extras)
library(sf)
library(shinydashboard)


# Load dataset
weather <- read.csv("data/processed/weather_pro.csv")
cities <- read.csv("data/processed/cities.csv")

# Define UI
ui <- dashboardPage(
  # Add title
  dashboardHeader(title = "City Weather"),
  
  # Add sidebar layout
  dashboardSidebar(# Add slider input for selecting range of months
    sliderInput("month_range", "Select Month Range:",
                min = 1, max = 12, value = c(1, 12)),
    
    
    # Add dropdown menu input for selecting state
    selectInput("state", "Select State:",
                choices = unique(weather$state)),
    
    # Add dropdown menu input for selecting city
    selectInput("city", "Select City:",
                choices = NULL),
    
    # Add radio button input for selecting temperature or precipitation
    radioButtons("data_type", "Select Type:",
                 choices = c("Temperature", "Precipitation"),
                 selected = "Temperature")),
  dashboardBody(
      # Added a row for summary statistics
    fluidRow(
      
      valueBoxOutput("avgBox"),
      
      valueBoxOutput("maxBox"),
      
      valueBoxOutput("minBox")
    ),
    box(plotOutput("line_plot")),
    box(leafletOutput("map"))
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
  
  # data processing for summary statistics
  stat_data <- reactive({
    weather %>% filter(city == input$city ) %>%
      filter(month >= input$month_range[1], month <= input$month_range[length(input$month_range)]) %>%
      group_by(city) %>% 
      summarize(
        avg_temp = round(mean(observed_temp, na.rm =TRUE), 2),
        min_temp = min(observed_temp, na.rm =TRUE),
        max_temp = max(observed_temp, na.rm =TRUE),
        avg_prec = round(mean(observed_precip, na.rm =TRUE), 2),
        min_prec= min(observed_precip, na.rm =TRUE),
        max_prec = max(observed_precip, na.rm =TRUE)
      )
  })
  
  # create max summary statistic box
  output$maxBox <- renderValueBox({
    if (input$data_type == "Temperature") {
      valueBox(
        paste0(stat_data()$max_temp, "°F"), "MAX", icon = icon("fa-light fa-sun"),
        color = "red"
      )
    }
    else{
      valueBox(
        paste0(stat_data()$max_prec), "MAX", icon = icon("fa-regular fa-cloud-sun-rain"),
        color = "red")
    }
  })
  
  # create min summary statistic box
  output$minBox <- renderValueBox({
    if (input$data_type == "Temperature") {
      valueBox(
        paste0(stat_data()$min_temp, "°F"), "MIN", icon = icon("fa-light fa-sun"),
        color = "blue"
      )
    }
    else{
      valueBox(
        paste0(stat_data()$min_prec), "MIN", icon = icon("fa-regular fa-cloud-sun-rain"),
        color = "blue")
    }
  })
  
  # create avg summary statistic box
  output$avgBox <- renderValueBox({
    if (input$data_type == "Temperature") {
      valueBox(
        paste0(stat_data()$avg_temp, "°F"), "AVG", icon = icon("fa-light fa-sun"),
        color = "green"
      )
    }
    else{
      valueBox(
        paste0(stat_data()$avg_prec), "AVG", icon = icon("fa-regular fa-cloud-sun-rain"),
        color = "green")
    }
  })
  
}

# Run app
shinyApp(ui, server)
