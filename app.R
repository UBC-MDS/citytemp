library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(leaflet)
library(geosphere)
library(leaflet.extras)
library(sf)
library(shinydashboard)

weather <- read.csv("data/processed/weather_pro.csv")
cities <- read.csv("data/processed/cities.csv")

# Change numeric month from number to name only for bar plots
weather_bar <- weather
weather_bar$month <- month.name[weather_bar$month]

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Citytemp Weather Dashboard"),
  
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Temperature or Precipitation Trends",
      tabName = "temp_precip_trends"
      
    ),
    menuItem(
      "City Ranking by Temp/Rain",
      tabName = "city_ranking"
      
    )
  )),
  dashboardBody(tabItems(
    # Tab 1: Temperature or Precipitation Trends
    tabItem(tabName = "temp_precip_trends",
            fluidRow(
              column(
                width = 4,
                # Add slider input for selecting range of months
                sliderInput(
                  "month_range",
                  "Select Month Range:",
                  min = 1,
                  max = 12,
                  value = c(1, 12)
                ),
                # Add dropdown menu input for selecting state
                selectInput("state",
                            "Select State:",
                            choices = unique(weather$state)),
                # Add dropdown menu input for selecting city
                selectInput("city",
                            "Select City:",
                            choices = NULL),
                # Add radio button input for selecting temperature or precipitation
                radioButtons(
                  "data_type",
                  "Select Data Type:",
                  choices = c("Temperature", "Precipitation"),
                  selected = "Temperature"
                )
              ),
              column(width = 8,
                     
                     # Added a row for summary statistics
                     fluidRow(
                       
                       valueBoxOutput("avgBox"),
                       
                       valueBoxOutput("maxBox"),
                       
                       valueBoxOutput("minBox")
                     ),
                     
                     # Add main panel with plot output
                     plotOutput("line_plot"),
                     leafletOutput("map"))
            )),
    # Tab 2: City Ranking by Temperature/Precipitation
    tabItem(tabName = "city_ranking",
            fluidRow(
              column(
                width = 4,
                selectInput("statename",
                            "Select a state:",
                            choices = unique(weather_bar$state)),
                selectInput(
                  "highlow",
                  "Select highest or lowest temperature:",
                  choices = c("Highest" = "high", "Lowest" = "low")
                ),
                selectInput("month",
                            "Select a month:",
                            choices = unique(weather_bar$month))
              ),
              column(
                width = 8,
                plotOutput("temp_barplot"),
                plotOutput("rain_barplot")
              )
            ))
  ))
)

# Define server logic
server <- function(input, output, session) {
  # Update city and state input based on map clicks
  observeEvent(input$map_marker_click, {
    updateSelectInput(session, "city", selected = input$map_marker_click$id)
    updateSelectInput(session,
                      "state",
                      selected = cities$state[cities$city == input$map_marker_click$id])
  })
  
  # Update city input possible values based on selected state
  observe({
    updateSelectInput(session,
                      "city",
                      choices = unique(weather$city[weather$state == input$state]))
  })
  
  # Filter data based on user inputs
  line_data <- reactive({
    weather %>% filter(month >= input$month_range[1], month <= input$month_range[length(input$month_range)]) %>%
      filter(state == input$state, city == input$city) %>%
      group_by(month, high_or_low) %>%
      summarise(
        observed_temp = mean(observed_temp, na.rm = TRUE),
        observed_precip = mean(observed_precip, na.rm = TRUE)
      )
  })
  
  # Create line plot based on filtered data and user data type input
  output$line_plot <- renderPlot({
    if (input$data_type == "Temperature") {
      ggplot(line_data(),
             aes(x = month, y = observed_temp, col = high_or_low)) +
        geom_point() +
        geom_line() +
        scale_x_continuous(breaks = seq(1, 12, by = 1)) +
        labs(x = "Month", y = "Temperature (°F)", color = "High/Low")
    }
    else{
      ggplot(line_data(), aes(x = month, y = observed_precip)) +
        geom_point(color = "violetred") +
        geom_line(color = "lightblue") +
        scale_x_continuous(breaks = seq(1, 12, by = 1)) +
        labs(x = "Month", y = "Precipitation")
    }
  })
  
  # --------------------------------------Map plot start here------------------------------------
  
  map_data <- reactive({
    weather %>% filter(city == input$city ) %>%
      filter(month >= input$month_range[1], month <= input$month_range[length(input$month_range)]) %>%
      group_by(city) %>% 
      summarize(
        avg_temp = round(mean(observed_temp, na.rm =TRUE), 2),
        avg_prec = round(mean(observed_precip, na.rm =TRUE), 2),
      )
  })
  
  map_data_color <- reactive({
    weather %>% 
      filter(month >= input$month_range[1], month <= input$month_range[length(input$month_range)]) %>%
      group_by(city) %>% 
      summarize(
        avg_temp = round(mean(observed_temp, na.rm =TRUE), 2),
        avg_prec = round(mean(observed_precip, na.rm =TRUE), 2),
      )
  })
  
  # Create leaflet map
  output$map <- renderLeaflet({
    leaflet(cities) |>
      addTiles() |>
      addCircleMarkers(
        ~ lon,
        ~ lat,
        popup = paste0(
          "<B>City: </B>",
          cities$city,
          "<br>",
          "<B>State: </B>",
          cities$state,
          "<br>",
          "<B>Elevation: </B>",
          cities$elevation,
          " m<br>",
          "<B>Distance to Coast: </B>",
          cities$distance_to_coast,
          " mi<br>"
        ),
        layerId = cities$city,
        label = cities$city,
        color = 
          if (input$data_type == "Temperature") {
            if_else(map_data_color()$avg_temp <= 40, "#FEF001", 
                        if_else(map_data_color()$avg_temp <= 50, "#FFCE03",
                                if_else(map_data_color()$avg_temp <= 60, "#FD9A01", 
                                        if_else(map_data_color()$avg_temp <= 70, "#FD6104",
                                                if_else(map_data_color()$avg_temp <= 80, "#FF2C05", "#F00505")))))
          }
        else{
          if_else(map_data_color()$avg_prec <= 0.05, "#BCD2E8", 
                  if_else(map_data_color()$avg_prec <= 0.10, "#91BAD6",
                          if_else(map_data_color()$avg_prec <= 0.15, "#73A5C6", 
                                  if_else(map_data_color()$avg_prec <= 0.20, "#528AAE",
                                          if_else(map_data_color()$avg_prec <= 0.25, "#2E5984", "#1E3F66")))))
        }
        ,
        radius = 5,
        stroke = FALSE,
        fillOpacity = 0.5
      ) |>
      setView(-100, 40, zoom = 3.5)
  })
  
  
  # --------------------------------------City Ranking Tab start here------------------------------------
  
  
  # Filter data based on user input, and calculate avg temp and rain
  bar_data <- reactive({
    weather_bar %>% filter(state == input$statename &
                             high_or_low == input$highlow &
                             month == input$month) %>%
      group_by(city) %>% summarize(
        avg_temp = mean(observed_temp, na.rm = TRUE),
        avg_rain = mean(observed_precip, na.rm = TRUE)
      )
  })
  
  
  
  # Create bar chart of cities by temperature
  output$temp_barplot <- renderPlot({
    ggplot(bar_data(), aes(x = avg_temp, y = reorder(city, avg_temp),fill= avg_temp)) +
      geom_bar(stat = "identity") + scale_fill_gradient(low="yellow", high="red") +
      labs(
        x = "Average Temperature",
        y = "City",
        title = paste0(
          "Cities in ",
          input$statename,
          " by average ",
          input$highlow,
          "est temperature in ",
          input$month
        )
      ) 
  })
  
  
  # Create bar chart of cities by precipitation
  output$rain_barplot <- renderPlot({
    ggplot(bar_data(), aes(x = avg_rain, y = reorder(city, avg_rain), fill = avg_rain)) +
      geom_bar(stat = "identity") + scale_fill_gradient(low="lightblue", high="darkblue") +
      labs(
        x = "Average Precipitation",
        y = "City",
        title = paste0(
          "Cities in ",
          input$statename,
          " by average precipitation in ",
          input$month
        )
      )
  })
  
  # --------------------------------------summary statistics box start here------------------------------------
  
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

