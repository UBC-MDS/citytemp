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
  dashboardHeader(title = "Weather Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem(
        "City Map with Temp/Rain Trends", 
        tabName = "temp_precip_trends", 
        icon = icon("line-chart")
      ),
      menuItem(
        "City Ranking by Temp/Rain", 
        tabName = "city_ranking", 
        icon = icon("bar-chart")
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "temp_precip_trends",
        fluidRow(
          column(
            width = 3,
            # Add slider input for selecting range of months
            sliderInput(
              "month_range",
              "Select Month Range:",
              min = 1,
              max = 12,
              value = c(1, 12)
            ),
            # Add dropdown menu input for selecting state
            selectInput(
              "state",
              "Select State:",
              choices = unique(weather$state)
            ),
            # Add dropdown menu input for selecting city
            selectInput(
              "city",
              "Select City:",
              choices = NULL
            ),
            # Add radio button input for selecting temperature or precipitation
            radioButtons(
              "data_type",
              "Select Type:",
              choices = c("Temperature", "Precipitation"),
              selected = "Temperature"
            ),
            # Add radio button input for selecting temperature metric
            radioButtons(
              "temp_metric",
              "Select temperature metric:",
              choices = c("Celsius", "Fareinheit"),
              selected = "Celsius"
            )
          ),
          column(
            width = 9,
            # Added a row for summary statistics
            fluidRow(
              valueBoxOutput("avgBox"),
              valueBoxOutput("maxBox"),
              valueBoxOutput("minBox")
            ),
            # Add main panel with plot output
            leafletOutput("map"),
            plotOutput("line_plot",
                       height = "200px",
                       width = "100%")
            
          )
        )
      ),
      
      tabItem(
        tabName = "city_ranking",
        fluidRow(
          column(
            width = 3,
            selectInput(
              "statename",
              "Select a state:",
              choices = unique(weather_bar$state)
            ),
            selectInput(
              "highlow",
              "Select highest or lowest temperature:",
              choices = c("Highest" = "high", "Lowest" = "low")
            ),
            selectInput(
              "month",
              "Select a month:",
              choices = unique(weather_bar$month)
            )
          ),
          column(
            width = 9,
            plotOutput("temp_barplot"),
            plotOutput("rain_barplot")
          )
        )
      )
    )
  )
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
  

  # --------------------------------------Map plot start here------------------------------------
  
  map_data <- reactive({
    weather %>% filter(city == input$city ) %>%
      filter(month >= input$month_range[1], month <= input$month_range[length(input$month_range)]) %>%
      group_by(city) %>% 
      summarize(
        avg_temp = round(mean(temp_f, na.rm =TRUE), 2),
        avg_prec = round(mean(precip, na.rm =TRUE), 2),
      )
  })
  
  map_data_color <- reactive({
    weather %>% 
      filter(month >= input$month_range[1], month <= input$month_range[length(input$month_range)]) %>%
      group_by(city) %>% 
      summarize(
        avg_temp = round(mean(temp_f, na.rm =TRUE), 2),
        avg_prec = round(mean(precip, na.rm =TRUE), 2),
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
                    if_else(map_data_color()$avg_temp <= 60, "#FD9A01", 
                            if_else(map_data_color()$avg_temp <= 80, "#FD6104", "#F00505")))
          }
        else{
          if_else(map_data_color()$avg_prec <= 0.05, "#BCD2E8", 
                  if_else(map_data_color()$avg_prec <= 0.15, "#73A5C6", 
                          if_else(map_data_color()$avg_prec <= 0.25, "#2E5984", "#1E3F66")))
        }
        ,
        radius = 5,
        stroke = FALSE,
        fillOpacity = 0.5
      ) %>%
      addLegend(title =
                  if (input$data_type == "Temperature"){
                    "Average Temperature (°F)"}
                else{"Average Precipitation (inches)"}
                ,
                colors =
                  if (input$data_type == "Temperature"){
                    c("#FEF001", "#FD9A01", "#FD6104", "#F00505")}
                else{c("#BCD2E8", "#73A5C6", "#2E5984", "#1E3F66")},
                labels = 
                  if (input$data_type == "Temperature"){
                    c("< 40", "40 - 60", "60 - 80", "80 <")}
                else{c("< 0.05", "0.05 - 0.15", "0.15 - 0.25", "0.25 <")}
      ) %>%
      setView(-100, 40, zoom = 3)
  })
  
# --------------------------------------Line plot start here------------------------------------
  
  
  # Filter data based on user inputs
  line_data <- reactive({
    weather %>% filter(month >= input$month_range[1], month <= input$month_range[length(input$month_range)]) %>%
      filter(state == input$state, city == input$city) %>%
      group_by(month, high_or_low) %>%
      summarise(
        temp_f = mean(temp_f, na.rm = TRUE),
        precip = mean(precip, na.rm = TRUE)
      )
  })
  
  
  # Create line plot based on filtered data and user data type input
  output$line_plot <- renderPlot({
    if (input$data_type == "Temperature") {
      ggplot(line_data(),
             aes(x = month, y = temp_f, col = high_or_low)) +
        geom_point() +
        geom_line() +
        theme_classic() +
        scale_x_continuous(breaks = seq(1, 12, by = 1)) +
        labs(x = "Month", y = "Temperature (°F)", color = "High/Low") +
        ggtitle(paste("Temperature Distribution for", input$city, ",", input$state)) +
        theme(plot.title = element_text(size=20, face="bold", family="Palatino", hjust = 0.5))

    }
    else{
      ggplot(line_data(), aes(x = month, y = precip)) +
        geom_point(color = "violetred") +
        geom_line(color = "lightblue") +
        theme_classic() +
        scale_x_continuous(breaks = seq(1, 12, by = 1)) +
        labs(x = "Month", y = "Precipitation") +
        ggtitle(paste("Precipitation Distribution for", input$city, ",", input$state)) +
        theme(plot.title = element_text(size=20, face="bold", family="Palatino", hjust = 0.5))
    }
  })
  
  
  
  # --------------------------------------City Ranking Tab start here------------------------------------
  
  
  # Filter data based on user input, and calculate avg temp and rain
  bar_data <- reactive({
    weather_bar %>% filter(state == input$statename &
                             high_or_low == input$highlow &
                             month == input$month) %>%
      group_by(city) %>% summarize(
        avg_temp = mean(temp_f, na.rm = TRUE),
        avg_rain = mean(precip, na.rm = TRUE)
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
        avg_temp_ft = round(mean(temp_f, na.rm =TRUE), 2),
        min_temp_ft = min(temp_f, na.rm =TRUE),
        max_temp_ft = max(temp_f, na.rm =TRUE),
        avg_temp_cs = round(mean(temp_c, na.rm =TRUE), 2),
        min_temp_cs = min(temp_c, na.rm =TRUE),
        max_temp_cs = max(temp_c, na.rm =TRUE),
        avg_prec = round(mean(precip, na.rm =TRUE), 2),
        min_prec= min(precip, na.rm =TRUE),
        max_prec = max(precip, na.rm =TRUE)
      )
  })
  
  # create max summary statistic box
  output$maxBox <- renderValueBox({
    if (input$data_type == "Temperature") {
      if (input$temp_metric == "Fareinheit") {
        valueBox(
          paste0(stat_data()$max_temp_ft, "°F"), "MAX", icon = icon("fa-light fa-sun"),
          color = "red")
      }
      else {
         {
           valueBox(
          paste0(stat_data()$max_temp_cs, "°C"), "MAX", icon = icon("fa-light fa-sun"),
          color = "red"
         
      )
      }
      }
    }
    else{
      valueBox(
        paste0(stat_data()$max_prec, " in."), "MAX", icon = icon("fa-regular fa-cloud-sun-rain"),
        color = "red")
    }
  })
  
  # create min summary statistic box
  output$minBox <- renderValueBox({
    if (input$data_type == "Temperature") {
      if (input$temp_metric == "Fareinheit") {
      valueBox(
        paste0(stat_data()$min_temp_ft, "°F"), "MIN", icon = icon("fa-light fa-sun"),
        color = "blue"
      )
      }
      else{
        valueBox(
          paste0(stat_data()$min_temp_cs, "°C"), "MIN", icon = icon("fa-light fa-sun"),
          color = "blue"
        )
      }
    }
    else{
      valueBox(
        paste0(stat_data()$min_prec, " in."), "MIN", icon = icon("fa-regular fa-cloud-sun-rain"),
        color = "blue")
    }
  })
  
  # create avg summary statistic box
  output$avgBox <- renderValueBox({
    if (input$data_type == "Temperature") {
      if (input$temp_metric == "Fareinheit") {
      valueBox(
        paste0(stat_data()$avg_temp_ft, "°F"), "AVG", icon = icon("fa-light fa-sun"),
        color = "green"
      )
      }
      else
      {
        valueBox(
          paste0(stat_data()$avg_temp_cs, "°C"), "AVG", icon = icon("fa-light fa-sun"),
          color = "green"
        )
      }
    }
    else{
      valueBox(
        paste0(stat_data()$avg_prec, " in."), "AVG", icon = icon("fa-regular fa-cloud-sun-rain"),
        color = "green")
    }
  })
  
  
}

# Run app
shinyApp(ui, server)

