library(httr)
library(jsonlite)
options(tigris_use_cache = TRUE)
library(shiny)
library(tigris)
library(leaflet)
library(dplyr)
library(stringr)
library(censusapi)
library(tidycensus)
library(sf)
library(lubridate)

# --- Data Setup ---
target_counties <- c("Shenandoah", "Rockingham", "Rockbridge", "Franklin", 
                     "Augusta", "Clarke", "Frederick", "Page",
                     "Warren", "Pittsylvania")

# Get counties
va_counties = counties(state = "VA", cb = TRUE, class = "sf")
va_counties <- va_counties %>% filter(NAME %in% target_counties)

# Get centroids
va_counties$centroid <- st_centroid(va_counties$geometry)
coords <- st_coordinates(va_counties$centroid)
va_counties$lat <- coords[,2]
va_counties$lon <- coords[,1]

# Download weather data from Open-Meteo
full_data <- list()
for (i in 1:nrow(va_counties)) {
  lat <- va_counties$lat[i]
  lon <- va_counties$lon[i]
  name <- va_counties$NAME[i]
  
  res <- GET("https://archive-api.open-meteo.com/v1/archive",
             query = list(
               latitude = lat,
               longitude = lon,
               start_date = "2024-01-01",
               end_date = as.character(Sys.Date() - 1),
               daily = "temperature_2m_mean,relative_humidity_2m_mean",
               timezone = "America/New_York"
             ))
  
  if (status_code(res) == 200) {
    data <- fromJSON(content(res, as = "text"))
    if (!is.null(data$daily)) {
      daily <- data$daily
      daily$county <- name
      full_data[[name]] <- as.data.frame(daily)
    }
  } else {
    warning(paste("Failed to get data for", name))
  }
}

weather_data <- bind_rows(full_data)
weather_data$temp_f <- weather_data$temperature_2m_mean * 9/5 + 32
weather_data$time <- as.Date(weather_data$time)  # Ensure time is Date

temp_f <- weather_data$temp_f
relative <- weather_data$relative_humidity_2m_mean
weather_data$humidity_index <- (temp_f) - (0.55 - 0.0055 * relative) * 
  (temp_f - 58)

# --- Shiny App ---
ui <- fluidPage(
  titlePanel("Virginia County Daily Weather"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("selected_date",
                  "Select Date:",
                  min = min(weather_data$time),
                  max = max(weather_data$time),
                  value = min(weather_data$time),
                  timeFormat = "%Y-%m-%d",
                  step = 1,
                  animate = animationOptions(interval = 50, loop = TRUE)),
      radioButtons( 
        inputId = "radio", 
        label = "What weather feature do you want to see?", 
        choices = list( 
          "Temperature (F)" = 1, 
          "Temperature-Humidity Index (THI)" = 2))
    ),
    
    mainPanel(
      leafletOutput("temp_map", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  
  reactive_data_weather <- reactive({
    weather_data %>%
      filter(time == input$selected_date) %>%
      group_by(county) %>%
      summarise(avg_temp_f = mean(temp_f, na.rm = TRUE))
  })
  reactive_data_humidity <- reactive({
    weather_data %>%
      filter(time == input$selected_date) %>%
      group_by(county) %>%
      summarise(humidity_mean = mean(humidity_index, na.rm = TRUE))
  })
  
  output$temp_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -78.5, lat = 37.5, zoom = 7)
  })
  
  observe({
    if (input$radio == 1) {
      map_data <- reactive_data_weather()
      merged <- va_counties %>%
        left_join(map_data, by = c("NAME" = "county"))
      
      pal <- colorNumeric(
        c("purple", "blue", "green", "yellow", "orange", "red"),
        domain = weather_data$temp_f,
        na.color = "NA"
      )
      
      merged$label <- paste0(merged$NAME, ": ",
                             round(merged$avg_temp_f, 1), "°F on ",
                             input$selected_date)
      
      leafletProxy("temp_map", data = merged) %>%
        clearShapes() %>%
        addPolygons(
          fillColor = ~pal(avg_temp_f),
          weight = 1,
          color = "black",
          fillOpacity = 0.7,
          label = ~label
        ) %>%
        clearControls() %>%
        addLegend("bottomright", pal = pal, values = weather_data$temp_f,
                  title = "Average <br> Temp (°F)")
    } else {
      map_data <- reactive_data_humidity()
      merged <- va_counties %>%
        left_join(map_data, by = c("NAME" = "county"))
      
      pal <- colorNumeric(
        c("purple", "blue", "green", "yellow", "orange", "red"),
        domain = weather_data$humidity_index,
        na.color = "NA"
      )
      
      merged$label <- paste0(merged$NAME, ": ",
                             round(merged$humidity_mean, 1), " on ",
                             input$selected_date)
      
      leafletProxy("temp_map", data = merged) %>%
        clearShapes() %>%
        addPolygons(
          fillColor = ~pal(humidity_mean),
          weight = 1,
          color = "black",
          fillOpacity = 0.7,
          label = ~label
        ) %>%
        clearControls() %>%
        addLegend("bottomright", pal = pal,
                  values = weather_data$humidity_index,
                  title = "Temperature <br> Humidity <br> Index")
    }
  })
  
}

shinyApp(ui, server)
