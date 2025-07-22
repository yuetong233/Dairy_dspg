options(tigris_use_cache = TRUE)
library(DT)
library(shiny)
library(tigris)
library(sf)
library(leaflet)
library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(scales)
library(rnassqs)
library(bslib)
library(ggimage)
library(tidyr)
library(purrr)
library(httr)
library(jsonlite)
library(blsAPI)
library(dataRetrieval)
library(plotly)
library(taskscheduleR)

# --- Data Setup ---
target_counties <- c("Shenandoah", "Rockingham", "Rockbridge", "Franklin", 
                     "Augusta", "Clarke", "Frederick", "Page",
                     "Warren", "Pittsylvania")

# Get counties
va_counties <- counties(state = "VA", cb = TRUE, class = "sf") %>%
  filter(NAME %in% target_counties)

# Get centroids
va_counties$centroid <- st_centroid(va_counties$geometry)
coords <- st_coordinates(va_counties$centroid)
va_counties$lat <- coords[, 2]
va_counties$lon <- coords[, 1]

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
               start_date = as.character(Sys.Date() - days(370)),
               end_date = as.character(Sys.Date() - days(5)),
               daily = "temperature_2m_max,temperature_2m_min,temperature_2m_mean,relative_humidity_2m_max,relative_humidity_2m_min,relative_humidity_2m_mean",
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
    warning(paste("Failed to get data for", name, "Status:", status_code(res)))
    print(content(res, as = "text"))
  }
}
full_data_hour <- list()
for (i in 1:nrow(va_counties)) {
  lat <- va_counties$lat[i]
  lon <- va_counties$lon[i]
  name <- va_counties$NAME[i]
  
  res <- GET("https://api.open-meteo.com/v1/forecast?hourly=temperature_2m,relative_humidity_2m&timezone=America%2FNew_York&past_days=2&forecast_days=3&wind_speed_unit=mph&temperature_unit=fahrenheit&precipitation_unit=inch",
             query = list(
               latitude = lat,
               longitude = lon
             )
  )
  if (status_code(res) == 200) {
    data <- fromJSON(content(res, as = "text"))
    if (!is.null(data$hourly)) {
      hourly <- data$hourly
      if (all(c("time", "temperature_2m", "relative_humidity_2m") %in% names(hourly))) {
        df <- data.frame(
          time = as.POSIXct(hourly$time, format = "%Y-%m-%dT%H:%M", tz = "America/New_York"),
          temperature_2m = hourly$temperature_2m,
          relative_humidity_2m = hourly$relative_humidity_2m,
          county = name
        )
        full_data_hour[[name]] <- df
      }
    }
  } else {
    warning(paste("Failed to get data for", name, "Status:", status_code(res)))
    print(content(res, as = "text"))
  }
}
if (length(full_data_hour) == 0) {
  stop("No weather data was retrieved from Open-Meteo. Please check your API connection.")
}
weather_data_hour <- bind_rows(full_data_hour)
# Check time column
if (!"time" %in% colnames(weather_data_hour) || !inherits(weather_data_hour$time, "POSIXct")) {
  stop("The 'time' column is missing or not properly formatted.")
}
weather_data <- bind_rows(full_data)
weather_data$time <- as.Date(weather_data$time)

# Convert temps to Fahrenheit
weather_data_hour <- weather_data_hour %>%
  mutate(
    thi_mean_hour = temperature_2m - (0.55 - 0.0055 * relative_humidity_2m) * (temperature_2m - 58)
  )
weather_data <- weather_data %>%
  mutate(
    temp_f_mean = temperature_2m_mean * 9/5 + 32,
    temp_f_max = temperature_2m_max * 9/5 + 32,
    temp_f_min = temperature_2m_min * 9/5 + 32,
    thi_mean = temp_f_mean - (0.55 - 0.0055 * relative_humidity_2m_mean) * (temp_f_mean - 58),
    thi_max = temp_f_max - (0.55 - 0.0055 * relative_humidity_2m_max) * (temp_f_max - 58),
    thi_min = temp_f_min - (0.55 - 0.0055 * relative_humidity_2m_min) * (temp_f_min - 58),
    month = format(time, "%Y-%m")
  )

# Tally THI categories
thi_summary <- weather_data %>%
  group_by(county, month) %>%
  summarise(
    days_over_72 = sum(thi_max > 72, na.rm = TRUE),
    days_below_68 = sum(thi_min < 68, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(days_over_72, days_below_68),
               names_to = "THI_category", values_to = "days") %>%
  mutate(
    THI_category = factor(THI_category,
                          levels = c("days_over_72", "days_below_68"),
                          labels = c("Above 72", "Below 68"))
  )

# --- Shiny App ---
ui <- fluidPage(
  titlePanel("Virginia County Daily Weather"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Use the slider below to choose a specific date. This will update the map to show weather conditions across Virginia counties for that day. Press the play button to view changes over time."),
      
      sliderInput("selected_date",
                  "Select Date:",
                  min = min(weather_data$time),
                  max = max(weather_data$time),
                  value = min(weather_data$time),
                  timeFormat = "%Y-%m-%d",
                  step = 1,
                  animate = animationOptions(interval = 50, loop = TRUE)),
      
      helpText("Choose which weather factor you'd like to view on the map: temperature, humidity, or the Temperature-Humidity Index (THI)."),
      
      radioButtons( 
        inputId = "radio", 
        label = "Select Weather Factor:", 
        choices = list( 
          "Temperature (°F)" = 1, 
          "Relative Humidity (%)" = 2,
          "Temperature-Humidity Index (THI)" = 3)),
      
      helpText("Use the dropdown to look at a specific county. This will display temperature and humidity trends over the past few days."),
      
      helpText("Milk production can drop when temperatures go above 70°F or below 40°F, or when humidity rises above 60% or drops below 40%."),
      
      selectInput("selected_county",
                  "Choose a County:",
                  choices = sort(unique(weather_data$county)),
                  selected = unique(weather_data$county)[1]),
      
      helpText("In the charts below, the green shaded area shows the optimal range of temperature and humidity for consistent milk production.")
    ),
    
    
    mainPanel(
      leafletOutput("temp_map", height = "400px"),
      br(),
      br(),
      plotlyOutput("temp_line_plot", height = "400px"),
      br(),
      plotlyOutput("hum_line_plot", height = "400px"),
      br(),
      plotlyOutput("thi_line_plot", height = "400px"),
      br(),
      uiOutput("table_title"),
      DTOutput("data_table"),
      br()
    )
  )
)

server <- function(input, output, session) {
  
  reactive_data_weather <- reactive({
    weather_data %>%
      filter(time == input$selected_date) %>%
      group_by(county) %>%
      summarise(avg_temp_f = mean(temp_f_mean, na.rm = TRUE))
  })
  
  reactive_data_humidity <- reactive({
    weather_data %>%
      filter(time == input$selected_date) %>%
      group_by(county) %>%
      summarise(avg_humidity = mean(relative_humidity_2m_mean, na.rm = TRUE))
  })
  
  reactive_data_thi <- reactive({
    weather_data %>%
      filter(time == input$selected_date) %>%
      group_by(county) %>%
      summarise(thi_value = mean(thi_mean, na.rm = TRUE))
  })
  
  output$temp_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -79.0, lat = 38.0, zoom = 7)
  })
  
  observe({
    if (input$radio == 1) {
      map_data <- reactive_data_weather()
      merged <- va_counties %>%
        left_join(map_data, by = c("NAME" = "county"))
      
      pal <- colorNumeric(
        c("purple", "blue", "green", "yellow", "orange", "red"),
        domain = weather_data$temp_f_mean,
        na.color = "transparent"
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
        addLegend("bottomright", pal = pal, values = weather_data$temp_f_mean,
                  title = "Mean <br> Temperature <br> (°F)")
      
    } else if (input$radio == 2) {
      map_data <- reactive_data_humidity()
      merged <- va_counties %>%
        left_join(map_data, by = c("NAME" = "county"))
      
      pal <- colorNumeric(
        c("purple", "blue", "green", "yellow", "orange", "red"),
        domain = weather_data$relative_humidity_2m_mean,
        na.color = "transparent"
      )
      
      merged$label <- paste0(merged$NAME, ": ",
                             round(merged$avg_humidity, 1), "% RH on ",
                             input$selected_date)
      
      leafletProxy("temp_map", data = merged) %>%
        clearShapes() %>%
        addPolygons(
          fillColor = ~pal(avg_humidity),
          weight = 1,
          color = "black",
          fillOpacity = 0.7,
          label = ~label
        ) %>%
        clearControls() %>%
        addLegend("bottomright", pal = pal,
                  values = weather_data$relative_humidity_2m_mean,
                  title = "Mean <br> Relative <br> Humidity (%)")
      
    } else {
      map_data <- reactive_data_thi()
      merged <- va_counties %>%
        left_join(map_data, by = c("NAME" = "county"))
      
      pal <- colorNumeric(
        c("purple", "blue", "green", "yellow", "orange", "red"),
        domain = weather_data$thi_mean,
        na.color = "transparent"
      )
      
      merged$label <- paste0(merged$NAME, ": ",
                             round(merged$thi_value, 1), " THI on ",
                             input$selected_date)
      
      leafletProxy("temp_map", data = merged) %>%
        clearShapes() %>%
        addPolygons(
          fillColor = ~pal(thi_value),
          weight = 1,
          color = "black",
          fillOpacity = 0.7,
          label = ~label
        ) %>%
        clearControls() %>%
        addLegend("bottomright", pal = pal,
                  values = weather_data$thi_mean,
                  title = "Temperature <br> Humidity <br> Index (THI)")
    }
  })
  
  output$temp_line_plot <- renderPlotly({
    
    temp_data <- weather_data_hour %>%
      filter(county == input$selected_county) 
    temp_data$color <- ifelse(temp_data$time >= Sys.time(), "Forecast", "Historical")
    dummy_df <- data.frame(
      x = c(Sys.time(), Sys.time()),
      y = c(70, 70)
    )
    p <- ggplot(temp_data, aes(x = time)) +
      annotate("segment",
               x = min(temp_data$time), xend = max(temp_data$time),
               y = 70, yend = 70,
               color = "#00a000",
               alpha = 0.5,
               linewidth = 0.5)+
      annotate("segment",
               x = min(temp_data$time), xend = max(temp_data$time),
               y = 40, yend = 40,
               color = "#00a000",
               alpha = 0.5,
               linewidth = 0.5) +
      annotate("rect",
               xmin = min(temp_data$time), xmax = max(temp_data$time),
               ymin = 40, ymax = 70,
               fill = "#00a000", alpha = 0.2) +
      geom_line(aes(y = temperature_2m, color = color)) +
      geom_line(data = dummy_df, aes(x = x, y = y, color = "Optimal<br>Range"), size=0.5, alpha=0.7) +
      geom_point(aes(y = temperature_2m, color = color,
                     text = paste0(format(time, "%B %d, %Y %I:%M %p"), 
                                   "<br>Temp: ", round(temperature_2m, 1), "°F")), size=1) +
      scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 day") +
      labs(
        title = paste("Recent Temperatures for", input$selected_county),
        x = "Time", y = "Temperature (Fahrenheit)",
        color = ""
      ) +
      scale_color_manual(values = c("Forecast" = "steelblue2", "Historical" = "hotpink", "Optimal<br>Range" = "#00a000")) +
      theme_minimal()
    
    
    plotly::ggplotly(p, tooltip = "text")
  })
  
  output$hum_line_plot <- renderPlotly({
    
    hum_data <- weather_data_hour %>%
      filter(county == input$selected_county)
    hum_data$color <- ifelse(hum_data$time > Sys.time(), "Forecast", "Historical")
    dummy_df <- data.frame(
      x = c(Sys.time(), Sys.time()),
      y = c(60, 60)
    )
    p_humid <- ggplot(hum_data, aes(x = time)) +
      annotate("segment",
               x = min(hum_data$time), xend = max(hum_data$time),
               y = 60, yend = 60,
               color = "#00a000",
               alpha = 0.5,
               linewidth = 0.5)+
      annotate("segment",
               x = min(hum_data$time), xend = max(hum_data$time),
               y = 40, yend = 40,
               color = "#00a000",
               alpha = 0.5,
               linewidth = 0.5) +
      annotate("rect",
               xmin = min(hum_data$time), xmax = max(hum_data$time),
               ymin = 40, ymax = 60,
               fill = "#00a000", alpha = 0.2) +
      geom_line(aes(y = relative_humidity_2m, color = color)) +
      geom_line(data = dummy_df, aes(x = x, y = y, color = "Optimal<br>Range"), size=0.5, alpha=0.7) +
      geom_point(aes(y = relative_humidity_2m, color = color,
                     text = paste0(format(time, "%B %d, %Y %I:%M %p"),
                                   "<br>Humidity: ", round(relative_humidity_2m, 1), "%")), size=1) +
      scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 day") +
      labs(
        title = paste("Recent Relative Humidity (RH) for", input$selected_county),
        x = "Time", y = "Relative Humidity (%)",
        color = ""
      ) +
      scale_color_manual(values = c("Forecast" = "steelblue2", "Historical" = "hotpink", "Optimal<br>Range" = "#00a000")) +
      theme_minimal()
    
    
    plotly::ggplotly(p_humid, tooltip = "text")
  })
  
  output$thi_line_plot <- renderPlotly({
    
    thi_data <- weather_data_hour %>%
      filter(county == input$selected_county)
    thi_data$color <- ifelse(thi_data$time > Sys.time(), "Forecast", "Historical")
    dummy_df <- data.frame(
      x = c(Sys.time(), Sys.time()),
      y = c(72, 72)
    )
    
    p_thi <- ggplot(thi_data, aes(x = time)) +
      annotate("segment",
               x = min(thi_data$time), xend = max(thi_data$time),
               y = 72, yend = 72,
               color = "#00a000",
               alpha = 0.5,
               linewidth = 0.5)+
      annotate("segment",
               x = min(thi_data$time), xend = max(thi_data$time),
               y = 68, yend = 68,
               color = "#00a000",
               alpha = 0.5,
               linewidth = 0.5) +
      annotate("rect",
               xmin = min(thi_data$time), xmax = max(thi_data$time),
               ymin = 68, ymax = 72,
               fill = "#00a000", alpha = 0.2) +
      geom_line(aes(y = thi_mean_hour, color = color)) +
      geom_line(data = dummy_df, aes(x = x, y = y, color = "Optimal<br>Range"), size=0.5, alpha=0.7) +
      geom_point(aes(y = thi_mean_hour, color = color,
                     text = paste0(format(time, "%B %d, %Y %I:%M %p"),
                                   "<br>THI: ", round(thi_mean_hour, 1), "%")), size=1) +
      scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 day") +
      labs(
        title = paste("Recent Temperature-Humidity Index (THI) for", input$selected_county),
        x = "Time", y = "THI",
        color = ""
      ) +
      scale_color_manual(values = c("Forecast" = "steelblue2", "Historical" = "hotpink", "Optimal<br>Range" = "#00a000")) +
      theme_minimal()
    
    
    plotly::ggplotly(p_thi, tooltip = "text")
  })
  
  output$table_title <- renderUI({
    h4(paste("Recent Weather Table for", input$selected_county))
  })
  
  output$data_table <- renderDT({
    selected_data <- weather_data_hour %>%
      filter(county == input$selected_county) %>%
      select(time, temperature_2m, relative_humidity_2m, thi_mean_hour) %>%
      mutate(
        formatted_time = format(time, "%I:%M%p")
      ) %>%
      mutate(
        formatted_date = format(time, "%m/%d/%y")
      )
    datatable(
      selected_data %>% select(formatted_date, formatted_time, temperature_2m, relative_humidity_2m, thi_mean_hour),
      colnames = c(
        "Date" = "formatted_date",
        "Time" = "formatted_time",
        "Temperature (°F)" = "temperature_2m",
        "Relative Humidity (%)" = "relative_humidity_2m",
        "THI" = "thi_mean_hour"
      ),
      options = list(pageLength = 24, ordering=FALSE)
    ) %>%
      formatRound(columns = c("Temperature (°F)", "Relative Humidity (%)", "THI"), digits = 1)
  })
}

shinyApp(ui, server)
