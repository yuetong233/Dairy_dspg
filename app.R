library(shiny)
library(tigris)
library(sf)
library(leaflet)
library(dplyr)
library(ggplot2)
library(readxl)
library(httr)
library(jsonlite)
library(devtools)
library(blsAPI)

# get va counties shapefile
va_counties <- counties(state = "VA", cb = TRUE, class = "sf")
# Prepare the payload with BLS series IDs and API key

# ui
ui <- navbarPage("VA Data Dashboard", 
                 
                 # tab 1- map
                 tabPanel("Map View",
                          leafletOutput("map", height = "700px"),
                          absolutePanel(
                            top = 80, left = 20, width = 200, draggable = TRUE,
                            style = "background-color: rgba(255,255,255,0.9); 
                              padding: 10px; border-radius: 10px; box-shadow: 
                              2px 2px 6px rgba(0,0,0,0.2);",
                            h4("Top 5 Counties by Score"),
                            tableOutput("top5_table")
                          )
                 ),
                 
                 # tab 2- line graph
                 tabPanel("Raw Milk Production",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Select Counties: "),
                              uiOutput("county_selector"),
                              helpText("Shows milk production trends by county")
                            ),
                            mainPanel(
                              plotOutput("line_plot", height = "600px")
                            )
                          )
                 ),
                 
                 # tab 3- labor graph
                 tabPanel("Labor Availability",
                          leafletOutput("labor_map", height = "700px")
                 )
)

server <- function(input, output, session) {
  counties <- counties(state = "VA", cb = TRUE, class = "sf")
  
  series_ids_county1 <- c('LAUCN510010000000003','LAUCN510030000000003','LAUCN510050000000003',
                          'LAUCN510070000000003','LAUCN510090000000003','LAUCN510110000000003',
                          'LAUCN510130000000003','LAUCN510150000000003','LAUCN510170000000003',
                          'LAUCN510190000000003','LAUCN510210000000003','LAUCN510230000000003',
                          'LAUCN510250000000003','LAUCN510270000000003','LAUCN510290000000003',
                          'LAUCN510310000000003','LAUCN510330000000003','LAUCN510350000000003',
                          'LAUCN510360000000003','LAUCN510370000000003','LAUCN510410000000003',
                          'LAUCN510430000000003','LAUCN510450000000003','LAUCN510470000000003',
                          'LAUCN510490000000003','LAUCN510510000000003','LAUCN510530000000003',
                          'LAUCN510570000000003','LAUCN510590000000003','LAUCN510610000000003',
                          'LAUCN510630000000003','LAUCN510650000000003','LAUCN510670000000003',
                          'LAUCN510690000000003','LAUCN510710000000003','LAUCN510730000000003',
                          'LAUCN510750000000003','LAUCN510770000000003','LAUCN510790000000003',
                          'LAUCN510810000000003','LAUCN510830000000003','LAUCN510850000000003',
                          'LAUCN510870000000003','LAUCN510890000000003','LAUCN510910000000003',
                          'LAUCN510930000000003','LAUCN510950000000003','LAUCN510970000000003')
  
  series_ids_county2 <- c('LAUCN510990000000003','LAUCN511010000000003','LAUCN511030000000003',
                          'LAUCN511050000000003','LAUCN511070000000003','LAUCN511090000000003',
                          'LAUCN511110000000003','LAUCN511130000000003','LAUCN511150000000003',
                          'LAUCN511170000000003','LAUCN511190000000003','LAUCN511210000000003',
                          'LAUCN511250000000003','LAUCN511270000000003','LAUCN511310000000003',
                          'LAUCN511330000000003','LAUCN511350000000003','LAUCN511370000000003',
                          'LAUCN511390000000003','LAUCN511410000000003','LAUCN511430000000003',
                          'LAUCN511450000000003','LAUCN511470000000003','LAUCN511490000000003',
                          'LAUCN511530000000003','LAUCN511550000000003','LAUCN511570000000003',
                          'LAUCN511590000000003','LAUCN511610000000003','LAUCN511630000000003',
                          'LAUCN511650000000003','LAUCN511670000000003','LAUCN511690000000003',
                          'LAUCN511710000000003','LAUCN511730000000003','LAUCN511750000000003',
                          'LAUCN511770000000003','LAUCN511790000000003','LAUCN511810000000003',
                          'LAUCN511830000000003','LAUCN511850000000003','LAUCN511870000000003',
                          'LAUCN511910000000003','LAUCN511930000000003','LAUCN511950000000003',
                          'LAUCN511970000000003','LAUCN511990000000003')
  
  series_cont <- c('LAUCN515100000000003','LAUCN515200000000003','LAUCN515300000000003',
                   'LAUCN515400000000003','LAUCN515500000000003','LAUCN515700000000003',
                   'LAUCN515900000000003','LAUCN516000000000003','LAUCN516100000000003',
                   'LAUCN516200000000003','LAUCN516300000000003','LAUCN516400000000003',
                   'LAUCN516500000000003','LAUCN516600000000003','LAUCN516700000000003',
                   'LAUCN516780000000003','LAUCN516800000000003','LAUCN516830000000003',
                   'LAUCN516850000000003','LAUCN516900000000003','LAUCN517000000000003',
                   'LAUCN517100000000003','LAUCN517200000000003','LAUCN517300000000003',
                   'LAUCN517350000000003','LAUCN517400000000003','LAUCN517500000000003',
                   'LAUCN517600000000003','LAUCN517700000000003','LAUCN517750000000003',
                   'LAUCN517900000000003','LAUCN518000000000003','LAUCN518100000000003',
                   'LAUCN518200000000003','LAUCN518300000000003','LAUCN518400000000003'
  )
  
  payload <- list('seriesid' = series_ids_county1, 'registrationKey' = "c107ff6e48f24ff8b78d2d32b4e87946")
  response <- blsAPI(payload, 2)
  data <- fromJSON(response)
  payload2 <- list('seriesid' = series_ids_county2, 'registrationKey' = "c107ff6e48f24ff8b78d2d32b4e87946")
  response2 <- blsAPI(payload2, 2)
  data2 <- fromJSON(response2)
  payload3 <- list('seriesid' = series_cont, 'registrationKey' = "c107ff6e48f24ff8b78d2d32b4e87946")
  response3 <- blsAPI(payload3, 2)
  data3 <- fromJSON(response3)
  series_df <- rbind(data$Results$series, data2$Results$series, data3$Results$series)
  
  index_data <- data.frame(
    NAME = c("Shenandoah", "Fairfax", "Albemarle", "Loudoun", "Richmond", 
             "Augusta"),
    index = c(75, 50, 90, 88, 40, 81)
  )
  
  merged <- merge(counties, index_data, by = "NAME", all.x = TRUE)
  pal <- colorNumeric(palette = "YlOrRd", domain = merged$index, na.color = 
                        "#f0f0f0")
  
  # index map
  output$map <- renderLeaflet({
    leaflet(merged) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(index),
        color = "#111111",
        weight = 1,
        fillOpacity = 0.7,
        label = ~lapply(paste0(
          "<strong>", NAME, "</strong><br>",
          "Index: ", index
        ), htmltools::HTML),
        labelOptions = labelOptions(
          direction = "auto",
          style = list("font-weight" = "normal"),
          textsize = "14px"
        )
      ) %>%
      addLegend(pal = pal, values = merged$index, title = "Score")
  })
  
  # top 5 table
  output$top5_table <- renderTable({
    merged %>%
      st_drop_geometry() %>%
      arrange(desc(index)) %>%
      select(NAME, index) %>%
      slice_head(n = 5)
  })
  
  milk_data <- read_excel("milk_production.xlsx")
  # select counties
  output$county_selector <- renderUI({
    selectInput("selected_counties", "Counties:",
                choices = unique(milk_data$COUNTY),
                selected = unique(milk_data$COUNTY)[1:3],
                multiple = TRUE)
  })
  
  # line graph by county
  output$line_plot <- renderPlot({
    req(input$selected_counties)  # ensure selcet
    
    filtered_data <- milk_data %>%
      filter(COUNTY %in% input$selected_counties)
    
    ggplot(filtered_data, aes(x = MONTH, y = MILK, color = COUNTY)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      theme_minimal() +
      labs(title = "Raw Milk Production by County by Month (2024)", y = 
             "Milk Output", x = "Month") +
      theme(plot.title = element_text(size = 16, face = "bold"))
  })
  #series_df <- data$Results$series
  
  # unnest the `data` list-column
  tidy_data <- series_df %>%
    tidyr::unnest(cols = c(data))
  
  latest_vals <- tidy_data %>%
    mutate(value = as.numeric(value)) %>%
    group_by(seriesID) %>%
    arrange(desc(year), desc(periodName)) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(fips = substr(seriesID, 6, 10))
  
  va_map_data <- va_counties %>%
    left_join(latest_vals, by = c("GEOID" = "fips"))
  
  pal2 <- colorNumeric("YlOrRd", domain = va_map_data$value)
  
  output$labor_map <- renderLeaflet({
    leaflet(va_map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal2(value),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = ~paste(NAME, ": ", value, "%")
      ) %>%
      addLegend(pal = pal2, values = ~value, opacity = 0.7,
                title = "Unemployment Rate",
                position = "topleft")
  })
}

# run  app
shinyApp(ui, server)
