library(shiny)
library(tigris)
library(sf)
library(leaflet)
library(dplyr)
library(ggplot2)
library(readxl)

# get va counties shapefile
va_counties <- counties(state = "VA", cb = TRUE, class = "sf")

# ui
ui <- navbarPage("VA Data Dashboard", #test2
                 
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
                 tabPanel("Line Graph View",
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
                 )
)

server <- function(input, output, session) {
  counties <- counties(state = "VA", cb = TRUE, class = "sf")
  
  index_data <- data.frame(
    NAME = c("Shenandoah", "Fairfax", "Albemarle", "Loudoun", "Richmond", 
             "Augusta"),
    index = c(75, 50, 90, 88, 40, 81)
  )
  
  merged <- merge(counties, index_data, by = "NAME", all.x = TRUE)
  pal <- colorNumeric(palette = "YlOrRd", domain = merged$index, na.color = 
                        "#f0f0f0")
  
  # leaflet map
  output$map <- renderLeaflet({
    leaflet(merged) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(index),
        color = "blue",
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
}

# run  app
shinyApp(ui, server)
