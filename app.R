library(shiny)
library(tigris)
library(sf)
library(leaflet)
library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(scales)
library(DT)
library(httr)
library(jsonlite)
library(devtools)
library(blsAPI)

# --- Load spatial data and prep ---

# Load counties and transform CRS
va_counties_gw <- counties(state = "VA", cb = TRUE, class = "sf") %>%
  st_transform(4326)
va_counties <- counties(state = "VA", cb = TRUE, class = "sf")
va_counties$NAME <- toupper(va_counties$NAME)
va_counties <- st_transform(va_counties, 4326)

# Define target counties uppercase
target_counties <- toupper(c("Shenandoah", "Warren", "Augusta", "Rockingham", 
                             "Page", "Frederick", "Clarke", "Rockbridge"))

# Filter to target counties
target_va_counties <- va_counties %>% filter(NAME %in% target_counties)
target_va_counties <- st_transform(target_va_counties, 4326)

# Load roads shapefile and transform
roads_path <- "C:/Users/irmo2303/Downloads/DSPG_foler/tl_2023_51_prisecroads-2/tl_2023_51_prisecroads.shp"
roads <- sf::st_read(roads_path, quiet = TRUE)
roads <- st_transform(roads, 4326)

# Filter primary and secondary roads
primary_secondary_roads <- roads %>% filter(RTTYP %in% c("P", "S"))

# Spatial join roads with counties
roads_with_county <- st_join(primary_secondary_roads, target_va_counties["NAME"], left = FALSE)

# Calculate road segment lengths (meters)
roads_with_county$length_m <- as.numeric(st_length(roads_with_county))

# Summarize total road length per county (km)
accessibility_scores <- roads_with_county %>%
  group_by(NAME) %>%
  summarise(total_road_length_km = sum(length_m, na.rm = TRUE) / 1000)

# Rescale length to 0-100 accessibility score
accessibility_scores$score <- scales::rescale(accessibility_scores$total_road_length_km, to = c(0, 100))

# Drop geometry for join
accessibility_scores_df <- accessibility_scores %>% st_set_geometry(NULL)

# Merge scores with target counties for mapping
merged <- target_va_counties %>%
  left_join(accessibility_scores_df, by = "NAME")

# Dummy suitability scores for dairy plant mapping (example)
index_data <- data.frame(
  NAME = target_counties,
  index = c(75, 60, 85, 90, 50, 65, 70, 80)
)

merged_suitability <- target_va_counties %>%
  left_join(index_data, by = "NAME")

# Transform merged suitability and accessibility after creation
merged_suitability <- st_transform(merged_suitability, 4326)
merged <- st_transform(merged, 4326)

# Load milk production data
milk_data <- read_excel("C:/Users/irmo2303/Downloads/DSPG_foler/milk_production.xlsx") %>%
  rename(`MILK (lbs)` = MILK)
milk_data$COUNTY <- toupper(milk_data$COUNTY)
milk_data$YEAR <- lubridate::year(milk_data$MONTH)
milk_data$MONTH <- lubridate::month(milk_data$MONTH, label = TRUE, abbr = FALSE)
milk_data$MONTH <- factor(milk_data$MONTH, levels = month.name, ordered = TRUE)

# Define color palettes
domain_vals <- merged_suitability$index
if (all(is.na(domain_vals)) || length(domain_vals) == 0) {
  domain_vals <- c(0, 100)
}
pal_suit <- colorNumeric(palette = "YlOrRd", domain = domain_vals, na.color = "#f0f0f0")
pal_access <- colorNumeric(palette = "Blues", domain = merged$score, na.color = "#f0f0f0")

# --- UI ---
ui <- navbarPage("VA Data Dashboard",
                 #tab1 - map
                 tabPanel("Map View",
                          leafletOutput("map", height = "700px"),
                          absolutePanel(
                            top = 80, left = 20, width = 300, draggable = TRUE,
                            style = "background-color: rgba(255,255,255,0.9);
               padding: 10px; border-radius: 10px; box-shadow: 2px 2px 6px rgba(0,0,0,0.2);",
                            tags$div(
                              style = "display: flex; align-items: center;",
                              tags$h4("Top 5 Counties by Dairy Plant Suitability Score", style = "margin: 0;"),
                              actionLink("show_info", label = NULL, icon = icon("info-circle"), style = "color: #31708f; margin-left: 8px;")
                            ),
                            tableOutput("top5_table")
                          )
                 ),
                 
                 #tab 2- line graph
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
                 ),
                 #tab 3- milk prod table
                 tabPanel("Milk Production Table",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("milk_county", "County:", choices = sort(target_counties))
                            ),
                            mainPanel(
                              htmlOutput("milk_avg"),
                              DT::dataTableOutput("milk_table")
                            )
                          )
                 ),
                 #tab 4 - road view
                 tabPanel("Roads & Accessibility",
                          sidebarLayout(
                            sidebarPanel(
                              helpText("Primary & Secondary Roads and Accessibility Scores by County")
                            ),
                            mainPanel(
                              leafletOutput("roads_map", height = "500px"),
                              plotOutput("accessibility_bar", height = "300px")
                            )
                          )
                 ),
                 # tab 5- labor graph
                 tabPanel("Labor Availability",
                          leafletOutput("labor_map", height = "700px")
                 ),
                 
                 #tab 6- groundwater graph
                 tabPanel("Groundwater Levels",
                          leafletOutput("groundwater_map", height = "700px")
                 )
)

# --- Server ---
server <- function(input, output, session) {
  #Part 1- labor API setup
  labor_counties <- counties(state = "VA", cb = TRUE, class = "sf")
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
                   'LAUCN518200000000003','LAUCN518300000000003','LAUCN518400000000003')
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
  
  # Part 2- groundwater API
  levels <- readNWISdata(stateCd="Virginia",
                         service = "gwlevels",
                         startDate = "2025-03-01",
                         endDate="")
  latest_vals <- levels %>%
    mutate(
      lev_va = as.numeric(lev_va),
      lev_dt = as.Date(lev_dt)
    ) %>%
    drop_na(lev_va) %>%
    group_by(site_no) %>%
    arrange(desc(lev_dt)) %>%
    slice(1) %>%
    ungroup()
  
  site_meta <- readNWISsite(unique(latest_vals$site_no)) %>%
    filter(!is.na(dec_long_va), !is.na(dec_lat_va))
  site_sf <- st_as_sf(site_meta, coords = c("dec_long_va", "dec_lat_va"), crs = 4326)
  site_with_levels <- site_sf %>%
    left_join(latest_vals, by = "site_no")
  site_with_county <- st_join(site_with_levels, va_counties_gw, join = st_within)
  county_avg <- site_with_county %>%
    st_drop_geometry() %>%
    group_by(GEOID) %>%
    summarise(avg_level = mean(lev_va, na.rm = TRUE), .groups = "drop")
  va_map_data2 <- va_counties_gw %>%
    left_join(county_avg, by = "GEOID")
  
  # Dairy Plant Suitability Map
  output$map <- renderLeaflet({
    leaflet(merged_suitability) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal_suit(index),
        color = "blue",
        weight = 1,
        fillOpacity = 0.7,
        label = ~lapply(paste0(
          "<strong>", NAME, "</strong><br>",
          "Suitability Score: ", index
        ), htmltools::HTML),
        labelOptions = labelOptions(
          direction = "auto",
          style = list("font-weight" = "normal"),
          textsize = "14px"
        )
      ) %>%
      addLegend(pal = pal_suit, values = merged_suitability$index, title = "Dairy Plant Suitability Score")
  })
  
  output$top5_table <- renderTable({
    merged_suitability %>%
      st_drop_geometry() %>%
      arrange(desc(index)) %>%
      select(NAME, index) %>%
      slice_head(n = 5)
  })
  
  # Milk Production Line Graph
  output$county_selector <- renderUI({
    selectInput("selected_counties", "Counties:",
                choices = sort(unique(milk_data$COUNTY[milk_data$COUNTY %in% target_counties])),
                selected = head(sort(unique(milk_data$COUNTY[milk_data$COUNTY %in% target_counties])), 3),
                multiple = TRUE
    )
  })
  
  output$line_plot <- renderPlot({
    req(input$selected_counties)
    filtered_data <- milk_data %>%
      filter(COUNTY %in% input$selected_counties)
    
    filtered_data$MONTH <- factor(filtered_data$MONTH, levels = month.name, ordered = TRUE)
    
    ggplot(filtered_data, aes(x = MONTH, y = `MILK (lbs)`, color = COUNTY, group = COUNTY)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      scale_x_discrete(expand = c(0.01, 0)) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Raw Milk Production by County by Month (2024)", y = "Milk Output (lbs)", x = "Month") +
      theme(plot.title = element_text(size = 16, face = "bold"))
  })
  
  # Milk Production Table and Average
  output$milk_table <- DT::renderDataTable({
    req(input$milk_county)
    milk_data %>%
      filter(COUNTY == input$milk_county)
  })
  
  output$milk_avg <- renderUI({
    req(input$milk_county)
    avg_all_months <- milk_data %>%
      filter(COUNTY == input$milk_county) %>%
      group_by(YEAR) %>%
      summarise(`Average MILK (lbs)` = mean(`MILK (lbs)`, na.rm = TRUE), .groups = 'drop')
    
    avg_text <- paste(
      paste0("<b>", input$milk_county, "</b><br>",
             paste0("Year ", avg_all_months$YEAR, ": ", scales::comma(avg_all_months$`Average MILK (lbs)`), " lbs")
      ),
      collapse = "<br>"
    )
    
    HTML(paste0("<h3 style='margin-top:0;'>Average Monthly Milk Output by Year</h3>", avg_text))
  })
  
  # Roads & Accessibility Map
  output$roads_map <- renderLeaflet({
    leaflet(merged) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal_access(score),
        color = "black",
        weight = 1,
        fillOpacity = 0.7,
        label = ~lapply(paste0(
          "<strong>", NAME, "</strong><br>",
          "Accessibility Score: ", round(score, 1)
        ), htmltools::HTML),
        labelOptions = labelOptions(direction = "auto", style = list("font-weight" = "normal"), textsize = "14px")
      ) %>%
      addPolylines(data = primary_secondary_roads, color = "blue", weight = 2, opacity = 0.6) %>%
      addLegend(pal = pal_access, values = merged$score, title = "Accessibility Score")
  })
  
  # Accessibility Bar Chart
  output$accessibility_bar <- renderPlot({
    ggplot(accessibility_scores, aes(x = reorder(NAME, score), y = score, fill = score)) +
      geom_col() +
      coord_flip() +
      scale_fill_gradient(low = "lightblue", high = "blue") +
      labs(title = "Accessibility Scores by County", x = "County", y = "Score") +
      theme_minimal()
  })
  
  observeEvent(input$show_info, {
    showModal(modalDialog(
      title = "About the Score",
      "The score ranks counties based on milk production, labor availability, and transportation access, helping identify the best locations for a dairy plant.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  # Output 3: Labor data setup
  tidy_data <- series_df %>%
    tidyr::unnest(cols = c(data))
  latest_vals <- tidy_data %>%
    mutate(value = as.numeric(value)) %>%
    group_by(seriesID) %>%
    arrange(desc(year), desc(periodName)) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(fips = substr(seriesID, 6, 10))
  va_map_data <- va_counties_gw %>%
    left_join(latest_vals, by = c("GEOID" = "fips"))
  pal2 <- colorNumeric("YlOrRd", domain = va_map_data$value, na.color="transparent")
  
  #Output 3: labor data map
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
          color = "#777",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = ~paste(NAME, ": ", value, "%")
      ) %>%
      addLegend(pal = pal2, values = ~value, opacity = 0.7,
                title = "Unemployment <br> Rate",
                position = "topleft")
  })
  
  # Output 4: groundwater map
  output$groundwater_map <- renderLeaflet({
    pal3 <- colorNumeric("Blues", domain = va_map_data2$avg_level, na.color="transparent")
    
    leaflet(va_map_data2) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~ifelse(is.na(avg_level), "white", pal3(avg_level)),
        fillOpacity = 0.8,
        color = "#999999",
        weight = 1,
        label = ~paste0(NAME, ": ", ifelse(is.na(avg_level), "None", paste0(round(avg_level, 2), " ft"))) 
      ) %>%
      addLegend(pal = pal3, values = ~avg_level,
                title = "Average <br> Groundwater <br> Level (ft)")
  })
  
}

# Run the app
shinyApp(ui, server)
