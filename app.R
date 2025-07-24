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
library(rnassqs)
library(bslib)
library(ggimage)
library(tidyr)
library(purrr)
library(plotly)
library(stringr)
library(httr)
library(jsonlite)
library(blsAPI)
library(dataRetrieval)
library(taskscheduleR)
library(readr)

#by tab:
#1. map view
  # suitability scores
    # fix na for hover thing
    # changing weights doesn't change the graph?
  # economic preference map
    # there is no map..
  # compare counties
    # format na parts better
    # make absolute panel MAP ONLY

#3. cow productivity
  # cow productivity model
    # warren, page, frederick, clarke all not working
    # fix help text
    # create error message for missing data (specifically Clarke)
  # dairy cow inventory
    # fix x-axis
    # fix table and table title for readability

#4. roads and accessibility
  # change colors to look better
  # change map color from Blues

#5. transportation costs
  # make explore lots button go somewhere
  # allow city and county names with autofill
  # auto-calculate distances once city name is entered
  # fix column names in table


# Authenticate with NASS API
nassqs_auth(key = "6644F8BA-CCCE-3CEE-BCE7-5BA5E83CA7E8")
transport_df <- read_excel("data/miles_processed.xlsx")
interpolated_df <- read.csv("interpolated_optimal_counties.csv")


# Load VA counties spatial data
va_counties <- counties(state = "VA", cb = TRUE, class = "sf") %>%
  st_transform(4326)
va_counties$NAME <- toupper(va_counties$NAME)

# Define target counties (uppercase)
target_counties <- toupper(c("SHENANDOAH", "ROCKINGHAM", "AUGUSTA", "WARREN", 
                             "PAGE", "FREDERICK", "CLARKE", "ROCKBRIDGE",
                             "PITTSYLVANIA", "FRANKLIN"))

target_va_counties <- va_counties %>% filter(NAME %in% target_counties)

# Milk Prod data 2011-2025
milk_data <- read_excel("data/VA_Milk_Production.xlsx") %>%
  rename(
    `MILK (lbs)` = POUNDS_OF_MILK,
    PRODUCERS = NUMBER_OF_PRODUCERS
  ) %>%
  mutate(
    COUNTY = toupper(COUNTY),
    DATE = parse_date_time(paste(YEAR, MONTH, "1"), orders = "Y b d"),  # lowercase 'b' for abbreviated month
    YEAR = as.numeric(YEAR),
    MONTH_NAME = factor(month.name[month(DATE)], levels = month.name, ordered = TRUE),
    MONTH_NUM = month(DATE)
  )


# Load roads shapefile and process for accessibility scores
roads_path <- "tl_2023_51_prisecroads-2/tl_2023_51_prisecroads.shp"
roads <- sf::st_read(roads_path, quiet = TRUE)
primary_secondary_roads <- roads %>% filter(RTTYP %in% c("P", "S")) %>% st_transform(4326)
# Join road segments with counties
roads_with_county <- st_join(primary_secondary_roads, target_va_counties["NAME"], left = FALSE)
roads_with_county$length_m <- as.numeric(st_length(roads_with_county))

# Calculate total road length per county
road_lengths <- roads_with_county %>%
  group_by(NAME) %>%
  summarise(total_road_length_km = sum(length_m, na.rm = TRUE) / 1000)

# Calculate area (km²) of each county
target_va_counties$area_km2 <- as.numeric(st_area(target_va_counties) / 1e6)

# Combine road lengths with area
accessibility_scores <- road_lengths %>%
  left_join(st_drop_geometry(target_va_counties)[, c("NAME", "area_km2")], by = "NAME") %>%
  mutate(
    road_density = total_road_length_km / area_km2,
  ) %>% 
  arrange(desc(road_density)) %>% slice(-1) %>%
  mutate(
    score = rescale(road_density, to = c(0, 100))
  )

accessibility_scores_df <- accessibility_scores %>% st_set_geometry(NULL)
accessibility_scores_df <- accessibility_scores_df %>%
  group_by(NAME) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)), .groups = "drop")


merged_accessibility <- target_va_counties %>%
  left_join(accessibility_scores_df, by = "NAME")



# Update color palette for map

pal_access <- colorNumeric(palette = "Blues", domain = merged_accessibility$score, na.color = "#f0f0f0")

# Function to fetch dairy cows data for last 10 years from API
fetch_dairy_cows <- function(years) {
  milk_cows_all <- lapply(years, function(y) {
    tryCatch({
      df <- nassqs(list(
        sector_desc = "ANIMALS & PRODUCTS",
        group_desc = "LIVESTOCK",
        commodity_desc = "CATTLE",
        class_desc = "COWS, MILK",
        statisticcat_desc = "INVENTORY",
        unit_desc = "HEAD",
        agg_level_desc = "COUNTY",
        state_alpha = "VA",
        source_desc = "SURVEY",  
        year = as.character(y)
      ))
      if (!is.null(df) && is.data.frame(df)) {
        df <- df %>% mutate(TYPE = "Milk Cows")
      }
      df
    }, error = function(e) {
      message(paste("Milk cows API call failed for year", y, ":", e$message))
      NULL
    })
  })
  
  milk_df <- do.call(rbind, Filter(Negate(is.null), milk_cows_all))
  milk_df
}
collapsiblePanel <- function(title, content) {
  tags$details(
    tags$summary(tags$b(title)),
    tags$div(style = "margin-top: 10px; margin-bottom: 15px;", content)
  )
}

# THI API setup
va_counties_thi <- target_va_counties
va_counties_thi$NAME <- str_to_title(target_va_counties$NAME)
va_counties_thi$centroid <- st_centroid(va_counties_thi$geometry)
coords <- st_coordinates(va_counties_thi$centroid)
va_counties_thi$lat <- coords[, 2]
va_counties_thi$lon <- coords[, 1]
full_data <- list()
for (i in 1:nrow(va_counties_thi)) {
  lat <- va_counties_thi$lat[i]
  lon <- va_counties_thi$lon[i]
  name <- va_counties_thi$NAME[i]
  
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
for (i in 1:nrow(va_counties_thi)) {
  lat <- va_counties_thi$lat[i]
  lon <- va_counties_thi$lon[i]
  name <- va_counties_thi$NAME[i]
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
if (!"time" %in% colnames(weather_data_hour) || !inherits(weather_data_hour$time, "POSIXct")) {
  stop("The 'time' column is missing or not properly formatted.")
}
weather_data <- bind_rows(full_data)
weather_data$time <- as.Date(weather_data$time)
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






my_theme <- bs_theme(
  bg = "#fff8f0",
  fg = "#5a3e1b",
  primary = "#8bc34a",
  base_font = font_google("Patrick Hand")
)
ui <- fluidPage(
  theme = my_theme,
  navbarPage(title = "VA Data Dashboard",
             
             tags$head(
               tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
               tags$style(HTML("      
        body {
          background-color: #fff8f0;
          font-family: 'Patrick Hand', cursive;
        }
        .navbar, .panel {
          border-radius: 15px;
          box-shadow: 2px 2px 10px rgba(90, 62, 27, 0.4);
        }
        .action-button, .btn {
          background-color: #8bc34a !important;
          border-color: #5a3e1b !important;
          color: #fff !important;
          font-weight: bold;
        }
        .leaflet-container {
          border-radius: 12px;
          box-shadow: 0 0 15px rgba(90, 62, 27, 0.3);
        }
        details summary {
          cursor: pointer;
          padding: 6px;
          font-size: 16px;
        }
        details[open] summary {
          font-weight: bold;
          color: #5a3e1b;
        }
      "))
             ),
             tabPanel("🧡🦃WELCOME🥛🗺 ",
                      tabsetPanel(id = "welcome_tabs", selected = "Project Overview",
                                  
                                  tabPanel("Project Overview",
                                           fluidRow(
                                             column(12,
                                                    tags$div(
                                                      style = "padding: 30px; background-color: #fff3e6; border-radius: 15px; box-shadow: 0px 0px 12px rgba(0,0,0,0.1);",
                                                      tags$h1("Welcome to the Virginia Dairy Plant Optimization Dashboard! 🐄"),
                                                      tags$p("This dashboard is a one-stop tool to explore and analyze the suitability of different counties in Virginia for dairy plant placement."),
                                                      tags$h4("📌 What's inside:"),
                                                      tags$ul(
                                                        tags$li("Milk production and cow productivity analysis"),
                                                        tags$li("Transportation and road accessibility modeling"),
                                                        tags$li("Data-driven recommendations for dairy plant siting"),
                                                        tags$li("User-adjustable preferences for economic vs. farm-based priorities")
                                                      ),
                                                      tags$p("Built with love by two DSPG students who drank way too much chocolate milk while designing this."),
                                                      tags$hr(),
                                                      tags$blockquote("“Good data leads to good decisions. Great dashboards make them accessible.”")
                                                    )
                                             )
                                           )
                                  ),
                                  
                                  tabPanel("Data Sources",
                                           fluidRow(
                                             column(12,
                                                    tags$h3("📊 Variables, Sources & Formats"),
                                                    DT::dataTableOutput("data_sources_table")
                                             )
                                           )
                                  ),
                                  
                                  tabPanel("About Us",
                                           fluidRow(
                                             column(6,
                                                    tags$div(
                                                      style = "padding: 20px; background-color: #fdf3e7; border-radius: 12px;",
                                                      tags$h3("👩 Irmak Ocel"),
                                                      tags$p("A Statistics & Public Health major from Virginia Tech. Passionate about making data make sense, especially in the public health & agriculture space."),
                                                      tags$p("Former intern at Amazon, dashboard whisperer, and milk efficiency queen 🐄📈")
                                                    )
                                             ),
                                             column(6,
                                                    tags$div(
                                                      style = "padding: 20px; background-color: #fdf3e7; border-radius: 12px;",
                                                      tags$h3("🧑‍💻 [Your Partner's Name]"),
                                                      tags$p("A [Major] student with a love for geospatial analysis, clean maps, and transportation modeling."),
                                                      tags$p("Known for: driving 3,000 virtual miles across Virginia to model cost paths.")
                                                    )
                                             )
                                           )
                                  )
                                  
                      )
             ),
             
             
             
             tabPanel("Map View 🗺️",
                      tabsetPanel(
                        tabPanel("Suitability Score Map",
                                 leafletOutput("suitability_map", height = "700px"),
                                 br(),
                                 br()
                        ),
                        tabPanel("Economic Preference Map",
                                 uiOutput("optimal_county_text"),
                                 leafletOutput("interpolation_map", height = "700px"),
                                 br()
                        ),
                        tabPanel("Compare Counties",
                                 tableOutput("county_comparison_table")
                        )
                        
                      ),
                      absolutePanel(
                        top = 425, left = 600, width = 250, draggable = TRUE,
                        style = "background-color: rgba(255,255,255,0.9); padding: 10px; border-radius: 10px; box-shadow: 2px 2px 6px rgba(0,0,0,0.2);",
                        tags$h3("🏁 Choose Your Priorities", style = "color: #5a3e1b; font-weight: bold; margin-top: 0;"),
                        
                        sliderInput("weight_slider", "Weight Preferences", min = 0, max = 100, value = 50, step = 1),
                        helpText(HTML("Use the slider to balance the priorities of farmers vs. investors.<br>0 = entirely investor-focused.<br>100 = entirely farmer-focused.")),
                        
                        tags$hr(),
                        
                        tags$div(
                          style = "display: flex; align-items: center; justify-content: space-between;",
                          tags$h4("Top 5 Counties by Suitability 🐮", style = "margin: 0;"),
                          actionLink("show_info", label = NULL, icon = icon("info-circle"), style = "color: #5a3e1b; margin-left: 8px;")
                        ),
                        tableOutput("top5_table")
                      )
             ),
             
             tabPanel("Cow Productivity 🐄📈",
                      tabsetPanel(
                        
                        tabPanel("Cow Productivity Model",
                                 fluidRow(
                                   column(3,
                                          selectInput("efficiency_county_single", "Select County:",
                                                      choices = str_to_title(target_counties),
                                                      selected = str_to_title(target_counties[1]))
                                   ),
                                   column(9,
                                          br(),
                                          plotlyOutput("efficiency_line_plot", height = "450px")
                                   )
                                 ),
                                 hr(),
                                 tags$h3("Milk Productivity Comparison Table"),
                                 tableOutput("monthly_efficiency_table"),
                                 hr(),
                                 tableOutput("efficiency_summary_table")
                        ),
                        
                        tabPanel("Milk Overview",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("selected_county", "Select County:", 
                                                 choices = str_to_title(target_counties), 
                                                 selected = str_to_title(target_counties[1])),
                                     selectInput("selected_year", "Choose a Year:", choices = 2016:2025, selected = 2024)
                                   ),
                                   mainPanel(
                                     br(),
                                     plotOutput("line_plot", height = "400px"),
                                     br(),
                                     htmlOutput("milk_avg"),
                                     DT::dataTableOutput("milk_table"),
                                     hr()
                                   )
                                 )
                        ),
                        
                        tabPanel("Dairy Cow Inventory",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("cow_county", "Select County:", 
                                                 choices = str_to_title(target_counties), 
                                                 selected = str_to_title(target_counties[1])),
                                     helpText("Dairy cow type can differ as organic and non-organic.")
                                   ),
                                   mainPanel(
                                     br(),
                                     plotOutput("cow_inventory_plot", height = "400px"),
                                     br(),
                                     DT::dataTableOutput("cow_inventory_table"),
                                     br()
                                   )
                                 )
                        )
                      ) 
             ),
             
             
             
             
             tabPanel("Roads & Accessibility 🚜",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Primary & Secondary Roads and Accessibility Scores by County")
                        ),
                        mainPanel(
                          leafletOutput("roads_map", height = "500px"),
                          br(),
                          plotOutput("accessibility_bar", height = "300px"),
                          hr(),
                          h3("Cost per Gallon by County and Destination"),
                          plotOutput("cost_per_gallon_tile", height = "400px"),
                          hr(),
                          h3("Gallons Needed per Trip Bubble Plot"),
                          plotOutput("gallons_needed_bubble", height = "400px"),
                          br()
                        )
                      )
             ),
             
             tabPanel("Transportation Costs 🚚",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Calculate Your Transportation Costs"),
                          textInput("user_city", "Enter your city or lot name:"),
                          numericInput("miles_arlington", "Miles to Arlington:", value = NA),
                          numericInput("miles_richmond", "Miles to Richmond:", value = NA),
                          numericInput("miles_vabeach", "Miles to VA Beach:", value = NA),
                          numericInput("mpg", "Truck MPG:", value = 5.5),
                          numericInput("fuel_price", "Fuel Price ($/gallon):", value = 4.5),
                          numericInput("truck_capacity", "Truck Capacity (gallons):", value = 6000),
                          numericInput("daily_production", "Daily Production (gallons):", value = 10000),
                          numericInput("operating_days", "Operating Days per Year:", value = 340),
                          actionButton("calc_btn", "💡 Calculate My Transportation Costs!")
                        ),
                        mainPanel(
                          h4("Transportation Cost Metrics"),
                          tableOutput("transport_metrics"),
                          hr(),
                          h4("Annual Transportation Cost Comparison"),
                          plotOutput("annual_cost_bar"),
                          hr(),
                          h3("Not sure but in the market? Take a look at available lots!"),
                          tableOutput("available_lots_table"),
                          actionButton("vedp_button", "Explore More Lots on VEDP", 
                                       onclick = "window.open('https://sites.vedp.org/', '_blank')", 
                                       class = "btn-primary"),
                          hr()
                        )
                      )
             ),
             tabPanel("Weather Statistics",
                      tabsetPanel(
                        tabPanel("Map",
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
                              
                              selectInput("selected_county_thi",
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
                        ),
                      )
             )
)


# Server logic for VA Dairy Dashboard
server <- function(input, output, session) {
  print("Server started")
  
  
  # Reactive years for fetching
  years_to_fetch <- reactive({
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    seq(current_year - 9, current_year)
  })
  
  # Suggested county from slider interpolation
  selected_county_df <- reactive({
    req(input$weight_slider)
    county_row <- interpolated_df %>%
      filter(weight == input$weight_slider, !is.na(county))
    
    if (nrow(county_row) == 0) return(NULL)
    county_row
  })
  
  # Fetch raw dairy cow inventory
  dairy_cows_data <- reactive({
    fetch_dairy_cows(years_to_fetch())
  })
  print(head(transport_df))
  
  # Cleaned VA dairy cow inventory
  dairy_cows_va <- reactive({
    df <- dairy_cows_data()
    req(df)
    df %>%
      filter(toupper(county_name) %in% target_counties) %>%
      mutate(
        COUNTY = toupper(county_name),
        YEAR = as.numeric(year),
        COWS = as.numeric(Value)
      ) %>%
      group_by(COUNTY, YEAR) %>%
      summarise(COWS = sum(COWS, na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(COWS))
  })
  
  
  monthly_efficiency <- reactive({
    req(dairy_cows_va())
    
    milk_data %>%
      filter(COUNTY %in% target_counties) %>%
      left_join(dairy_cows_va(), by = c("COUNTY", "YEAR")) %>%
      filter(!is.na(COWS), COWS > 0, !is.na(`MILK (lbs)`)) %>%
      mutate(
        efficiency_lbs_per_cow = `MILK (lbs)` / COWS,
        MONTH_NAME = month.name[month(DATE)],
        MONTH_NAME = factor(MONTH_NAME, levels = month.name, ordered = TRUE)
      )
  })
  
  
  
  # Monthly efficiency summary reactive
  monthly_efficiency_summary <- reactive({
    df <- monthly_efficiency()
    req(df)
    
    df %>%
      group_by(COUNTY) %>%
      summarise(
        avg_efficiency = mean(efficiency_lbs_per_cow, na.rm = TRUE),
        best_month_eff = ifelse(n() == 0, NA, max(efficiency_lbs_per_cow, na.rm = TRUE)),
        best_month = ifelse(n() == 0, NA, MONTH_NAME[which.max(efficiency_lbs_per_cow)]),
        .groups = "drop"
      )
  })
  
  
  
  
  # Join milk + cow inventory for yearly composite calcs
  combined_data <- reactive({
    req(dairy_cows_va())
    full_join(
      milk_data %>% rename(MILK_LBS = `MILK (lbs)`),
      dairy_cows_va(),
      by = c("COUNTY", "YEAR")
    ) %>%
      filter(COUNTY %in% target_counties)
  })
  
  combined_data_flagged <- reactive({
    # Summarize milk data into yearly totals
    yearly_milk <- milk_data %>%
      group_by(COUNTY, YEAR) %>%
      summarise(MILK_LBS = sum(`MILK (lbs)`, na.rm = TRUE), .groups = "drop") %>%
      mutate(has_milk = !is.na(MILK_LBS) & MILK_LBS > 0)
    
    # Clean cow inventory
    yearly_cows <- dairy_cows_va() %>%
      mutate(has_cows = !is.na(COWS) & COWS > 0)
    
    # Full join both
    full_join(yearly_milk, yearly_cows, by = c("COUNTY", "YEAR")) %>%
      mutate(data_flag = case_when(
        (is.na(has_milk) | !has_milk) & (is.na(has_cows) | !has_cows) ~ "❌ Missing Both Milk & Cow Data",
        (is.na(has_milk) | !has_milk) ~ "⚠️ Missing Milk Data",
        (is.na(has_cows) | !has_cows) ~ "⚠️ Missing Cow Inventory",
        COWS == 0 ~ "⚠️ Zero Cows Reported",
        TRUE ~ "✅ OK"
      )) %>%
      select(COUNTY, YEAR, MILK_LBS, COWS, data_flag)
  })
  
  
  
  
  # Composite suitability calculation
  composite_data <- reactive({
    req(is.data.frame(combined_data()), is.data.frame(accessibility_scores_df))
    
    efficiency_df <- combined_data() %>%
      filter(YEAR == 2024, !is.na(MILK_LBS), !is.na(COWS), COWS > 0) %>%
      mutate(
        milk_efficiency = MILK_LBS / (COWS * 30) # Match same calculation as monthly_efficiency
      ) %>%
      group_by(COUNTY) %>%
      summarise(
        milk_efficiency = mean(milk_efficiency, na.rm = TRUE),
        total_milk = sum(MILK_LBS, na.rm = TRUE),
        total_cows = mean(COWS, na.rm = TRUE),
        .groups = "drop"
      )
    
    county_names_df <- st_drop_geometry(target_va_counties)[, c("NAME"), drop = FALSE]
    
    left_join(
      left_join(
        county_names_df,
        accessibility_scores_df,
        by = "NAME"
      ),
      efficiency_df,
      by = c("NAME" = "COUNTY")
    ) %>%
      mutate(
        efficiency_score = rescale(milk_efficiency, to = c(0, 100), na.rm = TRUE),
        inventory_score = rescale(total_cows, to = c(0, 100), na.rm = TRUE),
        composite_index = round(0.4 * efficiency_score + 0.3 * inventory_score + 0.3 * score, 1)
      )
  })
  
  
  merged_suitability <- reactive({
    target_va_counties %>% left_join(composite_data(), by = "NAME")
  })
  
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
  
  
  # Suitability Score Map
  output$suitability_map <- renderLeaflet({
    data <- merged_suitability()
    highlighted <- selected_county_df()$county
    pal_suit <- colorNumeric(palette = "YlOrRd", 
                             domain = data$composite_index,
                             na.color = "transparent")
    
    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~ifelse(is.na(composite_index), "#bbbbbb", pal_suit(composite_index)),
        color = "#888888",
        weight = ~ifelse(NAME == highlighted, 3, 1),
        fillOpacity = 0.7,
        label = ~lapply(paste0(
          "<strong>", NAME, "</strong><br>",
          "Composite Score: <b>", composite_index, "</b><br>",
          "Efficiency: ", round(milk_efficiency, 1), " lbs/cow/month<br>",
          "Inventory: ", formatC(total_cows, format = "d", big.mark = ","), " cows<br>",
          "Accessibility Score: ", round(score, 1)
        ), htmltools::HTML)
      ) %>%
      addLegend(pal = pal_suit, 
                values = data$composite_index, 
                title = "Composite<br>Score",
                position="topleft")
  })
  
  output$interpolation_map <- renderLeaflet({
    req(selected_county_df())
    selected_name <- selected_county_df()$county
    
    selected_geom <- target_va_counties %>%
      filter(NAME == selected_name) %>%
      slice(1)  # ensure single geometry
    
    if (nrow(selected_geom) == 0) return(NULL)
    
    centroid <- st_centroid(selected_geom$geometry)
    coords <- tryCatch({
      st_coordinates(centroid)[1, ]
    }, error = function(e) {
      return(c(NA, NA))
    })
    
    leaflet(target_va_counties) %>%
      addProviderTiles("CartoDB.Positron") %>%
      
      # Draw all counties
      addPolygons(
        fillColor = "white",
        color = "gray80",
        weight = 1,
        fillOpacity = 0.5,
        label = ~NAME
      ) %>%
      
      # Highlight selected
      addPolygons(
        data = selected_geom,
        fillColor = "yellow",
        color = "darkblue",
        weight = 3,
        fillOpacity = 0.8,
        label = ~NAME
      ) %>%
      
      # Add centroid marker if valid
      { if (!any(is.na(coords))) {
        addCircleMarkers(
          lng = coords[1],
          lat = coords[2],
          radius = 7,
          color = "darkblue",
          fillColor = "yellow",
          fillOpacity = 0.9,
          weight = 2,
          label = "Suggested County"
        )
      } else {
        .
      }
      }
  })
  
  
  
  
  calc_transport_cost_full <- function(miles, mpg, fuel_price, truck_capacity, daily_production, operating_days) {
    gallons_needed <- miles / mpg
    cost_per_trip <- gallons_needed * fuel_price
    cost_per_gallon_trip <- cost_per_trip / truck_capacity
    trips_per_day <- daily_production / truck_capacity
    annual_cost <- cost_per_trip * trips_per_day * operating_days
    return(list(
      gallons_needed = round(gallons_needed, 2),
      cost_per_trip = round(cost_per_trip, 2),
      cost_per_gallon_trip = round(cost_per_gallon_trip, 4),
      trips_per_day = round(trips_per_day, 2),
      annual_cost = round(annual_cost, 2)
    ))
  }
  
  observeEvent(input$calc_btn, {
    output$transport_metrics <- renderTable({
      cities <- c("Arlington", "Richmond", "VA Beach")
      miles_list <- c(input$miles_arlington, input$miles_richmond, input$miles_vabeach)
      
      results <- lapply(1:3, function(i) {
        res <- calc_transport_cost_full(miles_list[i], input$mpg, input$fuel_price, input$truck_capacity, input$daily_production, input$operating_days)
        data.frame(
          Destination = cities[i],
          Gallons_Needed = res$gallons_needed,
          Cost_per_Trip = res$cost_per_trip,
          Cost_per_Gallon_Trip = res$cost_per_gallon_trip,
          Trips_per_Day = res$trips_per_day,
          Annual_Cost = res$annual_cost
        )
      })
      
      do.call(rbind, results)
    })
    
    output$annual_cost_bar <- renderPlot({
      cities <- c("Arlington", "Richmond", "VA Beach")
      miles_list <- c(input$miles_arlington, input$miles_richmond, input$miles_vabeach)
      
      annual_costs <- sapply(1:3, function(i) {
        res <- calc_transport_cost_full(miles_list[i], input$mpg, input$fuel_price, input$truck_capacity, input$daily_production, input$operating_days)
        res$annual_cost
      })
      
      bar_df <- data.frame(Destination = cities, Annual_Cost = annual_costs)
      
      ggplot(bar_df, aes(x = Destination, y = Annual_Cost, fill = Destination)) +
        geom_bar(stat = "identity") +
        labs(title = paste("Annual Transportation Cost for", input$user_city), y = "Cost ($)", x = "Destination") +
        theme_minimal()
    })
  })
  
  
  # Fix Top 5 counties table
  output$top5_table <- renderTable({
    merged_suitability() %>%
      st_drop_geometry() %>%
      arrange(desc(composite_index)) %>%
      mutate(title_case = str_to_title(NAME)) %>%
      mutate(composite_index = format(round(composite_index, 1))) %>%
      select(title_case, composite_index) %>%
      rename("Name" = "title_case",
             "Composite Index" = "composite_index") %>%
      slice_head(n = 5)
  })
  
  observeEvent(input$weight_slider, {
    print(paste("Slider value:", input$weight_slider))
    selected_county <- interpolated_df %>%
      filter(weight == input$weight_slider) %>%
      pull(county)
  })
  output$optimal_county_text <- renderUI({
    selected_row <- selected_county_df()
    if (is.null(selected_row)) {
      HTML("<span style='color: red;'>❌ No matching county found for this preference.</span>")
    } else {
      pct <- selected_row$weight
      paste_dairy <- paste0(100 - pct, "% dairy")
      paste_cost <- paste0(pct, "% cost")
      county <- selected_row$county
      
      HTML(paste0(
        "<strong style='color: #4CAF50;'>✅ Suggested county:</strong> ",
        "<span style='font-size: 18px; color: #5a3e1b;'>", county, "</span><br>",
        "<em>Based on your preference of ", paste_dairy, " and ", paste_cost, ".</em>"
      ))
    }
  })
  
  
  
  
  
  output$county_comparison_table <- renderTable({
    composite_data() %>%
      left_join(
        monthly_efficiency_summary() %>%
          rename(Best_Month = best_month, Best_Month_Efficiency = best_month_eff),
        by = c("NAME" = "COUNTY")
      ) %>%
      distinct(NAME, .keep_all = TRUE) %>%
      transmute(
        County = NAME,
        `Composite Score` = composite_index,
        `Avg Monthly Efficiency (lbs/cow)` = round(milk_efficiency, 1),
        `Total Cow Inventory` = formatC(total_cows, format = "d", big.mark = ","),
        `Accessibility Score` = round(score, 1),
        `Best Month` = Best_Month,
        `Efficiency in Best Month` = Best_Month_Efficiency
      )
  })
  
  
  output$efficiency_county_selector <- renderUI({
    selectInput("efficiency_counties", "Counties:",
                choices = sort(unique(combined_data()$COUNTY)),
                selected = head(sort(unique(combined_data()$COUNTY)), 3),
                multiple = TRUE)
  })
  output$efficiency_line_plot <- renderPlotly({
    df <- monthly_efficiency() %>%
      filter(str_to_title(COUNTY) == input$efficiency_county_single) %>%
      arrange(YEAR, month(DATE)) %>%
      mutate(
        time_id = as.Date(DATE),  # X-axis will be chronological date
        tooltip_text = paste0("Month: ", format(DATE, "%B %Y"),
                              "<br>Productivity: ", 
                              scales::comma(round(efficiency_lbs_per_cow, 1)), 
                              " lbs/cow/day")
      )
    
    plot_ly(df, x = ~time_id, y = ~efficiency_lbs_per_cow,
            type = 'scatter', mode = 'lines+markers',
            text = ~tooltip_text, hoverinfo = "text",
            line = list(color = 'darkorange'), marker = list(size = 6)) %>%
      layout(
        title = paste("Milk Productivity in", input$efficiency_county_single, "(2016–2025)"),
        xaxis = list(title = "Year", type = "date"),
        yaxis = list(title = "Milk Productivity (lbs/cow/day)"),
        hovermode = "closest"
      )
  })
  
  output$line_plot <- renderPlot({
    req(input$selected_county, input$selected_year)
    
    filtered_data <- milk_data %>%
      filter(str_to_title(COUNTY) == input$selected_county, YEAR == input$selected_year) %>%
      mutate(
        MONTH_NUM = month(DATE),
        MONTH_NAME = factor(month.name[MONTH_NUM], levels = month.name, ordered = TRUE)
      ) %>%
      group_by(MONTH_NAME) %>%
      summarise(`MILK (lbs)` = sum(`MILK (lbs)`, na.rm = TRUE), .groups = "drop")
    
    ggplot(filtered_data, aes(x = MONTH_NAME, y = `MILK (lbs)`)) +
      geom_line(aes(group = 1), color = "maroon", size = 1.5) +  # group = 1 fixes the line
      geom_point(color = "darkred", size = 2) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title = paste("Monthly Milk Production in", str_to_title(input$selected_county), 
                      "in", input$selected_year),
        x = "Month",
        y = "Milk Output (lbs)"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 16, face = "bold")
      )
  })
  
  output$milk_table <- renderDT({
    req(input$selected_county)
    milk_data_filtered <- milk_data %>% 
      filter(str_to_title(COUNTY) == input$selected_county) %>%
      arrange(desc(DATE)) %>%
      mutate(`Date` = format(DATE, "%B %Y")) %>%
      mutate(`Raw Milk Production (lbs)` = scales::comma(`MILK (lbs)`)) %>%
      mutate(`Number of Producers` = PRODUCERS) %>%
      select(`Date`, `Raw Milk Production (lbs)`, `Number of Producers`)
    datatable(milk_data_filtered,
              options = list(pageLength = 12, ordering=FALSE))
  })
  
  output$milk_avg <- renderUI({
    req(input$selected_county)
    
    df <- milk_data %>%
      filter(str_to_title(COUNTY) == input$selected_county) %>%
      group_by(YEAR) %>%
      summarise(avg_milk = mean(`MILK (lbs)`, na.rm = TRUE), .groups = "drop") %>%
      filter(YEAR %in% c(2011, 2025))
    
    if (nrow(df) < 2) {
      avg_text <- paste0("<b>", str_to_title(input$selected_county), "</b><br>Not enough data for 2011 and 2025.")
    } else {
      milk_2011 <- df$avg_milk[df$YEAR == 2011]
      milk_2025 <- df$avg_milk[df$YEAR == 2025]
      change <- (milk_2025 - milk_2011) / milk_2011 * 100
      
      avg_text <- paste0(
        #"<b>", str_to_title(input$selected_county), "</b><br>",
        "Average in 2011: ", scales::comma(round(milk_2011)), " lbs<br>",
        "Average in 2025: ", scales::comma(round(milk_2025)), " lbs<br>",
        ifelse(change >= 0, "📈 ", "📉 "),
        sprintf("%.1f%% %s in milk output", abs(change), ifelse(change >= 0, "increase", "decrease"))
      )
    }
    
    HTML(paste0("<h3 style='margin-top:0;'>Average Raw Milk Production in ", input$selected_county, "</h3>", avg_text))
  })
  
  
  
  
  output$roads_map <- renderLeaflet({
    leaflet(merged_accessibility) %>%
      setView(lng = -78.5, lat = 38.0, zoom = 7) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal_access(score),
        color = "black", weight = 1, fillOpacity = 0.7,
        label = ~lapply(paste0("<strong>", NAME, "</strong><br>Accessibility Score: ", round(score, 1)), htmltools::HTML),
        labelOptions = labelOptions(direction = "auto", style = list("font-weight" = "normal"), textsize = "14px")
      ) %>%
      addPolylines(data = primary_secondary_roads, color = "blue", weight = 2, opacity = 0.6) %>%
      addLegend(pal = pal_access, values = merged_accessibility$score, title = "Accessibility<br>Score")
  })
  
  output$accessibility_bar <- renderPlot({
    ggplot(accessibility_scores, aes(x = reorder(NAME, score), y = score, fill = score)) +
      geom_col() +
      coord_flip() +
      scale_fill_gradient(low = "lightblue", high = "blue") +
      labs(title = "Accessibility Scores by County", x = "County", y = "Score") +
      theme_minimal()
  })
  
  output$cow_inventory_table <- renderDT({
    dairy_cows_va() %>% filter(str_to_title(COUNTY) == input$cow_county)
  })
  
  output$cow_inventory_plot <- renderPlot({
    df <- dairy_cows_va() %>% filter(str_to_title(COUNTY) == input$cow_county)
    ggplot(df, aes(x = YEAR, y = COWS)) +
      geom_line(color = "forestgreen", size = 1.2) +
      geom_point(color = "darkgreen", size = 3) +
      scale_y_continuous(labels = scales::comma) +
      labs(title = paste("Dairy Cow Inventory Over Time in", input$cow_county), x = "Year", y = "Number of Dairy Cows") +
      theme_minimal()
  })
  
  
  output$monthly_efficiency_table <- renderTable({
    monthly_efficiency_summary() %>%
      mutate(month_name = month.name[best_month]) %>% 
      mutate(avg_efficiency = scales::comma(avg_efficiency)) %>%
      mutate(best_month_eff = scales::comma(best_month_eff)) %>%
      arrange(desc(avg_efficiency)) %>%
      select(COUNTY, avg_efficiency, best_month_eff, month_name) %>%
      rename(
        County = COUNTY,
        `Average Productivity (lbs/cow/day)` = avg_efficiency,
        `Best Month` = month_name,
        `Productivity in Best Month` = best_month_eff
      )
  })
  
  
  output$efficiency_county_selector <- renderUI({
    selectInput("efficiency_counties", "Counties:",
                choices = sort(unique(combined_data()$COUNTY)),
                selected = head(sort(unique(combined_data()$COUNTY)), 3),
                multiple = TRUE)
  })
  
  output$efficiency_plot <- renderPlot({
    req(input$efficiency_counties)
    df <- monthly_efficiency() %>%
      filter(COUNTY %in% input$efficiency_counties)
    
    ggplot(df, aes(x = MONTH_NAME, y = efficiency_lbs_per_cow, fill = COUNTY)) +
      geom_col(position = "dodge") +
      labs(
        title = paste("Milk Efficiency (lbs per cow per day) in", input$year_efficiency),
        x = "Month",
        y = "Efficiency (lbs/cow/day)"
      ) +
      theme_minimal()
  })
  # Graph 1: Annual Transportation Cost Faceted Bar Plot
  output$annual_cost_bar <- renderPlot({
    print("Rendering annual_cost_bar")   
    print(head(transport_df))           
    
    req(transport_df)
    
    annual_df <- transport_df %>%
      select(Location, Destination, Dollar_Per_Gallon_Mid_Size) %>%
      filter(!is.na(Dollar_Per_Gallon_Mid_Size))
    
    print(nrow(annual_df))               # New diagnostic
    
    if(nrow(annual_df) == 0){
      plot.new()
      text(0.5, 0.5, "No data available for Annual Cost")
    } else {
      ggplot(annual_df, aes(x = reorder(Location, -Dollar_Per_Gallon_Mid_Size), 
                            y = Dollar_Per_Gallon_Mid_Size, 
                            fill = Destination)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Annual Transportation Cost by Location and Destination",
             x = "Location", y = "Total Annual Cost ($)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  
  
  # Graph 2: Cost per Gallon Heatmap
  output$cost_per_gallon_tile <- renderPlot({
    req(transport_df)
    
    cost_df <- transport_df %>%
      select(Location, Destination, Dollar_Per_Gallon_Refrigerated_Truck)
    
    ggplot(cost_df, aes(x = Location, y = Destination, fill = Dollar_Per_Gallon_Refrigerated_Truck)) +
      geom_tile() +
      scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
      labs(title = "$ per Gallon (Refrigerated Truck) by County and Destination",
           x = "County",
           y = "Destination",
           fill = "$ per Gallon") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  output$gallons_needed_bubble <- renderPlot({
    req(transport_df)
    
    gallons_df <- transport_df %>%
      select(Location, Destination, Gallons_Needed)
    
    ggplot(gallons_df, aes(x = Location, y = Destination, size = Gallons_Needed, color = Destination)) +
      geom_point(alpha = 0.7) +
      scale_size_continuous(range = c(3,10)) +
      labs(title = "Gallons Needed per Trip by County and Destination",
           x = "County", y = "Destination") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  output$available_lots_table <- renderTable({
    data.frame(
      County = c("Rockingham (Harrisonburg)", "Augusta (Staunton)", "Shenandoah (Strasburg)"),
      Site_Name = c("Innovation Village @ Rockingham", "MEG Site", "Shenandoah Valley Site"),
      Address = c("North Valley Pike and Mount Clinton Pike, Harrisonburg, VA 22802",
                  "White Hill Road and I-64, Mint Springs, VA 24463",
                  "1 Shenandoah Valley Drive, Strasburg, VA 22657"),
      Contact_Name = rep("Abigail Patterson (VEDP)", 3),
      Email = rep("apatterson@vedp.org", 3),
      Phone = rep("804.545.5774", 3)
    )
  })
  
  
  
  
  composite_data <- reactive({
    req(is.data.frame(combined_data()), is.data.frame(accessibility_scores_df))
    
    efficiency_df <- combined_data() %>%
      filter(YEAR == 2024, !is.na(MILK_LBS), !is.na(COWS), COWS > 0) %>%
      mutate(
        milk_efficiency = MILK_LBS / (COWS * 12)  # original logic: yearly efficiency per cow per month
      ) %>%
      group_by(COUNTY) %>%
      summarise(
        milk_efficiency = mean(milk_efficiency, na.rm = TRUE),
        total_milk = sum(MILK_LBS, na.rm = TRUE),
        total_cows = mean(COWS, na.rm = TRUE),
        .groups = "drop"
      )
    
    county_names_df <- st_drop_geometry(target_va_counties)[, c("NAME"), drop = FALSE]
    
    left_join(
      left_join(
        county_names_df,
        accessibility_scores_df,
        by = "NAME"
      ),
      efficiency_df,
      by = c("NAME" = "COUNTY")
    ) %>%
      mutate(
        efficiency_score = rescale(milk_efficiency, to = c(0, 100), na.rm = TRUE),
        inventory_score = rescale(total_cows, to = c(0, 100), na.rm = TRUE),
        composite_index = round(0.4 * efficiency_score + 0.3 * inventory_score + 0.3 * score, 1)
      )
  })
  output$data_sources_table <- DT::renderDataTable({
    data.frame(
      Variable = c("Milk Production", "Dairy Cow Inventory", "Road Network", "Transportation Costs", "County Boundaries"),
      Description = c(
        "Monthly milk output by county",
        "Annual dairy cow headcount by county",
        "Primary & secondary road data for accessibility scoring",
        "Drive-time and cost estimates to processors",
        "Geospatial boundaries of VA counties"
      ),
      Source = c(
        "NASS QuickStats API",
        "NASS QuickStats API",
        "U.S. Census TIGER/Line",
        "Processed from OSRM + custom CSV",
        "TIGER/Line shapefiles"
      ),
      Format = c("API", "API", "Shapefile", "CSV", "Shapefile")
    )
  })
  
  output$efficiency_summary_table <- DT::renderDataTable({
    req(input$efficiency_counties)
    df <- combined_data() %>%
      filter(COUNTY %in% input$efficiency_counties, !is.na(COWS), !is.na(MILK_LBS))
    if (nrow(df) == 0) return(NULL)
    summary_data <- df %>% group_by(COUNTY) %>% group_map(~ {
      data_sub <- .x
      group_name <- .y$COUNTY
      cow_sd <- sd(data_sub$COWS, na.rm = TRUE)
      milk_sd <- sd(data_sub$MILK_LBS, na.rm = TRUE)
      if (cow_sd > 0 && milk_sd > 0) {
        cor_val <- cor(data_sub$COWS, data_sub$MILK_LBS, use = "complete.obs")
        cor_test <- cor.test(data_sub$COWS, data_sub$MILK_LBS)
        p_val <- cor_test$p.value
        slope <- coef(lm(MILK_LBS ~ COWS, data = data_sub))[2]
        interpretation <- case_when(
          is.na(cor_val) ~ "Not enough data",
          p_val > 0.05 ~ "No significant relationship",
          cor_val > 0 ~ "Positive: More cows -> more milk",
          cor_val < 0 ~ "Negative: More cows -> less milk",
          TRUE ~ "Unclear"
        )
      } else {
        cor_val <- p_val <- slope <- NA_real_
        interpretation <- "No variation"
      }
      tibble(
        COUNTY = group_name,
        cor_value = round(cor_val, 2),
        p_value = round(p_val, 2),
        lm_slope = round(slope, 2),
        interpretation = interpretation
      )
    }) %>% bind_rows()
    datatable(summary_data, rownames = FALSE, options = list(pageLength = 10))
  })
  
  observeEvent(input$show_info, {
    showModal(modalDialog(
      title = "About the Score",
      "The score ranks counties based on milk production, labor availability, and transportation access, helping identify the best locations for a dairy plant.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  output$temp_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -79.0, lat = 38.0, zoom = 7)
  })
  
  observe({
    if (input$radio == 1) {
      map_data <- reactive_data_weather()
      merged <- va_counties_thi %>%
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
        clearControls() %>%
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
      merged <- va_counties_thi %>%
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
      merged <- va_counties_thi %>%
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
      filter(county == input$selected_county_thi) 
    temp_data$time <- lubridate::with_tz(lubridate::ymd_hms(temp_data$time, tz = "UTC"), 
                                         tzone = Sys.timezone())
    temp_data$color <- ifelse(temp_data$time > Sys.time(), "Forecast", "Historical")
    temp_data <- temp_data[!is.na(temp_data$color), ]
    
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
                                   "<br>Temp: ", round(temperature_2m, 1), "°F")), size=0.8) +
      scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 day") +
      labs(
        title = paste("Recent Temperatures for", input$selected_county_thi),
        x = "Time", y = "Temperature (Fahrenheit)",
        color = ""
      ) +
      scale_color_manual(values = c("Forecast" = "orange", 
                                    "Historical" = "steelblue2", 
                                    "Optimal<br>Range" = "#00a000"),
                         na.translate = FALSE) +
      
      theme_minimal()
    
    
    plotly::ggplotly(p, tooltip = "text")
  })
  
  output$hum_line_plot <- renderPlotly({
    
    hum_data <- weather_data_hour %>%
      filter(county == input$selected_county_thi)
    hum_data$time <- lubridate::with_tz(lubridate::ymd_hms(hum_data$time, tz = "UTC"), 
                                        tzone = Sys.timezone())
    
    hum_data$color <- ifelse(hum_data$time > Sys.time(), "Forecast", "Historical")
    hum_data <- hum_data[!is.na(hum_data$color), ]
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
                                   "<br>Humidity: ", round(relative_humidity_2m, 1), "%")), size=0.8) +
      scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 day") +
      labs(
        title = paste("Recent Relative Humidity (RH) for", input$selected_county_thi),
        x = "Time", y = "Relative Humidity (%)",
        color = ""
      ) +
      scale_color_manual(values = c("Forecast" = "orange", 
                                    "Historical" = "steelblue2", 
                                    "Optimal<br>Range" = "#00a000"),
                         na.translate = FALSE) +
      theme_minimal()
    
    
    plotly::ggplotly(p_humid, tooltip = "text")
  })
  
  output$thi_line_plot <- renderPlotly({
    
    thi_data <- weather_data_hour %>%
      filter(county == input$selected_county_thi)
    library(lubridate)
    
    thi_data$time <- lubridate::with_tz(lubridate::ymd_hms(thi_data$time, tz = "UTC"), 
                                        tzone = Sys.timezone())
    
    thi_data$color <- ifelse(thi_data$time > Sys.time(), "Forecast", "Historical")
    thi_data <- thi_data[!is.na(thi_data$color), ]
    dummy_df <- data.frame(
      x = c(Sys.time(), Sys.time()),
      y = c(72, 72)
    )
    p_thi <- ggplot(thi_data, aes(x = time)) +
      annotate("segment",
               x = min(thi_data$time), xend = max(thi_data$time),
               y = 68, yend = 68,
               color = "#00a000",
               alpha = 0.5,
               linewidth = 0.5)+
      annotate("segment",
               x = min(thi_data$time), xend = max(thi_data$time),
               y = min(thi_data$thi_mean_hour), yend=min(thi_data$thi_mean_hour),
               color = "#00a000",
               alpha = 0.5,
               linewidth = 0.5) +
      annotate("rect",
               xmin = min(thi_data$time), xmax = max(thi_data$time),
               ymin = min(thi_data$thi_mean_hour), ymax = 68,
               fill = "#00a000", alpha = 0.2) +
      geom_line(aes(y = thi_mean_hour, color = color)) +
      geom_line(data = dummy_df, aes(x = x, y = y, color = "Optimal<br>Range"), size=0.5, alpha=0.7) +
      geom_point(aes(y = thi_mean_hour, color = color,
                     text = paste0(format(time, "%B %d, %Y %I:%M %p"),
                                   "<br>THI: ", round(thi_mean_hour, 1))), size=0.8) +
      scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 day") +
      labs(
        title = paste("Recent Temperature-Humidity Index (THI) for", input$selected_county_thi),
        x = "Time", y = "THI",
        color = ""
      ) +
      scale_color_manual(values = c("Forecast" = "orange", 
                                    "Historical" = "steelblue2", 
                                    "Optimal<br>Range" = "#00a000"),
                         na.translate = FALSE) +
      theme_minimal()
    
    
    plotly::ggplotly(p_thi, tooltip = "text")
  })
  
  output$table_title <- renderUI({
    h4(paste("Recent Weather Table for", input$selected_county_thi))
  })
  
  output$data_table <- renderDT({
    selected_data <- weather_data_hour %>%
      filter(county == input$selected_county_thi) %>%
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
