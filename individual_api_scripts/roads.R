# --- Load libraries ---
library(sf)
library(tmap)
library(dplyr)
library(stringr)
library(readr)
library(geosphere)

# --- Load road network ---
roads <- read_sf("/Users/irmakocel/Desktop/Dairy_new/tl_2023_51_prisecroads-2")
gdb_path <- "/Users/irmakocel/Desktop/dspgdata/usda_ag_census_20230213 2/usda_ag_census_20230213.gdb"
dairy <- st_read(gdb_path, layer = "dairy")
head(dairy)  

#clean for va
va_dairy <- dairy[dairy$state == "VIRGINIA", ]
target_counties <- c("SHENANDOAH", "WARREN", "AUGUSTA", "ROCKINGHAM", "PAGE", "FREDERICK",
                     "ROCKBRIDGE", "CLARKE", "HARRISONBURG", "WINCHESTER", "HIGHLAND",
                     "WAYNESBORO", "JEFFERSON")

va_focus <- va_dairy[toupper(va_dairy$county) %in% target_counties, ]
View(va_focus)
write.csv(as.data.frame(va_focus), "va_dairy_focus.csv", row.names = FALSE)

# --- Create legend line for roads ---
legend_line <- st_sf(
  label = "Primary & Secondary Roads",
  geometry = st_sfc(st_linestring(rbind(c(-78, 37), c(-77.5, 37))), crs = 4326)
)
legend_line <- st_transform(legend_line, st_crs(roads))

# --- Transform and intersect with focus counties ---
roads <- st_transform(roads, st_crs(va_focus))
roads_focus <- st_intersection(roads, va_focus)

# --- Plot full road network ---
tmap_mode("plot")
tm_shape(va_dairy) +
  tm_polygons(col = "seashell2", border.col = "maroon") +
  tm_shape(va_focus) +
  tm_borders(col = "magenta2", lwd = 2) +
  tm_shape(roads) +
  tm_lines(col = "dodgerblue", lwd = 0.5, col_alpha = 0.6, legend.col.show = FALSE) +
  tm_shape(legend_line) +
  tm_lines(col = "dodgerblue", lwd = 2) +
  tm_add_legend(
    type = "line",
    col = c("dodgerblue", "maroon", "magenta2"),
    labels = c("Primary & Secondary Roads", "All Virginia County Lines", "Shenandoah Valley"),
    lwd = c(2, 2, 2)
  ) +
  tm_title("Road Network Across Virginia Dairy Counties")

# --- Load urban areas ---
gdb_path <- "/Users/irmakocel/Desktop/Dairy_new/tlgdb_2022_a_51_va.gdb"
urban_areas <- st_read(dsn = gdb_path, layer = "Incorporated_Place")
va_focus_transformed <- st_transform(va_focus, st_crs(urban_areas))
urban_focus <- st_filter(urban_areas, va_focus_transformed)

# --- Get urban centroids ---
urban_pts <- st_centroid(urban_focus)

# --- Read and clean population data ---
popdata <- read_csv("~/Desktop/Dairy_new/VA_Town_Population_Estimates.csv")
popdata_clean <- popdata %>%
  mutate(NAME = str_remove(Town, ", Virginia")) %>%
  mutate(NAME = str_remove(NAME, " town| city"))

# --- Match names and join population ---
urban_focus <- urban_focus %>%
  mutate(NAME = str_remove(NAMELSAD, " town| city")) %>%
  left_join(popdata_clean, by = "NAME")

# --- Rank and group towns ---
urban_focus_ranked <- urban_focus %>%
  arrange(desc(`2024`)) %>%
  mutate(rank = row_number(),
         pop_group = case_when(
           is.na(`2024`) ~ "Missing",
           rank <= 3 ~ "Top 3",
           rank <= 10 ~ "Top 4–10",
           TRUE ~ "Others"
         ))

# --- Assign weights for optimization ---
urban_focus_weighted <- urban_focus_ranked %>%
  mutate(weight = case_when(
    pop_group == "Top 3" ~ 3,
    pop_group == "Top 4–10" ~ 2,
    pop_group == "Others" ~ 1,
    TRUE ~ 0
  ))

# --- Compute weighted centroid ---
urban_pts <- st_centroid(urban_focus_weighted)
coords <- st_coordinates(urban_pts)
weights <- urban_focus_weighted$weight
weighted_lon <- sum(coords[,1] * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE)
weighted_lat <- sum(coords[,2] * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE)
weighted_optimal_point <- st_sfc(st_point(c(weighted_lon, weighted_lat)), crs = st_crs(urban_pts))
weighted_optimal_site <- st_sf(name = "Weighted Optimal Plant", geometry = weighted_optimal_point)

# --- Final Map with full legend ---
tmap_mode("plot")
tm_shape(va_dairy) +
  tm_polygons(col = "seashell2", border.col = "gray70") +
  tm_shape(roads_focus) +
  tm_lines(col = "dodgerblue", lwd = 0.5, alpha = 0.5) +
  tm_shape(urban_focus_ranked) +
  tm_dots(
    col = "pop_group",
    palette = c("Top 3" = "darkred", "Top 4–10" = "orange", "Others" = "gray60", "Missing" = "lightblue"),
    size = 0.3
  ) +
  tm_shape(weighted_optimal_site) +
  tm_symbols(col = "green", size = 0.6) +
  tm_text("name", size = 1, col = "darkgreen", fontface = "bold") +
  tm_add_legend(
    type = "symbol",
    col = c("darkred", "orange", "gray60", "lightblue", "green"),
    shape = rep(16, 5),
    size = c(0.6, 0.5, 0.4, 0.4, 0.6),
    labels = c("Top 3 Cities", "Top 4–10 Cities", "Other Cities", "Cities w/ Missing Pop", "Recommended Plant Site"),
    title = "Key Locations"
  ) +
  tm_layout(
    title = "Optimized Dairy Plant Location (Weighted by Population)",
    legend.outside = TRUE,
    legend.title.size = 1.1,
    legend.text.size = 0.9
  )
