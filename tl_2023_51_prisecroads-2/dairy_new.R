#primary and secondary roads in Virginia
library(sf)
list.files("/Users/irmakocel/Desktop")
roads = read_sf("tl_2023_51_prisecroads.shp")
library(sf)
library(tmap)

# Create dummy line for legend
library(sf)
library(tmap)

# Dummy line for legend
legend_line <- st_sf(
  label = "Primary & Secondary Roads",
  geometry = st_sfc(st_linestring(rbind(c(-78, 37), c(-77.5, 37))), crs = 4326)
)
legend_line <- st_transform(legend_line, st_crs(roads))

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
    labels = c(
      "Primary & Secondary Roads",
      "All Virginia County Lines",
      "Shenandoah Valley"
    ),
    lwd = c(2, 2, 2)
  ) +
  
  tm_title("Road Network Across Virginia Dairy Counties")


names(roads)
table(roads$RTTYP)



head(data)
plot(data$geometry)

#Cattle operations by county
library(sf)
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


#dair cow amounts graph
library(sf)
library(tmap)

tmap_mode("plot")

tm_shape(va_dairy) +
  tm_polygons(
    col = "gray90",
    border.col = "white"
  ) +
  
  tm_shape(va_focus) +
  tm_polygons(
    col = "total_heads_2017",  # Use the actual column name here!
    style = "fixed",
    breaks = c(0, 10, 250, 500, 1000, Inf),
    labels = c("0–10", "11–250", "251–500", "501–1000", "1000+"),
    palette = "blues",
    title = "Dairy Cows (2017)",
    border.col = "pink",
    lwd = 2
  ) +
  
  tm_title("VA Dairy Cow Numbers Within Shenandoah Valley")


#comparing dairy cow 2002-2007-2012-2017
library(dplyr)
library(tidyr)
library(tmap)

va_focus_long <- va_focus %>%
  select(county, Shape, total_heads_2002, total_heads_2007, total_heads_2012, total_heads_2017) %>%
  rename(geometry = Shape) %>%
  pivot_longer(
    cols = starts_with("total_heads"),
    names_to = "year",
    names_prefix = "total_heads_",
    values_to = "count"
  )

va_focus_long <- st_as_sf(va_focus_long)

va_focus_long$year <- factor(va_focus_long$year, levels = c("2002", "2007", "2012", "2017"))

tmap_mode("plot")
tm_shape(va_focus_long) +
  tm_polygons(
    col = "count",
    style = "fixed",
    breaks = c(0, 10, 250, 500, 1000, Inf),
    labels = c("0–10", "11–250", "251–500", "501–1000", "1000+"),
    palette = "Blues",
    title = "Dairy Cow Count"
  ) +
  tm_facets(by = "year", ncol = 2) +
  tm_layout(main.title = "Dairy Cow Counts in Shenandoah Valley (2002–2017)", main.title.size = 1.2)


