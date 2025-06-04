#primary and secondary roads in Virginia
library(sf)
list.files("/Users/irmakocel/Desktop")
data = read_sf("tl_2023_51_prisecroads.shp")
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




library(sf)
library(tmap)

# mapping the 
tmap_mode("plot")

tm_shape(va_dairy) +
  tm_polygons(
    col = "gray90",     
    border.col = "white"
  ) +
  tm_shape(va_focus) +
  tm_polygons(
    col = "total_heads_2017",
    fill.scale = tm_scale_manual(
      breaks = c(0, 10, 250, 500, 1000, Inf),
      labels = c("0–10", "11–250", "251–500", "501–1000", "1000+"),
      values = "Blues"
    ),
    fill.legend = tm_legend(title = "Dairy Cows (2017)"),
    border.col = "blue",
    border.lwd = 2
  ) +
  tm_title("VA Dairy with Target Counties Highlighted")




