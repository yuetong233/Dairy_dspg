library(readr)
library(dplyr)
library(stringr)
install.packages("ggplot2")
library(ggplot2)
# Load your new CSV
labor_data <- read_csv("C:/Users/irmo2303/Downloads/DSPG_foler/197047A2-617B-359E-905A-682FAA43D0D7.csv")

# Filter to Shenandoah Valley counties
target_counties <- toupper(c("Shenandoah", "Warren", "Augusta", "Rockingham", 
                             "Page", "Frederick", "Clarke", "Rockbridge"))

# Filter for total hired labor data
labor_filtered <- labor_data %>%
  filter(County %in% target_counties,
         str_detect(`Data Item`, regex("LABOR, HIRED", ignore_case = TRUE))) %>%
  filter(!Value %in% c("(D)", "(NA)", "Z")) %>%
  mutate(Value = as.numeric(gsub(",", "", Value)))

# Summarize or plot depending on what you want
labor_summary <- labor_filtered %>%
  group_by(County) %>%
  summarise(Average_Hired_Labor = mean(Value, na.rm = TRUE)) %>%
  arrange(desc(Average_Hired_Labor))

View(labor_summary)
library(ggplot2)

ggplot(labor_summary, aes(x = reorder(County, -Average_Hired_Labor), y = Average_Hired_Labor)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Average % of Farms Using Hired Labor",
       x = "County", y = "Percent") +
  theme_minimal()

