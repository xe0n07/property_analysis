# =============================================================================
# EDA_13_Education_TownChart_Suffolk.R
# Line chart: average Attainment 8 score by town/district, Suffolk, 2021-2025
#
# FIX vs. original version: same issue and same fix as EDA_12 (Norfolk) -
# needs the multi-year fact_education table from 10_Education_MultiYear_Loader.R.
# =============================================================================

library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(forcats)

db_path    <- "D:/Data_Science/Clean Data/norfolk_suffolk.db"
chart_path <- "D:/Data_Science/Charts/13_Education_TownChart_Suffolk.png"

con <- dbConnect(SQLite(), db_path)

education_suffolk <- dbGetQuery(con, "
  SELECT e.att8_score, e.town, e.academic_year
  FROM fact_education e
  WHERE e.county_name = 'Suffolk'
    AND e.att8_score IS NOT NULL
")

dbDisconnect(con)

if (nrow(education_suffolk) == 0 || n_distinct(education_suffolk$academic_year) < 2) {
  stop("fact_education has fewer than 2 academic years for Suffolk. Run 10_Education_MultiYear_Loader.R with all your downloaded DfE year files first.")
}

cat("\nSuffolk school-year rows with valid Attainment 8 score :", nrow(education_suffolk))
cat("\nAcademic years available :", paste(sort(unique(education_suffolk$academic_year)), collapse = ", "), "\n")

town_year_avg <- education_suffolk %>%
  filter(!is.na(town)) %>%
  group_by(town, academic_year) %>%
  summarise(
    avg_att8  = mean(att8_score, na.rm = TRUE),
    n_schools = n(),
    .groups = "drop"
  ) %>%
  filter(n_schools >= 2)

towns_with_trend <- town_year_avg %>%
  count(town) %>%
  filter(n >= 2) %>%
  pull(town)

plot_data <- town_year_avg %>%
  filter(town %in% towns_with_trend) %>%
  mutate(academic_year = factor(academic_year, levels = sort(unique(academic_year))))

cat("\n============================\n")
cat("AVERAGE ATTAINMENT 8 SCORE BY TOWN - SUFFOLK (2021-2025)\n")
cat("============================\n")
print(plot_data %>% arrange(town, academic_year))

line_chart <- ggplot(plot_data, aes(x = academic_year, y = avg_att8, color = town, group = town)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Suffolk Average Attainment 8 Score (2021-2025)",
    subtitle = "Town-wise average, towns with at least 2 schools reporting each year",
    x = "Academic Year",
    y = "Average Attainment 8 Score",
    color = "Town"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0)
  )

print(line_chart)

dir.create("D:/Data_Science/Charts", showWarnings = FALSE, recursive = TRUE)

ggsave(
  chart_path,
  plot = line_chart,
  width = 10,
  height = 7,
  dpi = 300
)

cat("\nChart saved to :", chart_path, "\n")
