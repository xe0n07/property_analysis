# =============================================================================
# EDA_12_Education_TownChart_Norfolk.R
# Line chart: average Attainment 8 score by town/district, Norfolk, 2021-2025
#
# FIX vs. original version:
# The original script produced a single-year BAR chart because
# fact_education only had 2024-2025 data - a line needs multiple points on
# the x-axis. Run 10_Education_MultiYear_Loader.R first to rebuild
# fact_education with 2021-2025 data, then this script produces the actual
# line chart the brief and your friend's reference (Fig 4.11) require.
# =============================================================================

library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(forcats)

db_path    <- "D:/Data_Science/Clean Data/norfolk_suffolk.db"
chart_path <- "D:/Data_Science/Charts/12_Education_TownChart_Norfolk.png"

con <- dbConnect(SQLite(), db_path)

education_norfolk <- dbGetQuery(con, "
  SELECT e.att8_score, e.town, e.academic_year
  FROM fact_education e
  WHERE e.county_name = 'Norfolk'
    AND e.att8_score IS NOT NULL
")

dbDisconnect(con)

if (nrow(education_norfolk) == 0 || n_distinct(education_norfolk$academic_year) < 2) {
  stop("fact_education has fewer than 2 academic years for Norfolk. Run 10_Education_MultiYear_Loader.R with all your downloaded DfE year files first.")
}

cat("\nNorfolk school-year rows with valid Attainment 8 score :", nrow(education_norfolk))
cat("\nAcademic years available :", paste(sort(unique(education_norfolk$academic_year)), collapse = ", "), "\n")

# ---- Average Attainment 8 per town, per academic year ----------------------
# Only keep towns with at least 2 schools reporting in a given year, same
# reliability threshold as your original script, applied per year here.
town_year_avg <- education_norfolk %>%
  filter(!is.na(town)) %>%
  group_by(town, academic_year) %>%
  summarise(
    avg_att8  = mean(att8_score, na.rm = TRUE),
    n_schools = n(),
    .groups = "drop"
  ) %>%
  filter(n_schools >= 2)

# Keep only towns that have data across at least 2 of the years, so the line
# actually shows a trend rather than a single dot
towns_with_trend <- town_year_avg %>%
  count(town) %>%
  filter(n >= 2) %>%
  pull(town)

plot_data <- town_year_avg %>%
  filter(town %in% towns_with_trend) %>%
  mutate(academic_year = factor(academic_year, levels = sort(unique(academic_year))))

cat("\n============================\n")
cat("AVERAGE ATTAINMENT 8 SCORE BY TOWN - NORFOLK (2021-2025)\n")
cat("============================\n")
print(plot_data %>% arrange(town, academic_year))

line_chart <- ggplot(plot_data, aes(x = academic_year, y = avg_att8, color = town, group = town)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Norfolk Average Attainment 8 Score (2021-2025)",
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
