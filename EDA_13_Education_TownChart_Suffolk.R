library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(ggplot2)

db_path = "D:/Data_Science/Clean Data/norfolk_suffolk.db"
chart_path = "D:/Data_Science/Charts/13_Education_TownChart_Suffolk.png"

con = dbConnect(SQLite(), db_path)

education_suffolk = dbGetQuery(con, "
  SELECT e.academic_year, e.att8_score, s.town
  FROM fact_education e
  JOIN dim_school s ON e.estab = s.estab
  JOIN dim_county c ON s.county_id = c.county_id
  WHERE c.county_name = 'Suffolk'
    AND e.att8_score IS NOT NULL
")

dbDisconnect(con)

cat("\nAcademic years available for Suffolk : ")
print(sort(unique(education_suffolk$academic_year)))

cat("\nSuffolk rows with valid Attainment 8 score : ", nrow(education_suffolk))

education_suffolk = education_suffolk %>%
  mutate(town = ifelse(is.na(town) | town == "", "NA", town))

town_year_avg = education_suffolk %>%
  group_by(town, academic_year) %>%
  summarise(
    avg_att8 = mean(att8_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(town, academic_year) %>%
  arrange(town, academic_year)

cat("\n============================\n")
cat("SUFFOLK AVERAGE ATTAINMENT 8 SCORE BY TOWN AND YEAR\n")
cat("============================\n")
print(town_year_avg, n = 30)

line_chart = ggplot(
  town_year_avg,
  aes(x = academic_year, y = avg_att8, color = town, group = town)
) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  geom_point(size = 1.8, na.rm = TRUE) +
  labs(
    title = "Suffolk Average Attainment 8 Score (2021-2025)",
    x = "Academic Year",
    y = "Average Attainment 8 Score",
    color = "town"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  )

print(line_chart)

dir.create("D:/Data_Science/Charts", showWarnings = FALSE, recursive = TRUE)

ggsave(
  chart_path,
  plot = line_chart,
  width = 11,
  height = 6,
  dpi = 300
)

cat("\nChart saved to :", chart_path, "\n")