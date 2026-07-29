library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)

db_path = "D:/Data_Science/Clean Data/norfolk_suffolk.db"
chart_path = "D:/Data_Science/Charts/11_Education_BoxPlot_Att8.png"

con = dbConnect(SQLite(), db_path)

education = dbGetQuery(con, "
  SELECT e.estab, e.academic_year, e.att8_score, s.school_name, s.town, c.county_name
  FROM fact_education e
  JOIN dim_school s ON e.estab = s.estab
  JOIN dim_county c ON s.county_id = c.county_id
  WHERE e.att8_score IS NOT NULL
")

dbDisconnect(con)

cat("\nAcademic years available : ")
print(sort(unique(education$academic_year)))

cat("\nRows with valid Attainment 8 score : ", nrow(education))

education = education %>%
  mutate(county_name = toupper(county_name))

att8_stats = education %>%
  group_by(county_name) %>%
  summarise(
    mean_att8 = mean(att8_score, na.rm = TRUE),
    median_att8 = median(att8_score, na.rm = TRUE),
    sd_att8 = sd(att8_score, na.rm = TRUE),
    n_records = n(),
    .groups = "drop"
  )

cat("\n============================\n")
cat("ATTAINMENT 8 SCORE DISTRIBUTION - ALL YEARS COMBINED (2021-2025)\n")
cat("============================\n")
print(att8_stats)

box_plot = ggplot(education, aes(x = county_name, y = att8_score, fill = county_name)) +
  geom_boxplot(width = 0.5, outlier.alpha = 0.5) +
  labs(
    title = "Attainment 8 Score Comparison: Norfolk vs Suffolk",
    x = "County",
    y = "Average Attainment 8 Score",
    fill = "County"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold")
  )

print(box_plot)

dir.create("D:/Data_Science/Charts", showWarnings = FALSE, recursive = TRUE)

ggsave(
  chart_path,
  plot = box_plot,
  width = 7,
  height = 6,
  dpi = 300
)

cat("\nChart saved to :", chart_path, "\n")