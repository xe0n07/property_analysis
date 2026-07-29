library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)

db_path = "D:/Data_Science/Clean Data/norfolk_suffolk.db"
chart_path = "D:/Data_Science/Charts/09_Crime_Pie_Robbery.png"

con = dbConnect(SQLite(), db_path)

robbery_by_lsoa = dbGetQuery(con, "
  SELECT
    l.lsoa_code,
    l.lsoa_name,
    cn.county_name,
    COUNT(*) AS robbery_count
  FROM fact_crime c
  JOIN dim_lsoa l ON c.lsoa_code = l.lsoa_code
  JOIN dim_county cn ON l.county_id = cn.county_id
  WHERE c.crime_type = 'Robbery'
    AND c.in_boundary = 1
  GROUP BY l.lsoa_code, l.lsoa_name, cn.county_name
")

population_by_lsoa = dbGetQuery(con, "
  SELECT lsoa_code, population
  FROM fact_population
")

dbDisconnect(con)

cat("\nRobbery-by-LSOA rows : ", nrow(robbery_by_lsoa))

robbery_with_pop = robbery_by_lsoa %>%
  left_join(population_by_lsoa, by = "lsoa_code") %>%
  mutate(
    district = str_trim(str_remove(lsoa_name, "\\s+[A-Za-z0-9]+$"))
  ) %>%
  filter(!is.na(population), population > 0)

district_robbery_rate = robbery_with_pop %>%
  group_by(county_name, district) %>%
  summarise(
    total_robberies = sum(robbery_count),
    total_population = sum(population),
    .groups = "drop"
  ) %>%
  mutate(
    rate_per_100k = (total_robberies / total_population) * 100000
  ) %>%
  arrange(desc(rate_per_100k))

cat("\n============================\n")
cat("ROBBERY RATE PER 100,000 BY DISTRICT\n")
cat("============================\n")
print(district_robbery_rate)

pie_data = district_robbery_rate %>%
  mutate(
    label = paste0(district, " (", round(rate_per_100k, 1), ")"),
    pct = rate_per_100k / sum(rate_per_100k) * 100
  ) %>%
  arrange(desc(district))

pie_chart = ggplot(pie_data, aes(x = "", y = rate_per_100k, fill = district)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = paste0(round(pct, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3.2,
    color = "white",
    fontface = "bold"
  ) +
  labs(
    title = "Robbery Rate per 100,000 by District",
    subtitle = "Norfolk and Suffolk districts, share of combined robbery rate",
    fill = "District"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right"
  )

print(pie_chart)

dir.create("D:/Data_Science/Charts", showWarnings = FALSE, recursive = TRUE)

ggsave(
  chart_path,
  plot = pie_chart,
  width = 9,
  height = 7,
  dpi = 300
)

cat("\nChart saved to :", chart_path, "\n")
