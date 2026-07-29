library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)

db_path = "D:/Data_Science/Clean Data/norfolk_suffolk.db"
chart_path = "D:/Data_Science/Charts/10_Crime_LineChart_DrugOffence.png"

con = dbConnect(SQLite(), db_path)

population_by_county = dbGetQuery(con, "
  SELECT l.county_id, cn.county_name, SUM(p.population) AS total_population
  FROM fact_population p
  JOIN dim_lsoa l ON p.lsoa_code = l.lsoa_code
  JOIN dim_county cn ON l.county_id = cn.county_id
  GROUP BY l.county_id, cn.county_name
")

drug_offences_yearly = dbGetQuery(con, "
  SELECT
    c.year,
    cn.county_name,
    COUNT(*) AS drug_count
  FROM fact_crime c
  JOIN dim_lsoa l ON c.lsoa_code = l.lsoa_code
  JOIN dim_county cn ON l.county_id = cn.county_id
  WHERE c.crime_type = 'Drugs'
  GROUP BY c.year, cn.county_name
  ORDER BY c.year
")

dbDisconnect(con)

cat("\nYears available for Drugs crime type : ")
print(sort(unique(drug_offences_yearly$year)))

drug_rate = drug_offences_yearly %>%
  left_join(population_by_county, by = "county_name") %>%
  mutate(
    rate_per_100k = (drug_count / total_population) * 100000
  )

cat("\n============================\n")
cat("DRUG OFFENCE RATE PER 100,000 BY YEAR (2023-2026)\n")
cat("============================\n")
print(drug_rate %>% select(year, county_name, drug_count, rate_per_100k))

cat("\nNote: 2026 may be a partial year depending on data extraction date.\n")

line_chart = ggplot(drug_rate, aes(x = year, y = rate_per_100k, color = county_name, group = county_name)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = unique(drug_rate$year)) +
  scale_color_manual(values = c("Norfolk" = "#2E86AB", "Suffolk" = "#E67E22")) +
  labs(
    title = "Drug Offence Rate per 100,000 (2023-2026)",
    subtitle = "Norfolk vs Suffolk, based on Norfolk/Suffolk Constabulary records",
    x = "Year",
    y = "Drug Offences per 100,000 People",
    color = "County"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

print(line_chart)

dir.create("D:/Data_Science/Charts", showWarnings = FALSE, recursive = TRUE)

ggsave(
  chart_path,
  plot = line_chart,
  width = 8,
  height = 6,
  dpi = 300
)

cat("\nChart saved to :", chart_path, "\n")
