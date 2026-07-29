library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(fmsb)

db_path = "D:/Data_Science/Clean Data/norfolk_suffolk.db"
chart_path = "D:/Data_Science/Charts/08_Crime_Radar_VehicleCrime.png"

con = dbConnect(SQLite(), db_path)

population_by_county = dbGetQuery(con, "
  SELECT l.county_id, cn.county_name, SUM(p.population) AS total_population
  FROM fact_population p
  JOIN dim_lsoa l ON p.lsoa_code = l.lsoa_code
  JOIN dim_county cn ON l.county_id = cn.county_id
  GROUP BY l.county_id, cn.county_name
")

vehicle_crime_monthly = dbGetQuery(con, "
  SELECT
    strftime('%m', c.crime_date) AS month_num,
    cn.county_name,
    COUNT(*) AS crime_count
  FROM fact_crime c
  JOIN dim_lsoa l ON c.lsoa_code = l.lsoa_code
  JOIN dim_county cn ON l.county_id = cn.county_id
  WHERE c.crime_type = 'Vehicle Crime'
    AND (
      (strftime('%Y', c.crime_date) = '2024' AND CAST(strftime('%m', c.crime_date) AS INTEGER) >= 5)
      OR
      (strftime('%Y', c.crime_date) = '2025' AND CAST(strftime('%m', c.crime_date) AS INTEGER) <= 4)
    )
  GROUP BY month_num, cn.county_name
")

dbDisconnect(con)

cat("\nPopulation by county\n")
print(population_by_county)

cat("\nVehicle crime monthly counts (May 2024 - April 2025)\n")
print(vehicle_crime_monthly)

month_labels = tibble(
  month_num = sprintf("%02d", c(5:12, 1:4)),
  month_name = c("May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr"),
  month_order = 1:12
)

vehicle_crime_rate = vehicle_crime_monthly %>%
  left_join(population_by_county, by = "county_name") %>%
  mutate(
    rate_per_100k = (crime_count / total_population) * 100000
  ) %>%
  left_join(month_labels, by = "month_num") %>%
  filter(!is.na(month_order)) %>%
  arrange(month_order)

cat("\n============================\n")
cat("VEHICLE CRIME RATE PER 100,000 (MAY 2024 - APRIL 2025)\n")
cat("============================\n")
print(vehicle_crime_rate %>% select(month_name, county_name, rate_per_100k))

radar_data = vehicle_crime_rate %>%
  select(month_name, county_name, rate_per_100k) %>%
  pivot_wider(names_from = month_name, values_from = rate_per_100k) %>%
  select(county_name, all_of(month_labels$month_name))

radar_matrix = as.data.frame(radar_data[, -1])
rownames(radar_matrix) = radar_data$county_name

max_val = ceiling(max(radar_matrix, na.rm = TRUE) * 1.1)
min_val = 0

radar_plot_data = rbind(
  rep(max_val, ncol(radar_matrix)),
  rep(min_val, ncol(radar_matrix)),
  radar_matrix
)

dir.create("D:/Data_Science/Charts", showWarnings = FALSE, recursive = TRUE)

png(chart_path, width = 2400, height = 2200, res = 300)

radarchart(
  radar_plot_data,
  pcol = c("#2E86AB", "#E67E22"),
  pfcol = scales::alpha(c("#2E86AB", "#E67E22"), 0.25),
  plwd = 2.5,
  plty = 1,
  cglcol = "grey70",
  cglty = 1,
  axislabcol = "grey40",
  vlcex = 0.9,
  title = "Vehicle Crime Rate per 100,000 (May 2024 - April 2025)"
)

legend(
  "topright",
  legend = rownames(radar_matrix),
  col = c("#2E86AB", "#E67E22"),
  lty = 1,
  lwd = 2.5,
  bty = "n"
)

dev.off()

cat("\nChart saved to :", chart_path, "\n")
