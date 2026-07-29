library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)

db_path = "D:/Data_Science/Clean Data/norfolk_suffolk.db"
chart_path = "D:/Data_Science/Charts/LM_01_HousePrice_vs_DrugOffenceRate.png"

con = dbConnect(SQLite(), db_path)

norfolk_districts = c(
  "Breckland", "Broadland", "Great Yarmouth",
  "King's Lynn and West Norfolk", "North Norfolk",
  "Norwich", "South Norfolk"
)

suffolk_districts = c(
  "Babergh", "East Suffolk", "Ipswich",
  "Mid Suffolk", "West Suffolk"
)

hp_years = dbGetQuery(con, "SELECT DISTINCT year FROM fact_house_price ORDER BY year")$year
crime_years = dbGetQuery(con, "SELECT DISTINCT year FROM fact_crime ORDER BY year")$year

common_years = intersect(hp_years, crime_years)

cat("\nHouse price years available : ")
print(hp_years)
cat("\nCrime years available : ")
print(crime_years)
cat("\nCommon years used for comparison : ")
print(common_years)

years_clause = paste(common_years, collapse = ",")

house_price_district = dbGetQuery(con, sprintf("
  SELECT d.district_name, cn.county_name, AVG(hp.price) AS avg_house_price
  FROM fact_house_price hp
  JOIN dim_district d ON hp.district_id = d.district_id
  JOIN dim_county cn ON d.county_id = cn.county_id
  WHERE hp.year IN (%s)
  GROUP BY d.district_name, cn.county_name
", years_clause))

drug_crime = dbGetQuery(con, sprintf("
  SELECT c.lsoa_code, l.lsoa_name, cn.county_name
  FROM fact_crime c
  JOIN dim_lsoa l ON c.lsoa_code = l.lsoa_code
  JOIN dim_county cn ON l.county_id = cn.county_id
  WHERE c.crime_type = 'Drugs'
    AND c.in_boundary = 1
    AND c.year IN (%s)
", years_clause))

population_by_lsoa = dbGetQuery(con, "SELECT lsoa_code, population FROM fact_population")

dbDisconnect(con)

cat("\nDrug crime rows retrieved : ", nrow(drug_crime))

drug_by_district = drug_crime %>%
  mutate(district_name = str_trim(str_remove(lsoa_name, "\\s+[A-Za-z0-9]+$"))) %>%
  count(district_name, county_name, lsoa_code, name = "drug_count") %>%
  left_join(population_by_lsoa, by = "lsoa_code") %>%
  filter(!is.na(population), population > 0) %>%
  group_by(district_name, county_name) %>%
  summarise(
    total_drug_offences = sum(drug_count),
    total_population = sum(population),
    .groups = "drop"
  ) %>%
  mutate(drug_rate_per_100k = (total_drug_offences / total_population) * 100000)

cat("\n============================\n")
cat("DRUG OFFENCE RATE PER 100,000 BY DISTRICT\n")
cat("============================\n")
print(drug_by_district)

model_data = house_price_district %>%
  inner_join(drug_by_district, by = c("district_name", "county_name"))

cat("\n============================\n")
cat("HOUSE PRICE vs DRUG OFFENCE RATE - MODEL DATA\n")
cat("============================\n")
print(model_data %>% select(district_name, county_name, avg_house_price, drug_rate_per_100k))

cat("\nDistricts used in model : ", nrow(model_data))

lm_model = lm(avg_house_price ~ drug_rate_per_100k, data = model_data)

cat("\n============================\n")
cat("LINEAR MODEL SUMMARY\n")
cat("============================\n")
print(summary(lm_model))

r_squared = summary(lm_model)$r.squared
p_value = summary(lm_model)$coefficients[2, 4]
slope = coef(lm_model)[2]

cat("\nR-squared : ", round(r_squared, 4))
cat("\nP-value (slope) : ", format.pval(p_value, digits = 4))
cat("\nSlope : ", round(slope, 2), "\n")

scatter_plot = ggplot(model_data, aes(x = drug_rate_per_100k, y = avg_house_price)) +
  geom_point(aes(color = county_name), size = 3.5, alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 0.9) +
  scale_y_continuous(labels = label_currency(prefix = "£", big.mark = ",")) +
  scale_color_manual(values = c("Norfolk" = "#2E86AB", "Suffolk" = "#E67E22")) +
  labs(
    title = "House Price vs Drug Offence Rate",
    subtitle = sprintf(
      "District-level average, %s | R² = %.3f, p = %s",
      paste(range(common_years), collapse = "-"),
      r_squared,
      format.pval(p_value, digits = 3)
    ),
    x = "Drug Offence Rate per 100,000 People",
    y = "Average House Price",
    color = "County"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

print(scatter_plot)

dir.create("D:/Data_Science/Charts", showWarnings = FALSE, recursive = TRUE)

ggsave(
  chart_path,
  plot = scatter_plot,
  width = 8,
  height = 6,
  dpi = 300
)

cat("\nChart saved to :", chart_path, "\n")
