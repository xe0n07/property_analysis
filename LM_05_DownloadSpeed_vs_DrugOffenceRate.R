library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)

db_path = "D:/Data_Science/Clean Data/norfolk_suffolk.db"
chart_path = "D:/Data_Science/Charts/LM_05_DownloadSpeed_vs_DrugOffenceRate.png"

# NOTE ON GEOGRAPHY: crime data only has LSOA, which maps to administrative
# DISTRICT (e.g. Breckland, Babergh) - it has no postcode field, so it
# cannot be joined to the postcode -> TOWN lookup used in the other four
# linear models. A verified town -> district mapping was not available for
# this analysis, so rather than risk an unverified or approximate bridge,
# this model aggregates download speed to COUNTY level (Norfolk / Suffolk)
# and drug offence rate to DISTRICT level (using the same LSOA-to-district
# logic as the crime EDA scripts). Every district within a county is
# therefore assigned that county's single average download speed. This
# means the vertical spread in the chart reflects only two values, and
# the apparent trend line is driven by the Norfolk/Suffolk split, not a
# genuine district-by-district relationship. This limitation is stated
# again in the chart subtitle and console output below.

con = dbConnect(SQLite(), db_path)

cat("\n============================\n")
cat("STEP 1: DRUG OFFENCE RATE BY DISTRICT\n")
cat("============================\n")

drug_crime = dbGetQuery(con, "
  SELECT c.lsoa_code, l.lsoa_name, cn.county_name
  FROM fact_crime c
  JOIN dim_lsoa l ON c.lsoa_code = l.lsoa_code
  JOIN dim_county cn ON l.county_id = cn.county_id
  WHERE c.crime_type = 'Drugs'
    AND c.in_boundary = 1
")

population_by_lsoa = dbGetQuery(con, "SELECT lsoa_code, population FROM fact_population")

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

cat("\nDrug offence rate calculated for", nrow(drug_by_district), "districts\n")
print(drug_by_district %>% select(district_name, county_name, drug_rate_per_100k))

cat("\n============================\n")
cat("STEP 2: AVERAGE DOWNLOAD SPEED BY COUNTY (DISTRICT NOT AVAILABLE)\n")
cat("============================\n")
cat("\nBroadband data has no reliable link to administrative district (only\n")
cat("postcode -> town). Since a town cannot be safely assigned to a single\n")
cat("district without a verified lookup, download speed is aggregated to\n")
cat("COUNTY level here and applied uniformly to every district within that\n")
cat("county.\n")

broadband = dbGetQuery(con, "
  SELECT bb.avg_speed, cn.county_name
  FROM fact_broadband bb
  JOIN dim_county cn ON bb.county_id = cn.county_id
")

dbDisconnect(con)

county_broadband = broadband %>%
  group_by(county_name) %>%
  summarise(
    avg_download_speed = mean(avg_speed, na.rm = TRUE),
    n_postcodes = n(),
    .groups = "drop"
  )

cat("\n")
print(county_broadband)

model_data = drug_by_district %>%
  left_join(county_broadband, by = "county_name")

cat("\n============================\n")
cat("DOWNLOAD SPEED vs DRUG OFFENCE RATE - DISTRICT LEVEL MODEL DATA\n")
cat("============================\n")
print(model_data %>% select(district_name, county_name, avg_download_speed, drug_rate_per_100k))

cat("\nDistricts used in model : ", nrow(model_data))

lm_model = lm(avg_download_speed ~ drug_rate_per_100k, data = model_data)

cat("\n============================\n")
cat("LINEAR MODEL SUMMARY\n")
cat("============================\n")
print(summary(lm_model))

r_squared = summary(lm_model)$r.squared
p_value = summary(lm_model)$coefficients[2, 4]
slope = coef(lm_model)[2]

cat("\nR-squared : ", round(r_squared, 4))
cat("\nP-value (slope) : ", format.pval(p_value, digits = 4))
cat("\nSlope : ", round(slope, 6), "\n")
cat("\nCaution: with only two distinct download speed values (one per county)\n")
cat("spread across", nrow(model_data), "districts, this R-squared largely reflects\n")
cat("the county split rather than a genuine district-level relationship.\n")

scatter_plot = ggplot(model_data, aes(x = drug_rate_per_100k, y = avg_download_speed)) +
  geom_point(aes(color = county_name), size = 3.5, alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 0.9) +
  scale_color_manual(values = c("Norfolk" = "#2E86AB", "Suffolk" = "#E67E22")) +
  labs(
    title = "Average Download Speed vs Drug Offence Rate",
    subtitle = sprintf(
      "District-level (n = %d) | R² = %.3f, p = %s | Download speed is county-level, not district-level",
      nrow(model_data),
      r_squared,
      format.pval(p_value, digits = 3)
    ),
    x = "Drug Offence Rate per 100,000 People",
    y = "Average Download Speed (Mbit/s)",
    color = "County"
  ) +
  theme_minimal(base_size = 12) +
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
