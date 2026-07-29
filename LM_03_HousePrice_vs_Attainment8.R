library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)

db_path = "D:/Data_Science/Clean Data/norfolk_suffolk.db"
lookup_script = "D:/Data_Science/EDA 1/00_Postcode_Town_Lookup.R"
chart_path = "D:/Data_Science/Charts/LM_03_HousePrice_vs_Attainment8.png"

source(lookup_script)

con = dbConnect(SQLite(), db_path)

house_price_raw = dbGetQuery(con, "
  SELECT hp.postcode, hp.price, cn.county_name
  FROM fact_house_price hp
  JOIN dim_district d ON hp.district_id = d.district_id
  JOIN dim_county cn ON d.county_id = cn.county_id
")

education_raw = dbGetQuery(con, "
  SELECT e.att8_score, s.postcode
  FROM fact_education e
  JOIN dim_school s ON e.estab = s.estab
  WHERE e.att8_score IS NOT NULL
")

dbDisconnect(con)

extract_district = function(postcode) {
  str_extract(str_to_upper(postcode), "^[A-Z]{1,2}[0-9]{1,2}")
}

house_price = house_price_raw %>%
  mutate(district = extract_district(postcode)) %>%
  left_join(postcode_town_lookup, by = "district")

education = education_raw %>%
  mutate(district = extract_district(postcode)) %>%
  left_join(postcode_town_lookup, by = "district")

cat("\nHouse price rows matched to a town : ", sum(!is.na(house_price$town)), "of", nrow(house_price))
cat("\nEducation rows matched to a town   : ", sum(!is.na(education$town)), "of", nrow(education))

town_house_price = house_price %>%
  filter(!is.na(town)) %>%
  group_by(town) %>%
  summarise(
    avg_house_price = mean(price, na.rm = TRUE),
    hp_n = n(),
    .groups = "drop"
  )

town_county = house_price %>%
  filter(!is.na(town)) %>%
  count(town, county_name) %>%
  group_by(town) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(town, county_name)

town_education = education %>%
  filter(!is.na(town)) %>%
  group_by(town) %>%
  summarise(
    avg_att8 = mean(att8_score, na.rm = TRUE),
    edu_n = n(),
    .groups = "drop"
  )

model_data = town_house_price %>%
  inner_join(town_education, by = "town") %>%
  left_join(town_county, by = "town")

cat("\n============================\n")
cat("HOUSE PRICE vs ATTAINMENT 8 SCORE - TOWN LEVEL MODEL DATA\n")
cat("============================\n")
print(model_data %>% select(town, county_name, avg_house_price, avg_att8, hp_n, edu_n))

cat("\nTowns used in model : ", nrow(model_data))
cat("\nNote: education data is sparse (small number of schools per town),")
cat("\nso town-level Attainment 8 averages may be based on very few schools.\n")

lm_model = lm(avg_house_price ~ avg_att8, data = model_data)

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

scatter_plot = ggplot(model_data, aes(x = avg_att8, y = avg_house_price)) +
  geom_point(aes(color = county_name, size = edu_n), alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 0.9) +
  scale_y_continuous(labels = label_currency(prefix = "£", big.mark = ",")) +
  scale_color_manual(values = c("Norfolk" = "#2E86AB", "Suffolk" = "#E67E22")) +
  scale_size_continuous(range = c(2, 6), name = "Schools\nin town") +
  labs(
    title = "House Price vs Average Attainment 8 Score",
    subtitle = sprintf(
      "Town-level average (n = %d towns) | R² = %.3f, p = %s",
      nrow(model_data),
      r_squared,
      format.pval(p_value, digits = 3)
    ),
    x = "Average Attainment 8 Score",
    y = "Average House Price",
    color = "County"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

print(scatter_plot)

dir.create("D:/Data_Science/Charts", showWarnings = FALSE, recursive = TRUE)

ggsave(
  chart_path,
  plot = scatter_plot,
  width = 9,
  height = 6,
  dpi = 300
)

cat("\nChart saved to :", chart_path, "\n")
