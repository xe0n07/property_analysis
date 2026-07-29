library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)

db_path = "D:/Data_Science/Clean Data/norfolk_suffolk.db"
lookup_script = "D:/Data_Science/EDA 1/00_Postcode_Town_Lookup.R"
chart_path = "D:/Data_Science/Charts/LM_02_HousePrice_vs_DownloadSpeed.png"

source(lookup_script)

con = dbConnect(SQLite(), db_path)

house_price_raw = dbGetQuery(con, "
  SELECT hp.postcode, hp.price, cn.county_name
  FROM fact_house_price hp
  JOIN dim_district d ON hp.district_id = d.district_id
  JOIN dim_county cn ON d.county_id = cn.county_id
")

broadband_raw = dbGetQuery(con, "
  SELECT bb.postcode, bb.avg_speed, cn.county_name
  FROM fact_broadband bb
  JOIN dim_county cn ON bb.county_id = cn.county_id
")

dbDisconnect(con)

extract_district = function(postcode) {
  str_extract(str_to_upper(postcode), "^[A-Z]{1,2}[0-9]{1,2}")
}

house_price = house_price_raw %>%
  mutate(district = extract_district(postcode)) %>%
  left_join(postcode_town_lookup, by = "district")

broadband = broadband_raw %>%
  mutate(district = extract_district(postcode)) %>%
  left_join(postcode_town_lookup, by = "district")

cat("\nHouse price rows matched to a town : ", sum(!is.na(house_price$town)), "of", nrow(house_price))
cat("\nBroadband rows matched to a town   : ", sum(!is.na(broadband$town)), "of", nrow(broadband))

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

town_broadband = broadband %>%
  filter(!is.na(town)) %>%
  group_by(town) %>%
  summarise(
    avg_download_speed = mean(avg_speed, na.rm = TRUE),
    bb_n = n(),
    .groups = "drop"
  )

model_data = town_house_price %>%
  inner_join(town_broadband, by = "town") %>%
  left_join(town_county, by = "town")

cat("\n============================\n")
cat("HOUSE PRICE vs DOWNLOAD SPEED - TOWN LEVEL MODEL DATA\n")
cat("============================\n")
print(model_data %>% select(town, county_name, avg_house_price, avg_download_speed, hp_n, bb_n))

cat("\nTowns used in model : ", nrow(model_data))

lm_model = lm(avg_house_price ~ avg_download_speed, data = model_data)

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

scatter_plot = ggplot(model_data, aes(x = avg_download_speed, y = avg_house_price)) +
  geom_point(aes(color = county_name), size = 3.5, alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 0.9) +
  scale_y_continuous(labels = label_currency(prefix = "£", big.mark = ",")) +
  scale_color_manual(values = c("Norfolk" = "#2E86AB", "Suffolk" = "#E67E22")) +
  labs(
    title = "House Price vs Download Speed",
    subtitle = sprintf(
      "Town-level average (n = %d towns) | R² = %.3f, p = %s",
      nrow(model_data),
      r_squared,
      format.pval(p_value, digits = 3)
    ),
    x = "Average Download Speed (Mbit/s)",
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
