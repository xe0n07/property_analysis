library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(scales)

db_path = "D:/Data_Science/Clean Data/norfolk_suffolk.db"
chart_path = "D:/Data_Science/Charts/01_HousePrice_BoxPlot.png"

con = dbConnect(SQLite(), db_path)

chosen_year = 2025

house_price = dbGetQuery(con, "
  SELECT hp.price, hp.year, d.county_id, c.county_name
  FROM fact_house_price hp
  JOIN dim_district d ON hp.district_id = d.district_id
  JOIN dim_county c ON d.county_id = c.county_id
")

dbDisconnect(con)

cat("\nYears available in house price data : ")
print(sort(unique(house_price$year)))

cat("\nNote: 2026 is a partial year in the raw dataset, so it is excluded")
cat("\nfrom this single-year comparison. 2025 is used as the latest full year.\n")

house_price_year = house_price %>%
  filter(year == chosen_year)

cat("\nRows used for chosen year", chosen_year, ": ", nrow(house_price_year))

price_stats = house_price_year %>%
  group_by(county_name) %>%
  summarise(
    mean_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    sd_price = sd(price, na.rm = TRUE),
    min_price = min(price, na.rm = TRUE),
    max_price = max(price, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

cat("\n============================\n")
cat("HOUSE PRICE DISTRIBUTION -", chosen_year, "\n")
cat("============================\n")
print(price_stats)

plot_data = house_price_year %>%
  filter(
    price > 0,
    price < quantile(price, 0.99, na.rm = TRUE)
  )

cat("\nRows after trimming top 1% outliers : ", nrow(plot_data))

box_plot = ggplot(plot_data, aes(x = county_name, y = price, fill = county_name)) +
  geom_boxplot(outlier.alpha = 0.3, width = 0.5) +
  scale_y_continuous(labels = label_currency(prefix = "£", scale = 1e-3, suffix = "k")) +
  scale_fill_manual(values = c("Norfolk" = "#2E86AB", "Suffolk" = "#E67E22")) +
  labs(
    title = paste("House Price Distribution -", chosen_year),
    subtitle = "Norfolk vs Suffolk (top 1% outliers trimmed for readability)",
    x = "County",
    y = "Price",
    fill = "County"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

print(box_plot)

dir.create("D:/Data_Science/Charts", showWarnings = FALSE, recursive = TRUE)

ggsave(
  chart_path,
  plot = box_plot,
  width = 8,
  height = 6,
  dpi = 300
)

cat("\nChart saved to :", chart_path, "\n")