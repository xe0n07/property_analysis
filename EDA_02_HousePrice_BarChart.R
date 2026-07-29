library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(scales)

db_path = "D:/Data_Science/Clean Data/norfolk_suffolk.db"
chart_path = "D:/Data_Science/Charts/02_HousePrice_BarChart.png"

con = dbConnect(SQLite(), db_path)

house_price = dbGetQuery(con, "
  SELECT hp.price, hp.year, c.county_name
  FROM fact_house_price hp
  JOIN dim_district d ON hp.district_id = d.district_id
  JOIN dim_county c ON d.county_id = c.county_id
")

dbDisconnect(con)

cat("\nYears available in house price data : ")
print(sort(unique(house_price$year)))

cat("\nNote: 2026 is a partial year in the raw dataset and is excluded from")
cat("\nthis combined average, so the figure reflects full calendar years only")
cat("\n(2021-2025).\n")

house_price_full_years = house_price %>%
  filter(year <= 2025)

avg_price = house_price_full_years %>%
  group_by(county_name) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

cat("\n============================\n")
cat("AVERAGE HOUSE PRICE BY COUNTY (2021-2025)\n")
cat("============================\n")
print(avg_price)

bar_chart = ggplot(avg_price, aes(x = county_name, y = avg_price, fill = county_name)) +
  geom_col(width = 0.5) +
  geom_text(
    aes(label = label_currency(prefix = "£", big.mark = ",")(round(avg_price))),
    vjust = -0.5,
    size = 4.5,
    fontface = "bold"
  ) +
  scale_y_continuous(
    labels = label_currency(prefix = "£", scale = 1e-3, suffix = "k"),
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_fill_manual(values = c("Norfolk" = "#2E86AB", "Suffolk" = "#E67E22")) +
  labs(
    title = "Average House Price by County",
    subtitle = "Combined average across 2021-2025 (full calendar years)",
    x = "County",
    y = "Average Price"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

print(bar_chart)

dir.create("D:/Data_Science/Charts", showWarnings = FALSE, recursive = TRUE)

ggsave(
  chart_path,
  plot = bar_chart,
  width = 7,
  height = 6,
  dpi = 300
)

cat("\nChart saved to :", chart_path, "\n")