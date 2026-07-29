library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(scales)

db_path = "D:/Data_Science/Clean Data/norfolk_suffolk.db"
chart_path = "D:/Data_Science/Charts/03_HousePrice_LineChart.png"

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

yearly_avg = house_price %>%
  group_by(county_name, year) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(county_name, year)

cat("\n============================\n")
cat("AVERAGE HOUSE PRICE BY YEAR (DATA AVAILABLE: 2021-2025)\n")
cat("============================\n")
print(yearly_avg)

line_chart = ggplot(yearly_avg, aes(x = year, y = avg_price, color = county_name, group = county_name)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = unique(yearly_avg$year)) +
  scale_y_continuous(labels = label_currency(prefix = "£", scale = 1e-3, suffix = "k")) +
  scale_color_manual(values = c("Norfolk" = "#2E86AB", "Suffolk" = "#E67E22")) +
  labs(
    title = "Average House Price Trend by Year",
    subtitle = "Norfolk vs Suffolk (2021-2025 available in dataset)",
    x = "Year",
    y = "Average Price",
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
