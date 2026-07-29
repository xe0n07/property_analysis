library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(ggplot2)

db_path = "D:/Data_Science/Clean Data/norfolk_suffolk.db"
chart_path = "D:/Data_Science/Charts/06_Broadband_StatBarChart.png"

con = dbConnect(SQLite(), db_path)

broadband = dbGetQuery(con, "
  SELECT bb.avg_speed, bb.max_speed, c.county_name
  FROM fact_broadband bb
  JOIN dim_county c ON bb.county_id = c.county_id
")

dbDisconnect(con)

speed_stats = broadband %>%
  group_by(county_name) %>%
  summarise(
    `Average Speed` = mean(avg_speed, na.rm = TRUE),
    `Maximum Speed` = mean(max_speed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(`Average Speed`, `Maximum Speed`),
    names_to = "speed_type",
    values_to = "speed_value"
  )

cat("\n============================\n")
cat("BROADBAND SPEED SUMMARY BY COUNTY\n")
cat("============================\n")
print(speed_stats)

bar_chart = ggplot(speed_stats, aes(x = county_name, y = speed_value, fill = speed_type)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(label = round(speed_value, 1)),
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 4,
    fontface = "bold"
  ) +
  scale_fill_manual(values = c("Average Speed" = "#2E86AB", "Maximum Speed" = "#A23B72")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Average vs Maximum Download Speed by County",
    x = "County",
    y = "Download Speed (Mbit/s)",
    fill = "Speed Type"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
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
