library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
library(ggplot2)
library(forcats)

db_path = "D:/Data_Science/Clean Data/norfolk_suffolk.db"
chart_path = "D:/Data_Science/Charts/04_Broadband_BoxPlot_Norfolk.png"

con = dbConnect(SQLite(), db_path)

broadband = dbGetQuery(con, "
  SELECT bb.postcode, bb.avg_speed, c.county_name
  FROM fact_broadband bb
  JOIN dim_county c ON bb.county_id = c.county_id
  WHERE c.county_name = 'Norfolk'
")

dbDisconnect(con)

cat("\nNorfolk broadband rows : ", nrow(broadband))

broadband = broadband %>%
  mutate(
    town_code = str_extract(postcode, "^[A-Z]{1,2}[0-9]{1,2}")
  ) %>%
  filter(!is.na(town_code))

town_counts = broadband %>%
  count(town_code, sort = TRUE)

cat("\nDistinct Norfolk postcode-district groups (used as 'town') : ", nrow(town_counts))

top_towns = town_counts %>%
  filter(n >= 20) %>%
  pull(town_code)

plot_data = broadband %>%
  filter(town_code %in% top_towns) %>%
  mutate(town_code = fct_reorder(town_code, avg_speed, .fun = median))

cat("\nTowns retained (min 20 postcodes each) : ", length(top_towns))
cat("\nRows used for plot : ", nrow(plot_data))

box_plot = ggplot(plot_data, aes(x = town_code, y = avg_speed)) +
  geom_boxplot(fill = "#2E86AB", outlier.alpha = 0.3, width = 0.6) +
  coord_flip() +
  labs(
    title = "Average Download Speed by Town - Norfolk",
    subtitle = "Grouped by postcode district (e.g. NR13, NR4). Minimum 20 postcodes per group.",
    x = "Postcode District (Town Proxy)",
    y = "Average Download Speed (Mbit/s)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold")
  )

print(box_plot)

dir.create("D:/Data_Science/Charts", showWarnings = FALSE, recursive = TRUE)

ggsave(
  chart_path,
  plot = box_plot,
  width = 8,
  height = 10,
  dpi = 300
)

cat("\nChart saved to :", chart_path, "\n")
