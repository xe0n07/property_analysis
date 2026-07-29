# =============================================================================
# EDA_04_Broadband_BoxPlot_Norfolk.R
# Box Plot of average download speed in the towns of Norfolk
#
# FIX vs. original version:
# The original script extracted the raw postcode district (e.g. "NR13") with
# str_extract() and plotted ~35 of those districts under the label
# "Town Proxy" - not real towns, and far too many categories to read.
# This version joins each postcode to its real Royal Mail post town using
# 00_Postcode_Town_Lookup.R, then plots one box per genuine town, matching
# the structure of the reference chart (Dereham, Great Yarmouth, King's
# Lynn, Norwich, Thetford, ... one box each).
# =============================================================================

library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
library(ggplot2)
library(forcats)

db_path    <- "D:/Data_Science/Clean Data/norfolk_suffolk.db"
chart_path <- "D:/Data_Science/Charts/04_Broadband_BoxPlot_Norfolk.png"

source("D:/Data_Science/EDA 1/00_Postcode_Town_Lookup.R")   # -> postcode_town_lookup

con <- dbConnect(SQLite(), db_path)

broadband <- dbGetQuery(con, "
  SELECT bb.postcode, bb.avg_speed, c.county_name
  FROM fact_broadband bb
  JOIN dim_county c ON bb.county_id = c.county_id
  WHERE c.county_name = 'Norfolk'
")

dbDisconnect(con)

cat("\nNorfolk broadband rows : ", nrow(broadband))

# ---- Map postcode -> real town (not a postcode-district proxy) ------------
broadband <- broadband %>%
  mutate(
    postcode = toupper(str_trim(postcode)),
    district = str_extract(postcode, "^[A-Z]{1,2}[0-9]{1,2}")
  ) %>%
  filter(!is.na(district)) %>%
  left_join(postcode_town_lookup, by = "district")

unmatched <- broadband %>% filter(is.na(town)) %>% distinct(district)
if (nrow(unmatched) > 0) {
  cat("\nWARNING - districts with no town match (check lookup table):\n")
  print(unmatched)
}

broadband <- broadband %>% filter(!is.na(town))

town_counts <- broadband %>% count(town, sort = TRUE)
cat("\nNorfolk towns available after mapping :", nrow(town_counts), "\n")
print(town_counts)

# Keep towns with a reasonable sample size, same "min 20 postcodes" rule as
# the original script, just applied to real towns instead of postcode codes
min_postcodes <- 20
keep_towns <- town_counts %>% filter(n >= min_postcodes) %>% pull(town)

plot_data <- broadband %>%
  filter(town %in% keep_towns) %>%
  mutate(town = fct_reorder(town, avg_speed, .fun = median))

cat("\nTowns retained (min", min_postcodes, "postcodes each) :", length(keep_towns))
cat("\nRows used for plot :", nrow(plot_data), "\n")

# ---- Summary statistics (mean / median / SD) for the report text ----------
town_stats <- plot_data %>%
  group_by(town) %>%
  summarise(
    mean_speed   = mean(avg_speed, na.rm = TRUE),
    median_speed = median(avg_speed, na.rm = TRUE),
    sd_speed     = sd(avg_speed, na.rm = TRUE),
    n_postcodes  = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(median_speed))

cat("\n============================\n")
cat("AVERAGE DOWNLOAD SPEED BY TOWN - NORFOLK\n")
cat("============================\n")
print(town_stats)

box_plot <- ggplot(plot_data, aes(x = town, y = avg_speed)) +
  geom_boxplot(fill = "#2E86AB", outlier.alpha = 0.3, width = 0.6) +
  coord_flip() +
  labs(
    title = "Average Broadband Download Speed Distribution in Norfolk Towns",
    subtitle = "Postcodes mapped to real post towns (min 20 postcodes per town)",
    x = "Town",
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
  height = 7,
  dpi = 300
)

cat("\nChart saved to :", chart_path, "\n")
