# =============================================================================
# EDA_05_Broadband_BoxPlot_Suffolk.R
# Box Plot of average download speed in the towns of Suffolk
#
# FIX vs. original version:
# Same issue as the Norfolk script - raw IP postcode districts (IP28, IP32,
# ...) were plotted as a "Town Proxy" instead of real towns. This version
# joins to real Royal Mail post towns via 00_Postcode_Town_Lookup.R
# (Ipswich, Felixstowe, Bury St Edmunds, Lowestoft, Stowmarket, ...).
# =============================================================================

library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
library(ggplot2)
library(forcats)

db_path    <- "D:/Data_Science/Clean Data/norfolk_suffolk.db"
chart_path <- "D:/Data_Science/Charts/05_Broadband_BoxPlot_Suffolk.png"

source("D:/Data_Science/EDA 1/00_Postcode_Town_Lookup.R")   # -> postcode_town_lookup

con <- dbConnect(SQLite(), db_path)

broadband <- dbGetQuery(con, "
  SELECT bb.postcode, bb.avg_speed, c.county_name
  FROM fact_broadband bb
  JOIN dim_county c ON bb.county_id = c.county_id
  WHERE c.county_name = 'Suffolk'
")

dbDisconnect(con)

cat("\nSuffolk broadband rows : ", nrow(broadband))

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
cat("\nSuffolk towns available after mapping :", nrow(town_counts), "\n")
print(town_counts)

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
cat("AVERAGE DOWNLOAD SPEED BY TOWN - SUFFOLK\n")
cat("============================\n")
print(town_stats)

# NOTE: Ofcom fixed-postcode speed data occasionally contains a small number
# of extreme outliers (e.g. leased-line / business circuits reporting
# 500-900+ Mbit/s at a single postcode). These are real recorded values,
# not data errors, but they can compress the box plot. If your report needs
# a chart focused on typical residential speeds, consider adding
# coord_cartesian(ylim = c(0, 150)) below (this clips the *view* only and
# does not delete rows, unlike filtering/removing outliers from plot_data).
box_plot <- ggplot(plot_data, aes(x = town, y = avg_speed)) +
  geom_boxplot(fill = "#E67E22", outlier.alpha = 0.3, width = 0.6) +
  coord_flip() +
  labs(
    title = "Average Broadband Download Speed Distribution in Suffolk Towns",
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
