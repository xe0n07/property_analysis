# =============================================================================
# EDA_07_Crime_BoxPlot_OneCrime_OneYear.R
# Box plot of one chosen crime type, one chosen year (2024 or 2025),
# Norfolk vs Suffolk
#
# NOTE: this script's underlying logic was already correct in your original
# version (monthly counts aggregated by county, one dot per month, boxed by
# county) - it is reproduced here mostly unchanged, with clearer comments
# and consistent styling with the rest of the EDA set.
# =============================================================================

library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)

db_path    <- "D:/Data_Science/Clean Data/norfolk_suffolk.db"
chart_path <- "D:/Data_Science/Charts/07_Crime_BoxPlot_OneCrime_OneYear.png"

# ---- Choose crime type and year here ---------------------------------------
chosen_crime <- "Violence And Sexual Offences"   # change to any crime_type value in fact_crime
chosen_year  <- 2024                             # must be 2024 or 2025 per brief

con <- dbConnect(SQLite(), db_path)

crime_monthly <- dbGetQuery(con, sprintf("
  SELECT
    strftime('%%Y-%%m', c.crime_date) AS year_month,
    l.county_id,
    cn.county_name,
    COUNT(*) AS crime_count
  FROM fact_crime c
  JOIN dim_lsoa l ON c.lsoa_code = l.lsoa_code
  JOIN dim_county cn ON l.county_id = cn.county_id
  WHERE c.crime_type = '%s'
    AND c.year = %d
  GROUP BY year_month, l.county_id, cn.county_name
", chosen_crime, chosen_year))

dbDisconnect(con)

cat("\n============================\n")
cat("CRIME TYPE :", chosen_crime, "| YEAR :", chosen_year, "\n")
cat("============================\n")
cat("\nMonthly crime-count rows retrieved :", nrow(crime_monthly), "\n")
print(crime_monthly)

# ---- Summary statistics for the report -------------------------------------
crime_stats <- crime_monthly %>%
  group_by(county_name) %>%
  summarise(
    mean_count   = mean(crime_count, na.rm = TRUE),
    median_count = median(crime_count, na.rm = TRUE),
    sd_count     = sd(crime_count, na.rm = TRUE),
    n_months     = n(),
    .groups = "drop"
  )

cat("\n============================\n")
cat(chosen_crime, "-", chosen_year, "MONTHLY COUNT STATISTICS\n")
cat("============================\n")
print(crime_stats)

box_plot <- ggplot(crime_monthly, aes(x = county_name, y = crime_count, fill = county_name)) +
  geom_boxplot(width = 0.5, outlier.alpha = 0.5) +
  geom_jitter(width = 0.08, alpha = 0.5, size = 2) +
  scale_fill_manual(values = c("Norfolk" = "#2E86AB", "Suffolk" = "#E67E22")) +
  labs(
    title = paste0(chosen_crime, " - Monthly Counts (", chosen_year, ")"),
    subtitle = "Distribution of monthly crime counts across LSOAs, Norfolk vs Suffolk",
    x = "County",
    y = "Crimes Recorded Per Month",
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
  width = 7,
  height = 6,
  dpi = 300
)

cat("\nChart saved to :", chart_path, "\n")
