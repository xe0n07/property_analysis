# =============================================================================
# EDA_11_Education_BoxPlot_Att8.R
# Box plot comparing average Attainment 8 scores, Norfolk vs Suffolk, for
# ONE chosen academic year (brief allows any year in 2021-2026)
#
# FIX vs. original version:
# The original script could only use a single 2024-2025 snapshot because
# fact_education held only one year. Run 10_Education_MultiYear_Loader.R
# first to rebuild fact_education with all downloaded DfE years - this
# script then simply filters to whichever year you choose below.
# =============================================================================

library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)

db_path    <- "D:/Data_Science/Clean Data/norfolk_suffolk.db"
chart_path <- "D:/Data_Science/Charts/11_Education_BoxPlot_Att8.png"

# ---- Choose the academic year to compare here ------------------------------
chosen_year <- "2024-2025"   # must match a value in fact_education$academic_year

con <- dbConnect(SQLite(), db_path)

education <- dbGetQuery(con, sprintf("
  SELECT e.estab, e.att8_score, e.academic_year, e.school_name, e.town, e.county_name
  FROM fact_education e
  WHERE e.att8_score IS NOT NULL
    AND e.academic_year = '%s'
", chosen_year))

dbDisconnect(con)

if (nrow(education) == 0) {
  stop(sprintf(
    "No rows found for academic_year = '%s'. Run 10_Education_MultiYear_Loader.R first, then check the exact year labels with:\n  SELECT DISTINCT academic_year FROM fact_education;",
    chosen_year
  ))
}

cat("\nSchools with valid Attainment 8 score in", chosen_year, ":", nrow(education), "\n")

att8_stats <- education %>%
  group_by(county_name) %>%
  summarise(
    mean_att8   = mean(att8_score, na.rm = TRUE),
    median_att8 = median(att8_score, na.rm = TRUE),
    sd_att8     = sd(att8_score, na.rm = TRUE),
    n_schools   = n(),
    .groups = "drop"
  )

cat("\n============================\n")
cat("ATTAINMENT 8 SCORE DISTRIBUTION -", chosen_year, "\n")
cat("============================\n")
print(att8_stats)

box_plot <- ggplot(education, aes(x = county_name, y = att8_score, fill = county_name)) +
  geom_boxplot(width = 0.5, outlier.alpha = 0.4) +
  geom_jitter(width = 0.08, alpha = 0.3, size = 1.5) +
  scale_fill_manual(values = c("Norfolk" = "#2E86AB", "Suffolk" = "#E67E22")) +
  labs(
    title = paste0("Attainment 8 Score Distribution - ", chosen_year),
    subtitle = "Norfolk vs Suffolk, school-level KS4 results",
    x = "County",
    y = "Attainment 8 Score",
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
