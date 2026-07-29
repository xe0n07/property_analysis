# =============================================================================
# EDA_08_Crime_Radar_VehicleCrime.R
# Radar chart: Vehicle Crime rate per 100,000 people, Norfolk vs Suffolk,
# 12-month window (May start_year - April start_year + 1)
#
# FIX vs. previous version - two separate bugs were stacked on top of each
# other, which is why this kept failing in different ways:
#
# BUG 1 - crime_date is NOT stored as text.
#   Despite the build script calling format(crime_date, "%Y-%m-%d"), the
#   column ends up in SQLite as REAL: R's native Date representation (days
#   since 1970-01-01). Calling SQLite's strftime()/date() directly on that
#   column does NOT work, because SQLite interprets a bare number as a
#   Julian Day count from 4714 BC, not an R-epoch day count. That is exactly
#   why earlier runs printed dates like "-4658-01" and why date-range
#   filtering done inside SQL returned 0 rows.
#   FIX: pull crime_date out as-is and convert it in R with
#   as.Date(crime_date, origin = "1970-01-01") - NOT "1899-12-30" (that's
#   Excel's epoch, and it's what produced the wrong dates / 0 rows in the
#   second-to-last attempt). The "year" column already in fact_crime is
#   reliable on its own, so we use it directly instead of re-deriving year
#   from crime_date.
#
# BUG 2 - radar_data was a matrix, not a data frame.
#   radar_data <- rbind(rep(100,12), rep(0,12), norfolk_pct, suffolk_pct)
#   rbind() of plain numeric vectors returns a matrix. fmsb::radarchart()
#   requires a data.frame - with a matrix it just prints
#   "The data must be given as dataframe." and returns NULL *without*
#   calling plot.new(), which is why the following legend() call then threw
#   "plot.new has not been called yet".
#   FIX: wrap the row-bound object in as.data.frame().
#
# Values were checked against the actual norfolk_suffolk.db to confirm the
# corrected logic returns real, non-zero, month-varying rates.
# =============================================================================

library(DBI)
library(RSQLite)
library(dplyr)
library(lubridate)
library(fmsb)
library(scales)

db_path    <- "D:/Data_Science/Clean Data/norfolk_suffolk.db"
chart_path <- "D:/Data_Science/Charts/08_Crime_Radar_VehicleCrime.png"

# ---- Choose the 12-month window here (May start_year -> April start_year+1)
start_year <- 2025

month_numbers <- c(5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3, 4)
month_names   <- c("May", "Jun", "Jul", "Aug", "Sep", "Oct",
                   "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")

con <- dbConnect(SQLite(), db_path)

# ---- Population per county (denominator for the rate) ----------------------
population <- dbGetQuery(con, "
  SELECT
    cn.county_name,
    SUM(p.population) AS total_population
  FROM fact_population p
  JOIN dim_lsoa l  ON p.lsoa_code = l.lsoa_code
  JOIN dim_county cn ON l.county_id = cn.county_id
  GROUP BY cn.county_name
")

# ---- All Vehicle Crime rows, with the raw (numeric) crime_date and the -----
# ---- already-reliable 'year' column pulled straight from fact_crime -------
# NOTE: deliberately NOT using strftime() on crime_date in SQL - see BUG 1.
vehicle_crime <- dbGetQuery(con, "
  SELECT
    c.crime_date,
    c.year,
    cn.county_name
  FROM fact_crime c
  JOIN dim_lsoa l  ON c.lsoa_code = l.lsoa_code
  JOIN dim_county cn ON l.county_id = cn.county_id
  WHERE c.crime_type = 'Vehicle Crime'
")

dbDisconnect(con)

cat("\nVehicle crime rows pulled :", nrow(vehicle_crime), "\n")

if (nrow(vehicle_crime) == 0) {
  stop("No Vehicle Crime rows were returned - check crime_type spelling / table contents.")
}

# ---- Correct date conversion: R-epoch REAL -> Date -------------------------
vehicle_crime <- vehicle_crime %>%
  mutate(
    crime_date = as.Date(crime_date, origin = "1970-01-01"),
    month      = month(crime_date)
  )

# ---- Keep only the chosen 12-month window (May start_year - Apr start_year+1)
vehicle_crime <- vehicle_crime %>%
  filter(
    (year == start_year     & month >= 5) |
      (year == start_year + 1 & month <= 4)
  )

cat("Rows in window May", start_year, "- April", start_year + 1, ":", nrow(vehicle_crime), "\n")

if (nrow(vehicle_crime) == 0) {
  stop("No Vehicle Crime rows fall inside the chosen 12-month window - try a different start_year.")
}

# ---- Monthly count -> rate per 100,000 people, by county -------------------
vehicle_crime_rate <- vehicle_crime %>%
  count(county_name, month, name = "crime_count") %>%
  left_join(population, by = "county_name") %>%
  mutate(rate_per_100k = (crime_count / total_population) * 100000)

# ---- Build one row per county, columns in May -> April order, filling any --
# ---- missing county/month combination with a 0 rate ------------------------
norfolk_values <- sapply(month_numbers, function(m) {
  v <- vehicle_crime_rate$rate_per_100k[
    vehicle_crime_rate$county_name == "Norfolk" & vehicle_crime_rate$month == m
  ]
  if (length(v) == 0) 0 else v
})

suffolk_values <- sapply(month_numbers, function(m) {
  v <- vehicle_crime_rate$rate_per_100k[
    vehicle_crime_rate$county_name == "Suffolk" & vehicle_crime_rate$month == m
  ]
  if (length(v) == 0) 0 else v
})

cat("\n============================\n")
cat("VEHICLE CRIME RATE PER 100,000 (MAY", start_year, "- APRIL", start_year + 1, ")\n")
cat("============================\n")
print(data.frame(
  month         = month_names,
  norfolk_rate  = round(norfolk_values, 2),
  suffolk_rate  = round(suffolk_values, 2)
))

# ---- Express each county's rate as a % of the overall max rate, to match --
# ---- the 0-100% radial scale used in the reference chart -------------------
maximum_value <- max(c(norfolk_values, suffolk_values), na.rm = TRUE)
if (maximum_value <= 0) maximum_value <- 1

norfolk_percentage <- round((norfolk_values / maximum_value) * 100, 2)
suffolk_percentage <- round((suffolk_values / maximum_value) * 100, 2)

# ---- FIX for BUG 2: wrap in as.data.frame() so fmsb accepts it -------------
radar_data <- as.data.frame(rbind(
  rep(100, 12),
  rep(0, 12),
  norfolk_percentage,
  suffolk_percentage
))
colnames(radar_data) <- month_names
rownames(radar_data) <- c("max", "min", "Norfolk", "Suffolk")

dir.create("D:/Data_Science/Charts", recursive = TRUE, showWarnings = FALSE)

png(
  filename = chart_path,
  width  = 3600,
  height = 3200,
  res    = 400
)

par(mar = c(7, 4, 5, 4))  # extra bottom margin so the legend has room below "Nov"

radarchart(
  radar_data,
  axistype   = 1,
  pcol       = c("#1F77B4", "#FF7F0E"),
  pfcol      = alpha(c("#1F77B4", "#FF7F0E"), 0.25),
  plwd       = 3,
  plty       = 1,
  cglcol     = "grey75",
  cglty      = 1,
  cglwd      = 1,
  vlcex      = 1.1,
  axislabcol = "black",
  caxislabels = c("0%", "25%", "50%", "75%", "100%"),
  title = paste0(
    "Vehicle Crime Rate per 100,000 Population\n",
    "May ", start_year, " - April ", start_year + 1
  )
)

legend(
  "bottom",
  inset  = -0.18,   # negative inset pushes the legend below the plot region,
  # clear of the "Nov" axis label
  xpd    = TRUE,     # allow drawing outside the plot region
  legend = c("NORFOLK", "SUFFOLK"),
  col    = c("#1F77B4", "#FF7F0E"),
  lwd    = 3,
  cex    = 1.1,
  horiz  = TRUE,
  bty    = "n"
)

dev.off()

cat("\nChart saved to :", chart_path, "\n")