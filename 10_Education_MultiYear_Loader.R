# =============================================================================
# 10_Education_MultiYear_Loader.R
#
# PURPOSE
# -------
# Your original fact_education table was loaded from a SINGLE DfE Key Stage 4
# file (2024/25 only) - confirmed by the comments in your own EDA_11/12/13
# scripts: "education_clean.csv is a single 2024-2025 KS4 snapshot... no
# multi-year Attainment 8 history is available." That is why your Attainment
# 8 box plot showed only one year and your "line chart" had to become a bar
# chart - a line needs >= 2 points on the x-axis and you only had one.
#
# Your friend's Figures 4.11 / 4.12 show 2021-2025 (four academic years),
# which means they loaded FOUR separate DfE "Key stage 4 performance"
# releases (one CSV/zip per academic year: 2021/22, 2022/23, 2023/24,
# 2024/25) and stacked them into one long-format table with a year column,
# instead of loading only the latest year.
#
# This script is that fix. It does NOT invent data - you said you already
# have the multiple yearly DfE KS4 files downloaded. Point YEAR_FILES below
# at your actual files and run this once to rebuild fact_education properly
# before re-running EDA_11 / EDA_12 / EDA_13.
#
# WHERE TO GET THE FILES (if you need to re-download any year)
# ---------------------------------------------------------------------------
# DfE "Key stage 4 performance", Explore Education Statistics, one release
# per academic year. Use the "institution level" / school-level underlying
# data file from each release's "Download all data (ZIP)" link:
#   2021/22 : https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-4-performance/2021-22
#   2022/23 : https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-4-performance/2022-23
#   2023/24 : https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-4-performance/2023-24
#   2024/25 : https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-4-performance/2024-25
# Filter each file to LEA (local authority) = Norfolk or Suffolk, or to your
# chosen list of main towns, before/after loading - see NOTE below.
#
# WHAT THIS SCRIPT DOES
# ---------------------------------------------------------------------------
# 1. Reads each year's school-level KS4 CSV (column names vary slightly
#    release to release - the loader auto-detects the common DfE variants).
# 2. Standardises to: estab, school_name, town, la_name, att8_score,
#    academic_year (e.g. "2021-2022"), county_name.
# 3. Row-binds all years into one long table -> writes fact_education back
#    into your SQLite database, replacing the single-year table.
#
# You must still map each school to a county (Norfolk / Suffolk) and, if you
# want town-wise charts, to a real town - the DfE files give a "town" /
# address field and a "new_la_name" (local authority) field which you can
# join to dim_county the same way your other fact tables do.
# =============================================================================

library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(DBI)
library(RSQLite)

db_path <- "D:/Data_Science/Clean Data/norfolk_suffolk.db"

# ---- 1. Point this at your downloaded DfE KS4 school-level files ----------
# One file per academic year. Update the paths to match where you saved them.
YEAR_FILES <- c(
  "2021-2022" = "D:/Data_Science/Raw Data/ks4_2021_22_institution.csv",
  "2022-2023" = "D:/Data_Science/Raw Data/ks4_2022_23_institution.csv",
  "2023-2024" = "D:/Data_Science/Raw Data/ks4_2023_24_institution.csv",
  "2024-2025" = "D:/Data_Science/Raw Data/ks4_2024_25_institution.csv"
)

# ---- 2. Column-name auto-detection -----------------------------------------
# DfE has used slightly different header names across releases
# (e.g. ATT8SCR vs AVG_ATT8 vs P8MEA-style prefixes). This helper picks
# whichever of the known aliases is present in a given year's file.
pick_col <- function(df_names, candidates) {
  hit <- candidates[candidates %in% df_names]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

read_ks4_year <- function(path, academic_year) {

  if (!file.exists(path)) {
    warning(sprintf("File not found for %s: %s - skipping this year.", academic_year, path))
    return(NULL)
  }

  raw <- read_csv(path, guess_max = 100000, show_col_types = FALSE)
  nm  <- names(raw)

  estab_col  <- pick_col(nm, c("URN", "ESTAB", "estab", "LAESTAB", "SCHNAME_ESTAB"))
  name_col   <- pick_col(nm, c("SCHNAME", "school_name", "SCH_NAME"))
  town_col   <- pick_col(nm, c("TOWN", "town", "ADDRESS3", "SCH_TOWN"))
  la_col     <- pick_col(nm, c("NEW_LA_NAME", "LEA_NAME", "LA_NAME", "la_name"))
  att8_col   <- pick_col(nm, c("ATT8SCR", "AVG_ATT8", "att8_score", "ATT8SCR_AVG"))
  postcode_col <- pick_col(nm, c("PCODE", "POSTCODE", "postcode"))

  missing_required <- c(estab = estab_col, att8 = att8_col)
  if (anyNA(missing_required)) {
    stop(sprintf(
      "Could not find required columns in %s.\nFound columns: %s\nAdd the correct header name(s) to pick_col() candidates above.",
      path, paste(nm, collapse = ", ")
    ))
  }

  out <- raw %>%
    transmute(
      estab       = as.character(.data[[estab_col]]),
      school_name = if (!is.na(name_col)) .data[[name_col]] else NA_character_,
      town        = if (!is.na(town_col)) .data[[town_col]] else NA_character_,
      la_name     = if (!is.na(la_col)) .data[[la_col]] else NA_character_,
      postcode    = if (!is.na(postcode_col)) .data[[postcode_col]] else NA_character_,
      att8_score  = suppressWarnings(as.numeric(.data[[att8_col]])),
      academic_year = academic_year
    ) %>%
    filter(!is.na(att8_score))

  cat(sprintf("  %s : %d schools with a valid Attainment 8 score\n", academic_year, nrow(out)))
  out
}

cat("Loading DfE Key Stage 4 school-level files for each academic year...\n")

education_all_years <- imap(YEAR_FILES, ~ read_ks4_year(.x, .y)) %>%
  compact() %>%          # drop any NULLs (years that were skipped)
  bind_rows()

cat("\nTotal school-year rows loaded across all years :", nrow(education_all_years), "\n")

# ---- 3. Tag county (Norfolk / Suffolk) -------------------------------------
# DfE's LA name field is the most reliable way to do this: Norfolk schools
# report LA = "Norfolk"; Suffolk schools report one of the Suffolk district
# LAs (e.g. "Suffolk", "West Suffolk", "East Suffolk", "Ipswich",
# "Babergh", "Mid Suffolk"). Adjust the la_name patterns below if your
# files use different LA labels for Suffolk's district councils.
education_all_years <- education_all_years %>%
  mutate(
    county_name = case_when(
      str_detect(la_name, regex("norfolk", ignore_case = TRUE)) ~ "Norfolk",
      str_detect(la_name, regex("suffolk|ipswich|babergh|mid suffolk", ignore_case = TRUE)) ~ "Suffolk",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(county_name))

cat("\nRows after restricting to Norfolk / Suffolk :", nrow(education_all_years), "\n")
cat("\nRows per academic year, per county:\n")
print(education_all_years %>% count(academic_year, county_name))

# ---- 4. Write back to the database -----------------------------------------
con <- dbConnect(SQLite(), db_path)

dbWriteTable(con, "fact_education", education_all_years, overwrite = TRUE)

dbDisconnect(con)

cat("\nfact_education table rebuilt with", nrow(education_all_years),
    "rows spanning", length(unique(education_all_years$academic_year)), "academic years.\n")
cat("You can now run EDA_11_Education_BoxPlot_Att8.R, EDA_12_Education_TownChart_Norfolk.R\n")
cat("and EDA_13_Education_TownChart_Suffolk.R for a genuine 2021-2025 multi-year comparison.\n")
