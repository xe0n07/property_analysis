library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(tibble)
library(RSQLite)
library(DBI)

clean_data_path = "D:/Data_Science/Clean Data"
db_path = "D:/Data_Science/Clean Data/norfolk_suffolk.db"

norfolk_crime_raw = read_csv(
  file.path(clean_data_path, "Norfolk_Clean.csv"),
  show_col_types = FALSE,
  col_types = cols(month = col_character())
)

suffolk_crime_raw = read_csv(
  file.path(clean_data_path, "Suffolk_Clean.csv"),
  show_col_types = FALSE,
  col_types = cols(month = col_character())
)

house_price_raw = read_csv(
  file.path(clean_data_path, "house_price_clean.csv"),
  show_col_types = FALSE
)

broadband_raw = read_csv(
  file.path(clean_data_path, "Broadband_Clean.csv"),
  show_col_types = FALSE
)

education_raw = read_csv(
  file.path(clean_data_path, "education_clean.csv"),
  show_col_types = FALSE
)

population_raw = read_csv(
  file.path(clean_data_path, "population_clean.csv"),
  show_col_types = FALSE
)

cat("\n============================\n")
cat("STEP 1: STANDARDISE CRIME DATES\n")
cat("============================\n")

norfolk_crime = norfolk_crime_raw %>%
  mutate(
    month_parsed = parse_date_time(
      month,
      orders = c("mdy", "ymd", "dmy"),
      quiet = TRUE
    )
  ) %>%
  mutate(month_parsed = as.Date(month_parsed))

suffolk_crime = suffolk_crime_raw %>%
  mutate(
    month_parsed = parse_date_time(
      month,
      orders = c("ymd", "mdy", "dmy"),
      quiet = TRUE
    )
  ) %>%
  mutate(month_parsed = as.Date(month_parsed))

cat("\nNorfolk unparsed dates : ", sum(is.na(norfolk_crime$month_parsed)))
cat("\nSuffolk unparsed dates : ", sum(is.na(suffolk_crime$month_parsed)))

if (sum(is.na(norfolk_crime$month_parsed)) > 0) {
  cat("\nSample raw Norfolk month values (unparsed) :\n")
  print(head(unique(norfolk_crime$month[is.na(norfolk_crime$month_parsed)]), 5))
}

if (sum(is.na(suffolk_crime$month_parsed)) > 0) {
  cat("\nSample raw Suffolk month values (unparsed) :\n")
  print(head(unique(suffolk_crime$month[is.na(suffolk_crime$month_parsed)]), 5))
}

crime_all = bind_rows(
  norfolk_crime %>% mutate(source_county = "Norfolk"),
  suffolk_crime %>% mutate(source_county = "Suffolk")
) %>%
  mutate(
    crime_date = month_parsed,
    year = year(crime_date)
  ) %>%
  select(-month, -month_parsed) %>%
  filter(!is.na(crime_date))

cat("\nCombined crime rows : ", nrow(crime_all))

cat("\n============================\n")
cat("STEP 2: TAG OUT-OF-BOUNDARY CRIME LSOAs\n")
cat("============================\n")

norfolk_districts_titlecase = c(
  "Breckland", "Broadland", "Great Yarmouth",
  "King's Lynn And West Norfolk", "North Norfolk",
  "Norwich", "South Norfolk"
)

norfolk_districts = c(
  "Breckland", "Broadland", "Great Yarmouth",
  "King's Lynn and West Norfolk", "North Norfolk",
  "Norwich", "South Norfolk"
)

suffolk_districts = c(
  "Babergh", "East Suffolk", "Ipswich",
  "Mid Suffolk", "West Suffolk"
)

crime_all = crime_all %>%
  mutate(
    lsoa_district = str_trim(str_remove(lsoa_name, "\\s+[A-Za-z0-9]+$")),
    in_boundary = case_when(
      source_county == "Norfolk" & lsoa_district %in% norfolk_districts ~ TRUE,
      source_county == "Suffolk" & lsoa_district %in% suffolk_districts ~ TRUE,
      TRUE ~ FALSE
    )
  )

cat("\nIn-boundary rows  : ", sum(crime_all$in_boundary))
cat("\nOut-of-boundary rows (kept, tagged) : ", sum(!crime_all$in_boundary))

cat("\n============================\n")
cat("STEP 3: STANDARDISE POSTCODES\n")
cat("============================\n")

house_price = house_price_raw %>%
  mutate(postcode_key = str_replace_all(str_to_upper(postcode), "\\s+", ""))

education = education_raw %>%
  mutate(postcode_key = str_replace_all(str_to_upper(postcode), "\\s+", ""))

broadband = broadband_raw %>%
  mutate(postcode_key = str_replace_all(str_to_upper(postcode), "\\s+", ""))

cat("\nHouse price distinct postcode_key : ", n_distinct(house_price$postcode_key))
cat("\nEducation distinct postcode_key   : ", n_distinct(education$postcode_key))
cat("\nBroadband distinct postcode_key   : ", n_distinct(broadband$postcode_key))

cat("\n============================\n")
cat("STEP 4: FILTER POPULATION TO RELEVANT LSOAs\n")
cat("============================\n")

crime_lsoa_codes = crime_all %>%
  distinct(lsoa_code) %>%
  pull(lsoa_code)

population = population_raw %>%
  filter(lsoa_code %in% crime_lsoa_codes)

cat("\nPopulation rows before filter : ", nrow(population_raw))
cat("\nPopulation rows after filter  : ", nrow(population))

cat("\n============================\n")
cat("STEP 5: BUILD DIMENSION TABLES\n")
cat("============================\n")

dim_county = tibble(
  county_id = 1:2,
  county_name = c("Norfolk", "Suffolk")
)

dim_district = tibble(
  district_name = c(norfolk_districts_titlecase, suffolk_districts),
  county_name = c(
    rep("Norfolk", length(norfolk_districts_titlecase)),
    rep("Suffolk", length(suffolk_districts))
  )
) %>%
  left_join(dim_county, by = "county_name") %>%
  mutate(district_id = row_number()) %>%
  select(district_id, district_name, county_id)

district_lookup = dim_district %>%
  select(district_id, district_name)

house_price = house_price %>%
  left_join(district_lookup, by = c("district" = "district_name"))

cat("\nHouse price rows unmatched to a district : ",
    sum(is.na(house_price$district_id)))

dim_lsoa = crime_all %>%
  distinct(lsoa_code, lsoa_name, source_county) %>%
  left_join(dim_county, by = c("source_county" = "county_name")) %>%
  select(lsoa_code, lsoa_name, county_id) %>%
  distinct(lsoa_code, .keep_all = TRUE)

dim_school = education %>%
  distinct(estab, school_name, town, postcode_key, county) %>%
  left_join(dim_county, by = c("county" = "county_name")) %>%
  select(estab, school_name, town, postcode = postcode_key, county_id) %>%
  distinct(estab, .keep_all = TRUE)

cat("\ndim_county rows   : ", nrow(dim_county))
cat("\ndim_district rows : ", nrow(dim_district))
cat("\ndim_lsoa rows     : ", nrow(dim_lsoa))
cat("\ndim_school rows   : ", nrow(dim_school))

cat("\n============================\n")
cat("STEP 6: BUILD FACT TABLES\n")
cat("============================\n")

fact_population = population %>%
  semi_join(dim_lsoa, by = "lsoa_code") %>%
  select(lsoa_code, population)

fact_house_price = house_price %>%
  filter(!is.na(district_id)) %>%
  mutate(
    transaction_pk = row_number(),
    date = format(date, "%Y-%m-%d")
  ) %>%
  select(
    transaction_pk,
    date,
    year,
    postcode = postcode_key,
    district_id,
    price
  )

fact_broadband = broadband %>%
  distinct(postcode_key, .keep_all = TRUE) %>%
  left_join(dim_county, by = c("county" = "county_name")) %>%
  select(
    postcode = postcode_key,
    avg_speed,
    max_speed,
    county_id
  )

fact_crime = crime_all %>%
  mutate(
    crime_pk = row_number(),
    crime_date = format(crime_date, "%Y-%m-%d")
  ) %>%
  select(
    crime_pk,
    crime_date,
    year,
    lsoa_code,
    crime_type,
    outcome,
    police_force,
    in_boundary
  )

fact_education = education %>%
  mutate(education_pk = row_number()) %>%
  select(
    education_pk,
    estab,
    academic_year,
    total_pupils,
    att8_score
  )

cat("\nfact_population rows   : ", nrow(fact_population))
cat("\nfact_house_price rows  : ", nrow(fact_house_price))
cat("\nfact_house_price years : ")
print(sort(unique(fact_house_price$year)))
cat("\nfact_broadband rows    : ", nrow(fact_broadband))
cat("\nfact_crime rows        : ", nrow(fact_crime))
cat("\nfact_education rows    : ", nrow(fact_education))
cat("\nfact_education academic years : ")
print(sort(unique(fact_education$academic_year)))

cat("\n============================\n")
cat("STEP 7: WRITE TO SQLITE (3NF)\n")
cat("============================\n")

if (file.exists(db_path)) file.remove(db_path)

con = dbConnect(SQLite(), db_path)

dbExecute(con, "PRAGMA foreign_keys = ON")

dbWriteTable(con, "dim_county", dim_county, overwrite = TRUE)
dbWriteTable(con, "dim_district", dim_district, overwrite = TRUE)
dbWriteTable(con, "dim_lsoa", dim_lsoa, overwrite = TRUE)
dbWriteTable(con, "dim_school", dim_school, overwrite = TRUE)

dbWriteTable(con, "fact_population", fact_population, overwrite = TRUE)
dbWriteTable(con, "fact_house_price", fact_house_price, overwrite = TRUE)
dbWriteTable(con, "fact_broadband", fact_broadband, overwrite = TRUE)
dbWriteTable(con, "fact_crime", fact_crime, overwrite = TRUE)
dbWriteTable(con, "fact_education", fact_education, overwrite = TRUE)

dbExecute(con, "CREATE INDEX idx_lsoa_county ON dim_lsoa(county_id)")
dbExecute(con, "CREATE INDEX idx_district_county ON dim_district(county_id)")
dbExecute(con, "CREATE INDEX idx_school_county ON dim_school(county_id)")
dbExecute(con, "CREATE INDEX idx_pop_lsoa ON fact_population(lsoa_code)")
dbExecute(con, "CREATE INDEX idx_hp_district ON fact_house_price(district_id)")
dbExecute(con, "CREATE INDEX idx_hp_postcode ON fact_house_price(postcode)")
dbExecute(con, "CREATE INDEX idx_bb_postcode ON fact_broadband(postcode)")
dbExecute(con, "CREATE INDEX idx_bb_county ON fact_broadband(county_id)")
dbExecute(con, "CREATE INDEX idx_crime_lsoa ON fact_crime(lsoa_code)")
dbExecute(con, "CREATE INDEX idx_crime_year ON fact_crime(year)")
dbExecute(con, "CREATE INDEX idx_edu_estab ON fact_education(estab)")
dbExecute(con, "CREATE INDEX idx_edu_year ON fact_education(academic_year)")

cat("\nTables written to database:\n")
print(dbListTables(con))

cat("\n============================\n")
cat("STEP 8: VALIDATION CHECKS\n")
cat("============================\n")

cat("\nRow counts per table\n")
for (tbl in dbListTables(con)) {
  n = dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM ", tbl))$n
  cat(sprintf("  %-20s : %d\n", tbl, n))
}

orphan_house_price = dbGetQuery(con, "
  SELECT COUNT(*) AS n
  FROM fact_house_price hp
  LEFT JOIN dim_district d ON hp.district_id = d.district_id
  WHERE d.district_id IS NULL
")$n

orphan_crime = dbGetQuery(con, "
  SELECT COUNT(*) AS n
  FROM fact_crime c
  LEFT JOIN dim_lsoa l ON c.lsoa_code = l.lsoa_code
  WHERE l.lsoa_code IS NULL
")$n

orphan_population = dbGetQuery(con, "
  SELECT COUNT(*) AS n
  FROM fact_population p
  LEFT JOIN dim_lsoa l ON p.lsoa_code = l.lsoa_code
  WHERE l.lsoa_code IS NULL
")$n

orphan_education = dbGetQuery(con, "
  SELECT COUNT(*) AS n
  FROM fact_education e
  LEFT JOIN dim_school s ON e.estab = s.estab
  WHERE s.estab IS NULL
")$n

cat("\nOrphan fact_house_price rows (no matching district) : ", orphan_house_price)
cat("\nOrphan fact_crime rows (no matching lsoa)           : ", orphan_crime)
cat("\nOrphan fact_population rows (no matching lsoa)      : ", orphan_population)
cat("\nOrphan fact_education rows (no matching school)     : ", orphan_education)

dbDisconnect(con)

cat("\n\nSQLite database built successfully at:\n")
cat(db_path, "\n")