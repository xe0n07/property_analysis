library(tidyverse)

norfolk_dir = "D:/Data_Science/Dataset/Education/Norfolk"
suffolk_dir = "D:/Data_Science/Dataset/Education/Suffolk"

norfolk_files = list.files(
  norfolk_dir,
  pattern = "^[0-9]{4}-[0-9]{4}_926_ks4final\\.csv$",
  full.names = TRUE
)

suffolk_files = list.files(
  suffolk_dir,
  pattern = "^[0-9]{4}-[0-9]{4}_935_ks4final\\.csv$",
  full.names = TRUE
)

cat("\nNorfolk files found\n")
print(norfolk_files)

cat("\nSuffolk files found\n")
print(suffolk_files)

read_ks4_file = function(file_path, county_label) {
  extracted_year = str_extract(basename(file_path), "^[0-9]{4}-[0-9]{4}")
  
  raw = read_csv(
    file_path,
    show_col_types = FALSE,
    col_types = cols(
      RECTYPE = col_character(),
      LEA = col_character(),
      ESTAB = col_character(),
      SCHNAME = col_character(),
      TOWN = col_character(),
      PCODE = col_character(),
      TOTPUPS = col_character(),
      ATT8SCR = col_character(),
      .default = col_character()
    )
  )
  
  raw %>%
    select(
      rectype = RECTYPE,
      lea_code = LEA,
      estab = ESTAB,
      school_name = SCHNAME,
      town = TOWN,
      postcode = PCODE,
      total_pupils = TOTPUPS,
      att8_score = ATT8SCR
    ) %>%
    mutate(
      county = county_label,
      academic_year = extracted_year
    )
}

norfolk_all_years = map_dfr(
  norfolk_files,
  read_ks4_file,
  county_label = "Norfolk"
)

suffolk_all_years = map_dfr(
  suffolk_files,
  read_ks4_file,
  county_label = "Suffolk"
)

cat("\nNorfolk rows across all years : ", nrow(norfolk_all_years))
cat("\nSuffolk rows across all years : ", nrow(suffolk_all_years))

if (any(is.na(norfolk_all_years$academic_year))) {
  stop("academic_year extraction failed for one or more Norfolk files. Check filename pattern.")
}

if (any(is.na(suffolk_all_years$academic_year))) {
  stop("academic_year extraction failed for one or more Suffolk files. Check filename pattern.")
}

education_clean = bind_rows(norfolk_all_years, suffolk_all_years)

education_clean = education_clean %>%
  mutate(
    rectype = as.character(rectype),
    school_name = str_trim(as.character(school_name)),
    town = str_trim(as.character(town)),
    postcode = str_trim(as.character(postcode)),
    att8_score = na_if(att8_score, "NE"),
    att8_score = na_if(att8_score, "SUPP"),
    att8_score = as.numeric(att8_score),
    total_pupils = na_if(total_pupils, "NEW"),
    total_pupils = na_if(total_pupils, "SUPP"),
    total_pupils = as.numeric(total_pupils)
  ) %>%
  filter(
    rectype %in% c("1", "2"),
    !is.na(school_name),
    school_name != "",
    !is.na(town),
    town != ""
  ) %>%
  select(-rectype) %>%
  distinct(estab, county, academic_year, .keep_all = TRUE)

cat("\n============================\n")
cat("EDUCATION DATASET CHECK (2021-2025)\n")
cat("============================\n")

cat("\nRows : ", nrow(education_clean))
cat("\nColumns : ", ncol(education_clean))

cat("\n\nRows Per County\n")
print(table(education_clean$county))

cat("\n\nRows Per Academic Year\n")
print(table(education_clean$academic_year))

cat("\n\nRows Per County Per Academic Year\n")
print(table(education_clean$county, education_clean$academic_year))

cat("\n\nMissing School Name : ", sum(is.na(education_clean$school_name)))
cat("\nMissing Attainment 8 Score : ", sum(is.na(education_clean$att8_score)))
cat("\nMissing Total Pupils (includes 'NEW' schools with no cohort yet) : ",
    sum(is.na(education_clean$total_pupils)))
cat("\nDuplicate estab+county+academic_year : ",
    sum(duplicated(education_clean[, c("estab", "county", "academic_year")])))

cat("\n\nAttainment 8 Score Summary\n")
print(summary(education_clean$att8_score))

cat("\nFirst 10 Rows\n")
print(head(education_clean, 10))

dir.create("D:/Data_Science/Clean Data", showWarnings = FALSE, recursive = TRUE)

write_csv(
  education_clean,
  "D:/Data_Science/Clean Data/education_clean.csv"
)

cat("\n\neducation_clean.csv created successfully (2021-2025, all years)!\n")