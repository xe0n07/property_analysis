install.packages(c("lubridate","readr","purrr","stringr"))

library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)

crime_folder = "D:/Data_Science/Dataset/Crime"
output_folder = "D:/Data_Science/Dataset"

####################################################
# COMBINE NORFOLK FILES
####################################################

norfolk_files = list.files(
  path = crime_folder,
  pattern = "norfolk.*\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

cat("Norfolk files found:", length(norfolk_files), "\n")

norfolk_all = map_dfr(
  norfolk_files,
  ~read_csv(.x, show_col_types = FALSE)
)

write_csv(
  norfolk_all,
  file.path(output_folder, "Norfolk_All.csv")
)

####################################################
# COMBINE SUFFOLK FILES
####################################################

suffolk_files = list.files(
  path = crime_folder,
  pattern = "suffolk.*\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

cat("Suffolk files found:", length(suffolk_files), "\n")

suffolk_all = map_dfr(
  suffolk_files,
  ~read_csv(.x, show_col_types = FALSE)
)

write_csv(
  suffolk_all,
  file.path(output_folder, "Suffolk_All.csv")
)

####################################################
# CLEAN NORFOLK DATA
####################################################

norfolk = read_csv(
  file.path(output_folder, "Norfolk_All.csv"),
  show_col_types = FALSE
)

cat("\n==============================\n")
cat("Cleaning Norfolk Dataset\n")
cat("==============================\n")

norfolk = norfolk %>%
  transmute(
    month = Month,
    reported_by = `Reported by`,
    police_force = `Falls within`,
    lsoa_code = `LSOA code`,
    lsoa_name = `LSOA name`,
    crime_type = `Crime type`,
    outcome = `Last outcome category`
  )

norfolk = norfolk %>%
  mutate(
    month = ym(month),
    year = year(month),
    lsoa_code = str_to_upper(str_trim(lsoa_code)),
    lsoa_name = str_squish(lsoa_name),
    crime_type = str_to_title(str_squish(crime_type)),
    reported_by = str_squish(reported_by),
    police_force = str_squish(police_force),
    outcome = if_else(is.na(outcome), "Unknown", outcome)
  )

norfolk = norfolk %>%
  filter(
    !is.na(month),
    lsoa_code != "",
    lsoa_name != "",
    crime_type != ""
  )

norfolk = norfolk %>%
  distinct()

cat("\nMissing Values\n")
print(colSums(is.na(norfolk)))

cat("\nCrime Types\n")
print(sort(table(norfolk$crime_type), decreasing = TRUE))

cat("\nDataset Size\n")
print(dim(norfolk))

write_csv(
  norfolk,
  file.path(output_folder, "Norfolk_Clean.csv")
)

####################################################
# CLEAN SUFFOLK DATA
####################################################

suffolk = read_csv(
  file.path(output_folder, "Suffolk_All.csv"),
  show_col_types = FALSE
)

cat("\n==============================\n")
cat("Cleaning Suffolk Dataset\n")
cat("==============================\n")

suffolk = suffolk %>%
  transmute(
    month = Month,
    reported_by = `Reported by`,
    police_force = `Falls within`,
    lsoa_code = `LSOA code`,
    lsoa_name = `LSOA name`,
    crime_type = `Crime type`,
    outcome = `Last outcome category`
  )

suffolk = suffolk %>%
  mutate(
    month = ym(month),
    year = year(month),
    lsoa_code = str_to_upper(str_trim(lsoa_code)),
    lsoa_name = str_squish(lsoa_name),
    crime_type = str_to_title(str_squish(crime_type)),
    reported_by = str_squish(reported_by),
    police_force = str_squish(police_force),
    outcome = if_else(is.na(outcome), "Unknown", outcome)
  )

suffolk = suffolk %>%
  filter(
    !is.na(month),
    lsoa_code != "",
    lsoa_name != "",
    crime_type != ""
  )

suffolk = suffolk %>%
  distinct()

cat("\nMissing Values\n")
print(colSums(is.na(suffolk)))

cat("\nCrime Types\n")
print(sort(table(suffolk$crime_type), decreasing = TRUE))

cat("\nDataset Size\n")
print(dim(suffolk))

write_csv(
  suffolk,
  file.path(output_folder, "Suffolk_Clean.csv")
)

####################################################
# FINAL SUMMARY
####################################################

cat("\n=====================================\n")
cat("Processing Completed Successfully\n")
cat("=====================================\n")

cat("\nGenerated Files:\n")
cat("Norfolk_All.csv\n")
cat("Suffolk_All.csv\n")
cat("Norfolk_Clean.csv\n")
cat("Suffolk_Clean.csv\n")

cat("\nFinal Records\n")
cat("Norfolk :", nrow(norfolk), "\n")
cat("Suffolk :", nrow(suffolk), "\n")