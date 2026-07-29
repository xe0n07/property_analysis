library(readr)
library(dplyr)
library(stringr)

broadband = read_csv(
  "D:/Data_Science/Dataset/Broadband/201805_fixed_pc_performance_r03.csv",
  show_col_types = FALSE
)

cat("\nBroadband Dataset\n")
cat("=============================\n")
cat("Rows:", nrow(broadband), "\n")
cat("Columns:", ncol(broadband), "\n\n")

broadband = broadband %>%
  transmute(
    postcode = postcode,
    avg_speed = `Average download speed (Mbit/s)`,
    max_speed = `Maximum download speed (Mbit/s)`
  )

broadband = broadband %>%
  mutate(
    postcode = str_to_upper(postcode),
    postcode = str_squish(postcode),
    avg_speed = as.numeric(avg_speed),
    max_speed = as.numeric(max_speed)
  )

broadband = broadband %>%
  mutate(
    county = case_when(
      str_detect(postcode, "^NR") ~ "Norfolk",
      str_detect(postcode, "^IP") ~ "Suffolk",
      TRUE ~ NA_character_
    )
  )

broadband = broadband %>%
  filter(
    !is.na(county),
    !is.na(avg_speed),
    !is.na(max_speed),
    postcode != ""
  ) %>%
  distinct()

cat("Missing Values\n")
cat("=============================\n")
print(colSums(is.na(broadband)))

cat("\nCounty Distribution\n")
cat("=============================\n")
print(count(broadband, county))

cat("\nAverage Download Speed\n")
cat("=============================\n")
print(
  broadband %>%
    group_by(county) %>%
    summarise(
      Mean = round(mean(avg_speed), 2),
      Median = round(median(avg_speed), 2),
      Maximum = round(max(avg_speed), 2),
      Minimum = round(min(avg_speed), 2),
      .groups = "drop"
    )
)

cat("\nDataset Size\n")
cat("=============================\n")
print(dim(broadband))

cat("\nPreview\n")
cat("=============================\n")
print(head(broadband))

write_csv(
  broadband,
  "D:/Data_Science/Dataset/Broadband/Broadband_Clean.csv"
)

cat("\nBroadband_Clean.csv created successfully.\n")