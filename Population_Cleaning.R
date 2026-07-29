library(tidyverse)
library(readxl)

pop_raw = read_excel(
  "D:/Data_Science/Dataset/Population/sapelsoasyoa20222024.xlsx",
  sheet = "Mid-2024 LSOA 2021",
  skip = 3
)

cat("\nColumns Found In Raw Sheet\n")
print(names(pop_raw))

pop_clean = pop_raw %>%
  select(
    lsoa_code = `LSOA 2021 Code`,
    lsoa_name = `LSOA 2021 Name`,
    population = Total
  )

pop_clean = pop_clean %>%
  mutate(
    lsoa_code = str_trim(as.character(lsoa_code)),
    lsoa_name = str_trim(as.character(lsoa_name)),
    population = as.numeric(population)
  ) %>%
  filter(
    !is.na(lsoa_code),
    lsoa_code != "",
    !is.na(population)
  ) %>%
  distinct(lsoa_code, .keep_all = TRUE)

cat("\n============================\n")
cat("POPULATION DATASET CHECK\n")
cat("============================\n")

cat("\nRows : ", nrow(pop_clean))
cat("\nColumns : ", ncol(pop_clean))

cat("\n\nMissing LSOA Code : ", sum(is.na(pop_clean$lsoa_code)))
cat("\nMissing Population : ", sum(is.na(pop_clean$population)))
cat("\nDuplicate LSOA Code : ", sum(duplicated(pop_clean$lsoa_code)))

cat("\n\nPopulation Summary\n")
print(summary(pop_clean$population))

cat("\nFirst 10 Rows\n")
print(head(pop_clean, 10))

dir.create("D:/Data_Science/Clean Data", showWarnings = FALSE, recursive = TRUE)

write_csv(
  pop_clean,
  "D:/Data_Science/Clean Data/population_clean.csv"
)

cat("\n\npopulation_clean.csv created successfully!\n")