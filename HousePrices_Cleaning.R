library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(lubridate)

pp_files = list.files(
  "D:/Data_Science/Dataset/HousePrices",
  pattern = "^pp-20(21|22|23|24|25|26)\\.csv$",
  full.names = TRUE
)

cat("\nFiles Found For Merging\n")
print(pp_files)

col_names_pp = c(
  "transaction_id",
  "price",
  "date",
  "postcode",
  "property_type",
  "old_new",
  "duration",
  "paon",
  "saon",
  "street",
  "locality",
  "town",
  "district",
  "county",
  "category",
  "record_status"
)

house_price = map_dfr(
  pp_files,
  function(f) {
    read_csv(
      f,
      col_names = col_names_pp,
      show_col_types = FALSE
    )
  }
)

cat("\nMerged Dataset Size (2021-2026)\n")
print(dim(house_price))

cat("\nColumn Names\n")
print(names(house_price))

house_price_clean = house_price %>%
  select(
    date,
    postcode,
    town,
    district,
    county,
    price
  )

house_price_clean = house_price_clean %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    postcode = str_to_upper(str_trim(postcode)),
    town = str_to_title(town),
    district = str_to_title(district),
    county = str_to_title(county),
    price = as.numeric(price)
  )

house_price_clean = house_price_clean %>%
  filter(county %in% c("Norfolk", "Suffolk"))

house_price_clean = house_price_clean %>%
  filter(
    !is.na(postcode),
    postcode != "",
    !is.na(price),
    !is.na(year),
    !is.na(district),
    !is.na(county)
  ) %>%
  distinct()

cat("\nMissing Values\n")
print(colSums(is.na(house_price_clean)))

cat("\nSummary Statistics\n")
print(summary(house_price_clean))

cat("\nClean Dataset Size (Norfolk & Suffolk, 2021-2026)\n")
print(dim(house_price_clean))

cat("\nRows Per Year\n")
print(table(house_price_clean$year))

cat("\nNote: 2026 is a partial year (raw file is far smaller than prior years,")
cat("\nsince the year is still in progress). Any 2026 average or trend should")
cat("\nbe labelled as partial-year data, not a full annual figure.\n")

print(head(house_price_clean, 10))

dir.create(
  "D:/Data_Science/Clean Data",
  showWarnings = FALSE,
  recursive = TRUE
)

write_csv(
  house_price_clean,
  "D:/Data_Science/Clean Data/house_price_clean.csv"
)

cat("\nHouse Price Cleaning (2021-2026) Completed Successfully.\n")