# This code is manually adjusted, and run manually
library(tidyverse)
library(mdsr)
library(readxl)
library(RSQLite)

## Annoying pre-processing
src <- "https://www.fueleconomy.gov/feg/epadata/16data.zip"
lcl <- usethis::use_zip(src)
filename <- "16data/2016 FEGuide for DOE-OK to release-no-sales-5-8-2019_Mercedes_public.xlsx" # nolint

cars_df <- read_excel(filename) |>
  janitor::clean_names() |>
  mutate(
    make = iconv(mfr_name, from = "latin1", to = "UTF-8"),
    model = iconv(carline, from = "latin1", to = "UTF-8")
  ) |>
  select(
    make,
    division,
    model,
    displacement = eng_displ,
    number_cyl,
    number_gears,
    city_mpg = city_fe_guide_conventional_fuel,
    hwy_mpg = hwy_fe_guide_conventional_fuel
  ) |>
  distinct(model, .keep_all = TRUE) |>
  filter(make == "Volkswagen Group of" & division == "Audi")

con <- dbConnect(RSQLite::SQLite(), dbname = "cars.sqlite")

# Write the data frame to the database as a table called 'iris_table'
dbWriteTable(con, "audi", cars_df)
dbDisconnect(con)
