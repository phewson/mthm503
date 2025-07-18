# R/load_data.R
library(DBI)
library(RPostgres)
library(RSQLite)
library(dplyr)     # brings in filter(), left_join()
library(magrittr)  # brings in %>%
library(here)

get_db_connection <- function() {
  is_ci <- Sys.getenv("CI") == "true"
  if (is_ci) {
    dbConnect(RSQLite::SQLite(), here("mock_data", "mockdb.sqlite"))
  } else {
    dbConnect(
      Postgres(),
      dbname     = Sys.getenv("SUPABASE_DB"),
      host       = Sys.getenv("SUPABASE_HOST"),
      user       = Sys.getenv("SUPABASE_USER"),
      password   = Sys.getenv("SUPABASE_PASSWORD"),
      port       = as.integer(Sys.getenv("SUPABASE_PORT")),
      sslmode    = Sys.getenv("SUPABASE_SSLMODE"),
      gssencmode = "disable"
    )
  }
}

load_stats19_data <- function() {
  con <- get_db_connection()
  casualties <- dbReadTable(con, "stats19_casualties")
  accidents  <- dbReadTable(con, "stats19_accidents")
  vehicles   <- dbReadTable(con, "stats19_vehicles")
  dbDisconnect(con)
  
  # Join & filter to pedestrian crashes
  casualties %>%
   filter(casualty_class == "Pedestrian") %>%
    left_join(accidents, by = "accident_index") %>%
    left_join(vehicles,    by = "accident_index")
}

# 3) targets-friendly loader (uses tar_read())
load_extrication_data_targets <- function() {
  tar_read(fire_rescue_extrication_casualties)
}

# 4) interactive loader (direct DB read)
load_extrication_data <- function() {
  con <- get_db_connection()
  df  <- dbReadTable(con, "fire_rescue_extrication_casualties")
  dbDisconnect(con)
  df
}

# 5) likewise for stats19_by_financial_year:
load_stats19_by_year_targets <- function() {
  tar_read(stats19_by_financial_year)
}

load_stats19_by_year <- function() {
  con <- get_db_connection()
  df  <- dbReadTable(con, "stats19_by_financial_year")
  dbDisconnect(con)
  df
}

# At the bottom of R/load_data.R

load_extrication_data_from_db <- function() {
  con <- get_db_connection()
  df  <- dbReadTable(con, "fire_rescue_extrication_casualties")
  dbDisconnect(con)
  df
}

load_olive_oil_from_db <- function() {
  con <- get_db_connection()
  df <- dbReadTable(con, "olive_oil")
  dbDisconnect(con)
  df
}

