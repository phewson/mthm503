library(targets)
library(DBI)
library(here)

tar_option_set(
  packages = c("DBI", "RSQLite", "dplyr", "here")
)

source("R/functions.R")

list(
  tar_target(
    db_conn,
    connect_db(),
    cue = tar_cue(mode = "always")  # always reconnect to DB
  ),
  tar_target(
    raw_data,
    get_data(db_conn)
  ),
  tar_target(
    clean_data,
    clean_dataset(raw_data)
  ),
  tar_target(
    summary_data,
    summarize_data(clean_data)
  )
)
