con <- DBI::dbConnect(RSQLite::SQLite(), here::here("mock_data/mockdb.sqlite"))

mock_df <- data.frame(
  casualty_severity = c("Fatal", "Serious", "Slight"),
  sex_of_casualty = c("Male", "Female", "Male"),
  pedestrian_location = c("Crossing", "Footpath", "Road")
)
DBI::dbWriteTable(con, "stats19_casualties", mock_df, overwrite = TRUE)
mock_bike_hire <- dget(here::here("mock_data", "bike_hire.R"))
DBI::dbWriteTable(con, "bike_hire", mock_bike_hire, overwrite = TRUE)


DBI::dbDisconnect(con)
