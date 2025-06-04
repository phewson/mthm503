con <- DBI::dbConnect(RSQLite::SQLite(), here::here("mock_data/mockdb.sqlite"))

mock_df <- data.frame(
  casualty_severity = c("Fatal", "Serious", "Slight"),
  sex_of_casualty = c("Male", "Female", "Male"),
  pedestrian_location = c("Crossing", "Footpath", "Road")
)

DBI::dbWriteTable(con, "stats19_casualties", mock_df, overwrite = TRUE)

mock_extrications <- data.frame(
  financial_year = c("2010/11", "2011/12"),
  extrication = c("Unknown", "Hydraulic"),
  n_casualties = c(10, 20),
  sex = c("Male", "Female"),
  age_band = c("0-16", c("0-16"))
)

DBI::dbWriteTable(con, "fire_rescue_extrication_casualties",
                  mock_df, overwrite = TRUE)
DBI::dbDisconnect(con)
