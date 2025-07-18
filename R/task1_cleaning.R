# R/task1_cleaning.R

library(dplyr)
library(lubridate)
library(tidyr)

task1_clean_data <- function(df) {
  df %>%
    # Select a wider range of relevant variables
    select(
      casualty_severity,
      obs_date,
      age_of_casualty,
      sex_of_casualty,
      casualty_class,
      casualty_type,
      pedestrian_location,
      pedestrian_movement,
      urban_or_rural_area,
      weather_conditions,
      light_conditions,
      road_surface_conditions,
      special_conditions_at_site,
      carriageway_hazards,
      speed_limit_mph,
      road_type,
      junction_detail,
      junction_control,
      pedestrian_crossing_human_control,
      pedestrian_crossing_physical_facilities,
      vehicle_type,
      vehicle_manoeuvre,
      skidding_and_overturning,
      hit_object_in_carriageway,
      hit_object_off_carriageway,
      first_point_of_impact,
      sex_of_driver,
      age_of_driver,
      journey_purpose_of_driver
    ) %>%
    # Drop rows with NA in any key variables
    drop_na() %>%
    # Convert and bin features
    mutate(
      obs_date = as_date(obs_date),
      casualty_severity = factor(casualty_severity, levels = c("Slight", "Serious", "Fatal")),
      sex_of_casualty = factor(sex_of_casualty),
      casualty_class = factor(casualty_class),
      casualty_type = factor(casualty_type),
      pedestrian_location = factor(pedestrian_location),
      pedestrian_movement = factor(pedestrian_movement),
      urban_or_rural_area = factor(urban_or_rural_area),
      weather_conditions = factor(weather_conditions),
      light_conditions = factor(light_conditions),
      road_surface_conditions = factor(road_surface_conditions),
      special_conditions_at_site = factor(special_conditions_at_site),
      carriageway_hazards = factor(carriageway_hazards),
      road_type = factor(road_type),
      junction_detail = factor(junction_detail),
      junction_control = factor(junction_control),
      pedestrian_crossing_human_control = factor(pedestrian_crossing_human_control),
      pedestrian_crossing_physical_facilities = factor(pedestrian_crossing_physical_facilities),
      vehicle_type = factor(vehicle_type),
      vehicle_manoeuvre = factor(vehicle_manoeuvre),
      skidding_and_overturning = factor(skidding_and_overturning),
      hit_object_in_carriageway = factor(hit_object_in_carriageway),
      hit_object_off_carriageway = factor(hit_object_off_carriageway),
      first_point_of_impact = factor(first_point_of_impact),
      sex_of_driver = factor(sex_of_driver),
      journey_purpose_of_driver = factor(journey_purpose_of_driver),
      age_group = cut(
        age_of_casualty,
        breaks = c(0, 17, 34, 64, 120),
        labels = c("0-17", "18-34", "35-64", "65+"),
        right = FALSE
      ),
      driver_age_group = cut(
        age_of_driver,
        breaks = c(0, 17, 34, 64, 120),
        labels = c("0-17", "18-34", "35-64", "65+"),
        right = FALSE
      )
    )
}
