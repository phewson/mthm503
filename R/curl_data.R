library(httr2)
library(jsonlite)


utils::globalVariables(c(
  "casualty_class",
  "casualty_severity",
  "pedestrian_location",
  "pedestrian_movement",
  "pedestrian_crossing_human_control",
  "pedestrian_crossing_physical_facilities",
  "light_conditions",
  "weather_conditions",
  "road_surface_conditions",
  "speed_limit_mph",
  "road_type",
  "junction_detail",
  "junction_control",
  "urban_or_rural_area",
  "vehicle_type",
  "vehicle_manoeuvre",
  "first_point_of_impact",
  "age_of_casualty",
  "sex_of_casualty",
  "age_band_of_casualty",
  "sex_of_driver",
  "age_band_of_driver",
  "number_of_vehicles",
  "number_of_casualties",
  "first_road_class",
  "age_of_vehicle",
  "sex",
  "financial_year",
  "n_casualties",
  "age_band",
  "mean_casualties",
  "mean_count",
  "mean_extrication_rate",
  "se_rate",
  "se_count",
  "kmeans_cluster",
  "hclust_cluster",
  "PC",
  "Variance",
  "Cumulative",
  "PC1",
  "PC2",
  "Cluster",
  "feature",
  "importance",
  "selected",
  "age_specific_casualties",
  "se_casualties"
))


project_id <- "<YOUR_PROJECT_ID>"
anon_key <- "<YOUR_ANON_KEY>"


supabase_url <- paste0("https://", project_id, ".supabase.co")


table <- "stats19_accidents"


req_count <- request(paste0(supabase_url, "/rest/v1/", table)) %>%
  req_headers(
    apikey = anon_key,
    Authorization = paste("Bearer", anon_key),
    Accept = "application/json",
    Prefer = "count=exact",
    `Range-Unit` = "items",
    Range = "0-0"  # Request just first row for count
  ) %>%
  req_url_query(select = "accident_index")  # Or any column

resp_count <- req_perform(req_count)

count_header <- resp_headers(resp_count)[["content-range"]]
total_count <- as.numeric(sub(".*/", "", count_header))

print(paste("Total rows:", total_count))


req_data <- request(paste0(supabase_url, "/rest/v1/", table)) %>%
  req_headers(
    apikey = anon_key,
    Authorization = paste("Bearer", anon_key),
    Accept = "application/json",
    `Range-Unit` = "items",
    Range = "0-9"  # First 10 rows (0-based indexing)
  ) %>%
  req_url_query(select = "*")  # Select all columns

resp_data <- req_perform(req_data)

data <- fromJSON(resp_body_string(resp_data))


print(head(data))
