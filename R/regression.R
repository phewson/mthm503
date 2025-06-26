library(dplyr)
library(ggplot2)

check_hours <- function(hour) {
  valid <- is.numeric(hour) & !is.na(hour) & hour >= 0 & hour <= 23
  if (any(!valid)) {
    warning("One or more entries in 'hour' are invalid (not between 0 and 23).")
  }
  hour[!valid] <- NA
  return(hour) # nolint
}

get_datetime <- function(date, hour) {
  if (length(date) != length(hour)) {
    stop("Length of 'date' and 'hour' must match.")
  }
  hour <- check_hours(hour)
  datetime_str <- paste(as.character(date), sprintf("%02d:00:00", hour))
  strptime(datetime_str, format = "%d/%m/%Y %H:%M:%S")
}

get_time_factors <- function(df) {
  required_cols <- c("datetime", "hr")
  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  df |>
    mutate(doy = as.numeric(format(.data$datetime, "%j")),
           hour = .data$hr,
           dow = as.factor(weekdays(.data$datetime)),
           datetime_day = (as.numeric(.data$datetime) -
                             min(as.numeric(.data$datetime),
                                 na.rm = TRUE)) / (24 * 60 * 60))
}

create_dtg <- function(df) {
  df <- df |>
    mutate(datetime = get_datetime(.data$dteday, .data$hr))
}

plot_simpletrend <- function(df) {
  df |>
    ggplot(aes(x = as.POSIXct(.data$datetime), y = .data$cnt)) +
    geom_line(color = "steelblue") +
    labs(title = "Total Bike Hires Over Time", x = "Date", y = "Daily Count") +
    theme_minimal()
}

plot_temphire <- function(df) {
  df |>
    ggplot(aes(x = .data$temp, y = .data$cnt)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", color = "firebrick") +
    labs(title = "Relationship Between Temperature and Bike Hires",
         x = "Normalized Temperature", y = "Bike Hires") +
    theme_minimal()
}

plot_weekday <- function(df) {
  df |>
    ggplot(aes(x = .data$weekday, y = .data$cnt, group = .data$weekday)) +
    geom_boxplot(fill = "orange", alpha = 0.6) +
    labs(title = "Bike Hires by Day of the Week",
         x = "Day of Week", y = "Daily Count") +
    theme_minimal()
}

plot_season <- function(df) {
  df |>
    ggplot(aes(x = as.POSIXct(.data$datetime), y = .data$cnt)) +
    geom_line(color = "darkgreen") +
    facet_wrap(~ season, scales = "free_y") +
    labs(title = "Bike Hires by Season", x = "Date", y = "Count") +
    theme_minimal()
}

plot_humidity_windspeed <- function(df) {
  df |>
    ggplot(aes(x = .data$hum, y = .data$cnt, color = .data$windspeed)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    scale_color_viridis_c() +
    labs(title = "Interaction of Humidity and Windspeed on Bike Hires",
         x = "Humidity", y = "Bike Hires", color = "Windspeed") +
    theme_minimal()
}

fit_simple_gam <- function(df) {
  m1 <- mgcv::gam(cnt ~ s(as.numeric(datetime)) + hum,
                  data = df, family = poisson())
  return(m1) # nolint
}

fit_final_gam <- function(df) {
  df <- get_time_factors(df)
  gam_model <- mgcv::gam(cnt ~ s(hour, bs = "cc") + dow +
                           s(temp) + s(as.numeric(datetime_day)),
                         family = poisson, data = df,
                         method = "REML")
  return(gam_model) #nolint
}

get_insurance_data <- function() {
  MASS::Insurance
}

plot_claims <- function(df) {
  df |>
    ggplot(aes(x = .data$District, y = .data$Claims, fill = .data$Age)) +
    geom_col(position = "dodge") +
    labs(title = "Total Claims by District and Age Group",
         y = "Number of Claims", x = "District") +
    theme_minimal()
}

plot_claim_rate <- function(df) {
  df |>
    mutate(rate = .data$Claims / .data$Holders) |>
    ggplot(aes(x = .data$Age, y = .data$rate, fill = .data$Group)) +
    geom_col(position = "dodge") +
    labs(title = "Claim Rate per Policyholder by Age and Car Group",
         y = "Claims per Policyholder", x = "Age Group") +
    theme_minimal()
}

fit_glm_with_offset <- function(df) {
  glm_model <- glm(Claims ~ offset(log(Holders)) + Age + Group + District,
                   family = poisson, data = df)
  return(glm_model) # nolint
}

get_helprct <- function() {
  mosaicData::HELPrct
}

fit_mh_score <- function(df) {
  model1 <- lm(cesd ~ substance + mcs + sex + homeless, data = df)
  return(model1) # nolint
}
