# R/task2_regression.R

# Task 2: Regression Modeling of Extrication Rates
# This script defines helper functions to load, clean, model and
# diagnose how casualty age and sex affect extrication by fire brigade.

library(dplyr)
library(lubridate)
library(broom)
library(ggplot2)
library(tidyr)
 
load_extrication_data <- function(fire_data, year_data) {
  
  cat(" Step 1: Original fire_data:\n")
  print(head(fire_data))
  print(str(fire_data))
  
  cat(" Step 2: Original year_data:\n")
  print(head(year_data))
  print(str(year_data))
  
  year_summary <- year_data %>%
    group_by(financial_year) %>%
    summarise(
      number_of_stat19_reported_casualties = sum(number_of_stat19_reported_casualties, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("Step 3: Year summary (after summarise):\n")
  print(year_summary)
  
  counted_fire <- fire_data %>%
    filter(!is.na(extrication), extrication != "Unknown") %>%
    group_by(financial_year, sex, age_band) %>%
    summarise(extrications = sum(n_casualties), .groups = "drop")
  
  
  cat("Step 4: Counted fire data (summed extrications):\n")
  print(head(counted_fire))
  
counted_fire <- fire_data %>%
  filter(!is.na(extrication), extrication != "Unknown") %>%
  group_by(financial_year, sex, age_band) %>%
  summarise(extrications = sum(n_casualties), .groups = "drop")
# 🔗 2. Yıllık özetle birleştir
joined <- counted_fire %>%
  left_join(
    year_data %>%
      group_by(financial_year) %>%
      summarise(
        number_of_stat19_reported_casualties = sum(number_of_stat19_reported_casualties),
        .groups = "drop"
      ),
    by = "financial_year"
  )

df2_clean <- joined <- counted_fire %>%
  left_join(year_summary, by = "financial_year") %>%
  rename(
    collisions_reported = number_of_stat19_reported_casualties,
    sex_of_casualty    = sex,
    age_band_raw       = age_band
  ) %>%
  mutate(
    # Fix this mapping to match your data
    age_band_of_casualty = case_when(
      age_band_raw == "0-17"           ~ "0-17",
      age_band_raw %in% c("18-34")     ~ "18-34",
      age_band_raw == "35-64"          ~ "35-64",
      age_band_raw == "65+"            ~ "65+",
      TRUE                              ~ NA_character_
    ),
    rate = extrications / collisions_reported,
    sex_of_casualty      = factor(sex_of_casualty),
    age_band_of_casualty = factor(
      age_band_of_casualty,
      levels = c("0-17", "18-34", "35-64", "65+")
    )
  )

# Now inspect before dropping:
print(table(joined$age_band_raw, useNA="always"))
print(table(joined$age_band_of_casualty, useNA="always"))

# Only then drop:
df2_clean <- joined %>%
  drop_na(sex_of_casualty, age_band_of_casualty)


# 📊 4. Kontroller
cat("✅ Extrications dağılımı:\n")
print(table(df2_clean$extrications))

cat("\n✅ Rate dağılımı:\n")
print(summary(df2_clean$rate))

  joined <- counted_fire %>%
    left_join(year_summary, by = "financial_year")
  
  
  cat("✅ Step 5: After join:\n")
  print(head(joined))
  print(paste("🟠 Number of NAs in collisions_reported:", sum(is.na(joined$number_of_stat19_reported_casualties))))
  print("❗ age_band unique values before mapping:")
  print(unique(joined$age_band))
  
  # Tüm dönüşümler burada:
  processed <- joined %>%
    rename(
      collisions_reported = number_of_stat19_reported_casualties,
      sex_of_casualty = sex,
      age_band_raw = age_band
    ) %>%
    mutate(
      age_band_of_casualty = case_when(
        age_band_raw == "0-16" ~ "0-17",
        age_band_raw %in% c("17-24", "25-39") ~ "18-34",
        age_band_raw == "40-64" ~ "35-64",
        age_band_raw == "65+" ~ "65+",
        TRUE ~ NA_character_
      ),
      rate = extrications / collisions_reported,
      sex_of_casualty = factor(sex_of_casualty),
      age_band_of_casualty = factor(
        age_band_of_casualty,
        levels = c("0-17", "18-34", "35-64", "65+")
      )
    ) %>%
    drop_na(sex_of_casualty, age_band_of_casualty)
  
  cat("✅ Final processed preview:\n")
  print(head(processed))
  print(table(processed$age_band_of_casualty, useNA = "always"))
  
  return(processed)
}



task2_clean_data <- function(df) {
  df <- df %>%
    mutate(
      sex_of_casualty = factor(sex_of_casualty),
      age_group = age_band_of_casualty  # zaten factor olarak geldiği için yeniden binlemeye gerek yok
    ) %>%
    drop_na(sex_of_casualty, age_group)
  
  cat("✅ Available `sex_of_casualty` levels:\n")
  print(table(df$sex_of_casualty))
  
  cat("✅ `age_band_of_casualty` counts:\n")
  print(table(df$age_band_of_casualty, useNA = "always"))
  
  cat("✅ Assigned `age_group` counts:\n")
  print(table(df$age_group, useNA = "always"))
  
  return(df)
}





# 1) Poisson model kurma fonksiyonu
fit_poisson_model <- function(df) {
  glm(
    extrications ~ age_band_of_casualty * sex_of_casualty + offset(log(collisions_reported)),
    data   = df,
    family = poisson(link = "log")
  )
}

# 2) Model çıktısını özetle (exponentiated + CI)
summarize_poisson_model <- function(mod) {
  broom::tidy(mod, conf.int = TRUE, exponentiate = TRUE) %>%
    mutate(term = recode(term,
                         `(Intercept)`             = "Baseline (0–17, Female)",
                         `age_band_of_casualty18-34` = "Age 18–34",
                         `age_band_of_casualty35-64` = "Age 35–64",
                         `age_band_of_casualty65+`   = "Age 65+",
                         `sex_of_casualtyMale`       = "Male",
                         `age_band_of_casualty18-34:sex_of_casualtyMale` = "18–34 × Male",
                         `age_band_of_casualty35-64:sex_of_casualtyMale` = "35–64 × Male",
                         `age_band_of_casualty65+:sex_of_casualtyMale`   = "65+ × Male"))
}

# 3) Modelin varyasyon kontrolü (isteğe bağlı)
check_rate_variation <- function(df) {
  df %>%
    group_by(age_band_of_casualty, sex_of_casualty) %>%
    summarise(
      mean_rate = mean(rate),
      sd_rate   = sd(rate),
      n         = n(),
      .groups   = "drop"
    ) %>%
    print()
}
