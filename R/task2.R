#2. Regression Task
# ----------------------
# Analysis of Fire Rescue Extrication Casualties Data
# Using Multinomial Logistic and Count Regression Models
#
# This script performs:
# - Data extraction from database
# - Data summarization
# - Multinomial logistic regression on extrication methods by sex and age
# - Poisson and Negative Binomial regression on extrication rates per collision
# - Visualization of predicted extrication method probabilities by demographics
#



# Source database connection 
load_fire_rescue_data <- function() {
  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("PGRHOST"),
    port = as.integer(Sys.getenv("PGRPORT")),
    user = Sys.getenv("PGRUSER"),
    password = Sys.getenv("PGRPASSWORD"),
    dbname = Sys.getenv("PGRDATABASE")
  )
  
  
  on.exit(dbDisconnect(con))
  
on.exit(dbDisconnect(con))

#1. Load required packages 
library(DBI)          
library(dplyr)        
library(tidyr)        
library(nnet)         
library(broom)       
library(ggplot2)      
library(scales)       
library(MASS)         


# 2. Read tables from database 
extr <- dbReadTable(con, "fire_rescue_extrication_casualties")
stats19 <- dbReadTable(con, "stats19_by_financial_year")


# 3. Summarize total police reported collisions by financial year 
stats19_totals <- stats19 %>%
  group_by(financial_year) %>%
  summarise(total_collisions = sum(number_of_stat19_reported_casualties), .groups = "drop")


# 4. Prepare extrication counts by demographics 
extr_long <- extr %>%
  mutate(
    financial_year = as.character(financial_year),
    sex = factor(sex),
    age_band = factor(age_band),
    extrication = factor(extrication)
  ) %>%
  group_by(financial_year, sex, age_band, extrication) %>%
  summarise(n_casualties = sum(n_casualties), .groups = "drop") %>%
  left_join(stats19_totals, by = "financial_year") %>%
  mutate(log_collisions = log(total_collisions))


# 5. Multinomial logistic regression: extrication method ~ sex * age_band 
multinom_mod <- multinom(
  extrication ~ sex * age_band,
  data = extr_long,
  weights = n_casualties,
  trace = FALSE
)


# 6. Summarize and display Relative Risk Ratios (RRRs) 
rrr_table <- tidy(multinom_mod, exponentiate = TRUE, conf.int = TRUE) %>%
  rename(
    RRR = estimate,
    LowerCI = conf.low,
    UpperCI = conf.high,
    P.value = p.value
  )

cat("\n=== Multinomial Model: Relative Risk Ratios (RRRs) ===\n")
print(rrr_table)


# 7. Poisson regression: modeling rate of extrications per collision 
poisson_mod <- glm(
  n_casualties ~ extrication * sex * age_band + offset(log_collisions),
  family = poisson(link = "log"),
  data = extr_long
)

irr_table <- tidy(poisson_mod, exponentiate = TRUE, conf.int = TRUE) %>%
  rename(
    IRR = estimate,
    LowerCI = conf.low,
    UpperCI = conf.high,
    P.value = p.value
  )

cat("\n=== Poisson Model: Incidence Rate Ratios (IRRs) ===\n")
print(irr_table)


# 7a. Check for overdispersion in Poisson model 
dispersion <- sum(residuals(poisson_mod, type = "pearson")^2) / poisson_mod$df.residual
cat(sprintf("\nPoisson Dispersion Statistic = %.2f\n", dispersion))

if (dispersion > 1.5) {
  cat("Warning: Overdispersion detected. Fitting Negative Binomial model...\n")
  
  nb_mod <- glm.nb(
    n_casualties ~ extrication * sex * age_band + offset(log_collisions),
    data = extr_long
  )
  
  nb_table <- tidy(nb_mod, exponentiate = TRUE, conf.int = TRUE) %>%
    rename(
      IRR = estimate,
      LowerCI = conf.low,
      UpperCI = conf.high,
      P.value = p.value
    )
  
  cat("\n=== Negative Binomial Model: Incidence Rate Ratios (IRRs) ===\n")
  print(nb_table)
}


# 8. Predict and visualize predicted mix of extrication methods by demographics 

# 8a. Create new data grid for prediction (all combinations of sex and age_band)
sex_levels <- levels(extr_long$sex)
age_levels <- levels(extr_long$age_band)

newdata_mix <- expand.grid(
  sex = sex_levels,
  age_band = age_levels,
  stringsAsFactors = FALSE
) %>%
  mutate(
    sex = factor(sex, levels = sex_levels),
    age_band = factor(age_band, levels = age_levels)
  )

# 8b. Predict method probabilities from multinomial model
prob_mix <- predict(multinom_mod, newdata = newdata_mix, type = "probs")
colnames(prob_mix) <- levels(extr_long$extrication)

# 8c. Combine predictions with demographic data and reshape for plotting
mix_df <- cbind(newdata_mix, as.data.frame(prob_mix)) %>%
  pivot_longer(
    cols = levels(extr_long$extrication),
    names_to = "method",
    values_to = "prob"
  )

# 8d. Plot predicted probabilities by age band and sex
ggplot(mix_df, aes(x = age_band, y = prob, color = method, group = method)) +
  geom_line(size = 1.2) +
  facet_wrap(~ sex) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Predicted Mix of Extrication Methods by Age Band & Sex",
    x = "Age Band",
    y = "Predicted Probability",
    color = "Extrication Method"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
}