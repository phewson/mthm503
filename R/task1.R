# Load required packages
library(DBI)
library(RPostgres)
library(dplyr)
library(caret)
library(randomForest)
library(pROC)
library(ggplot2)
library(nnet)  # for multinom in caret

# 1. Connect to database
load_pedestrian_casualty_data <- function() {
con <- dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("PGRHOST"),
  port = as.integer(Sys.getenv("PGRPORT")),
  user = Sys.getenv("PGRUSER"),
  password = Sys.getenv("PGRPASSWORD"),
  dbname = Sys.getenv("PGRDATABASE")
)

on.exit(dbDisconnect(con))



#2. Load and merge data from tables
casualties <- dbGetQuery(con, "SELECT * FROM stats19_casualties")
vehicles <- dbGetQuery(con, "SELECT * FROM stats19_vehicles")
accidents <- dbGetQuery(con, "SELECT * FROM stats19_accidents")

# Join datasets on accident_index and vehicle_reference
model_df <- casualties %>%
  inner_join(accidents, by = "accident_index") %>%
  left_join(vehicles, by = c("accident_index", "vehicle_reference"))

# 3. Select relevant columns and clean 
model_df <- model_df %>%
  select(
    casualty_severity,
    sex_of_casualty,
    age_band_of_casualty,
    weather_conditions,
    light_conditions,
    urban_or_rural_area,
    age_band_of_driver,
    sex_of_driver,
    road_type,
    accident_severity
  ) %>%
  na.omit()  # remove rows with missing data for simplicity

#  4. Factorize categorical variables 
model_df$casualty_severity <- factor(model_df$casualty_severity,
                                     levels = c("Fatal", "Serious", "Slight"))

model_df$sex_of_casualty <- factor(model_df$sex_of_casualty)
model_df$age_band_of_casualty <- factor(model_df$age_band_of_casualty)
model_df$weather_conditions <- factor(model_df$weather_conditions)
model_df$light_conditions <- factor(model_df$light_conditions)
model_df$urban_or_rural_area <- factor(model_df$urban_or_rural_area)
model_df$age_band_of_driver <- factor(model_df$age_band_of_driver)
model_df$sex_of_driver <- factor(model_df$sex_of_driver)
model_df$road_type <- factor(model_df$road_type)
model_df$accident_severity <- factor(model_df$accident_severity)

#  5. Split data into train/test 
set.seed(42)
trainIndex <- createDataPartition(model_df$casualty_severity, p = .7, list = FALSE)
train_data <- model_df[trainIndex, ]
test_data <- model_df[-trainIndex, ]

# Plot class imbalance in train data 
ggplot(train_data, aes(x = casualty_severity)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Class Distribution (Train Data) - Casualty Severity",
       x = "Casualty Severity",
       y = "Count") +
  theme_minimal(base_size = 15)

#  6. Create dummy variables based on training data 
dummies <- dummyVars(casualty_severity ~ ., data = train_data)

train_features <- predict(dummies, newdata = train_data)
test_features <- predict(dummies, newdata = test_data)

train_labels <- train_data$casualty_severity
test_labels <- test_data$casualty_severity

# 7. Train models 

## Logistic Regression
model_logistic <- train(
  x = train_features,
  y = train_labels,
  method = "multinom",  # multinomial logistic regression
  trace = FALSE
)

## Random Forest
model_rf <- randomForest(x = train_features, y = train_labels)

#  8. Evaluate models 

# Logistic Regression predictions
pred_logistic <- predict(model_logistic, newdata = test_features)
prob_logistic <- predict(model_logistic, newdata = test_features, type = "prob")

# Random Forest predictions
pred_rf <- predict(model_rf, newdata = test_features)
prob_rf <- predict(model_rf, newdata = test_features, type = "prob")

# Accuracy
acc_logistic <- mean(pred_logistic == test_labels)
acc_rf <- mean(pred_rf == test_labels)

cat("Logistic Regression Accuracy:", round(acc_logistic, 4), "\n")
cat("Random Forest Accuracy:", round(acc_rf, 4), "\n")

# AUC for each class (one-vs-all)
auc_multiclass <- function(probs, true_labels, class) {
  roc_obj <- roc(response = as.numeric(true_labels == class),
                 predictor = probs[, class])
  auc(roc_obj)
}

classes <- levels(test_labels)
auc_logistic <- sapply(classes, function(cl) auc_multiclass(prob_logistic, test_labels, cl))
auc_rf <- sapply(classes, function(cl) auc_multiclass(prob_rf, test_labels, cl))

print(data.frame(Class = classes, Logistic_AUC = round(auc_logistic, 3), RF_AUC = round(auc_rf, 3)))

#  9. Plot confusion matrices 
plot_confusion <- function(true, pred, title) {
  cm <- confusionMatrix(pred, true)
  cm_table <- as.data.frame(cm$table)
  
  ggplot(cm_table, aes(x = Prediction, y = Reference, fill = Freq)) +
    geom_tile(color = "grey70") +
    geom_text(aes(label = Freq), color = "black", size = 5, fontface = "bold") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(title = title, fill = "Count") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_confusion(test_labels, pred_logistic, "Logistic Regression Confusion Matrix")
plot_confusion(test_labels, pred_rf, "Random Forest Confusion Matrix")


}