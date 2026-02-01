library(randomForest)
library(caret)
library(dplyr)

# Prepare Training Data
train_data <- read.csv("Prediction3Train.csv", stringsAsFactors = FALSE)
train_data$Major <- as.factor(train_data$Major)
train_data$University <- as.factor(train_data$University)
train_data$Hired <- as.factor(train_data$Hired)

set.seed(123)
train_index <- createDataPartition(train_data$Hired, p = 0.8, list = FALSE)
train_set <- train_data[train_index, ]

# Recode University factor to keep top 52 and label the rest as "Other"
train_set$University <- as.character(train_set$University)
univ_counts <- sort(table(train_set$University), decreasing = TRUE)
top_univ <- names(univ_counts)[1:52]
train_set$University[!(train_set$University %in% top_univ)] <- "Other"
train_set$University <- factor(train_set$University)

# Train the Random Forest Model
set.seed(123)
rf_model <- randomForest(Hired ~ GPA + Major + University, data = train_set, ntree = 100)
print(rf_model)

# Load and Prepare Student Data for Prediction
# Load and Prepare Student Data for Prediction
students_data <- read.csv("Prediction3Students.csv", stringsAsFactors = FALSE)

# Ensure 'Major' factor levels match training data
students_data$Major <- factor(students_data$Major, levels = levels(train_set$Major))

# Process 'University' similarly and recode values not in top_univ as "Other"
students_data$University <- as.character(students_data$University)
students_data$University[!(students_data$University %in% top_univ)] <- "Other"
students_data$University <- factor(students_data$University, levels = levels(train_set$University))

# Make Predictions on Student Data
predictions <- predict(rf_model, newdata = students_data)

# Replace NA predictions with a random choice between the available levels
na_indices <- which(is.na(predictions))
if(length(na_indices) > 0) {
  predictions[na_indices] <- sample(levels(train_set$Hired), length(na_indices), replace = TRUE)
}

# Export predictions: create submission file with ID and Predicted_Hired columns
submission <- students_data %>%
  select(ID) %>%
  mutate(Predicted_Hired = predictions)
#write.csv(submission, "submission.csv", row.names = FALSE)
