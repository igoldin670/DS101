library(rpart)
library(rpart.plot)

# Load the dataset
data <- read.csv("Prediction2025-2Train.csv")

# Encode categorical variables in R
data$Groom_MB <- as.numeric(factor(data$Groom_MB))
data$Bride_MB <- as.numeric(factor(data$Bride_MB))
data$Groom_Edu <- as.numeric(factor(data$Groom_Edu))
data$Bride_Edu <- as.numeric(factor(data$Bride_Edu))
data$Outcome <- ifelse(data$Outcome == "Success", 1, 0)

# Train a more flexible decision tree
tree <- rpart(Outcome ~ ., data = data, method = "class", 
              cp = 0.001,     # Reduce pruning for better learning
              minsplit = 5,   # Allow smaller nodes to split
              minbucket = 2,  # Reduce required instances per leaf
              maxdepth = 10)  # Allow deeper trees for better accuracy

# Predictions
pred <- predict(tree, data, type = "class")
data$predict <- pred

# Evaluate accuracy
accuracy <- mean(data$Outcome == data$predict)
print(paste("Model Accuracy:", round(accuracy, 4)))

# Visualize the improved tree
rpart.plot(tree, type = 2, extra = 104, fallen.leaves = TRUE, main = "Improved Decision Tree")

# Note: Prediction performance might not be great because midterms took priority 

# Load the test dataset
test_data <- read.csv("Prediction2025-2TestStud.csv")

# Encode categorical variables in the test data 
# (ensure that encoding is consistent with the training data)
test_data$Groom_MB <- as.numeric(factor(test_data$Groom_MB))
test_data$Bride_MB <- as.numeric(factor(test_data$Bride_MB))
test_data$Groom_Edu <- as.numeric(factor(test_data$Groom_Edu))
test_data$Bride_Edu <- as.numeric(factor(test_data$Bride_Edu))

# Use the trained decision tree to predict outcomes for the test data
test_pred <- predict(tree, newdata = test_data, type = "class")

# Convert numeric predictions back to character labels if needed
# Here, assuming 1 represents "Success" and 0 represents "Failure"
test_data$Outcome <- ifelse(test_pred == 1, "Success", "Failure")

# Write the predictions to a CSV file for submission
submission <- read.csv("Prediction2025-2submission.csv")
submission$Outcome <- test_data$Outcome
write.csv(submission, file = "Prediction2025-2submission.csv", row.names = FALSE)
