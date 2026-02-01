# Load the dataset
data <- read.csv("Prediction20251Train.csv")
test <- read.csv("Prediction20251TestStudents.csv")
submission <- read.csv("submission2025-1.csv")

# Visualizations to analyze the data
colors <- c("red", "green", "blue", "orange", "yellow")

boxplot(data$Score ~ data$Grade, main = "Score by Grade", xlab = "Grade", ylab = "Score", col = colors)
boxplot(data$Participation ~ data$Grade, main = "Participation by Grade", xlab = "Grade", ylab = "Participation", col = colors)

plot(data$Participation, data$Score, col = colors[as.numeric(as.factor(data$Grade))], 
     main = "Participation vs Score", xlab = "Participation", ylab = "Score")

# Predict grades using optimized rules
# Start with a default grade of "F" for everyone
data$predicted_grade <- rep("F", nrow(data))

# Assign "C" where Participation is > 0.2 and Score is at least 60
data$predicted_grade[data$Participation > 0.2 & data$Score >= 60] <- "C"

# Upgrade to "B" where Participation is at least 0.5 and Score is at least 76
data$predicted_grade[data$Participation >= 0.5 & data$Score >= 76] <- "B"

# Finally, upgrade to "A" where Participation is at least 0.45 and Score is at least 91
data$predicted_grade[data$Participation >= 0.45 & data$Score >= 91] <- "A"


# Accuracy calculation
table <- table(data$predicted_grade, data$Grade)
accuracy <- sum(diag(table)) / nrow(data)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

# Analyze mismatches
mismatches <- data[data$predicted_grade != data$Grade,]
print("Mismatched Cases:")
print(mismatches)

# Visualizing the predicted grades
grade_colors <- rep("yellow", nrow(data))
grade_colors[data$predicted_grade == 'A'] <- 'red'
grade_colors[data$predicted_grade == 'B'] <- 'green'
grade_colors[data$predicted_grade == 'C'] <- 'blue'
grade_colors[data$predicted_grade == 'D'] <- 'orange'

plot(data$Participation, data$Score, col = grade_colors, main = "Participation vs Score (Predicted)", 
     xlab = "Participation", ylab = "Score")

# Predict grades for the test dataset

# Start with a default grade of "F" for everyone in the test set
test$predicted_grade <- rep("F", nrow(test))

# Apply the same rules as used for the training data:
# 1. Assign "C" where Participation > 0.2 and Score is at least 60
test$predicted_grade[test$Participation > 0.2 & test$Score >= 60] <- "C"

# 2. Upgrade to "B" where Participation is at least 0.5 and Score is at least 76
test$predicted_grade[test$Participation >= 0.5 & test$Score >= 76] <- "B"

# 3. Finally, upgrade to "A" where Participation is at least 0.45 and Score is at least 91
test$predicted_grade[test$Participation >= 0.45 & test$Score >= 91] <- "A"

# Update the submission file with test predictions

# Assuming your submission file has two columns: ID and Grade,
# copy the predicted grades from the test data to the submission file.
submission$Grade <- test$predicted_grade

# Save the updated submission file

write.csv(submission, "submission2025-1.csv", row.names = FALSE)