# Load libraries
library(ggplot2)
library(dplyr)
library(corrplot)
library(purrr)

# --- Load and merge datasets ---
train <- read.csv('Vapnik25STrain.csv')
test  <- read.csv('Vapnik25STestStudents.csv')
uniData <- read.csv('UniversityData.csv')
train <- merge(train, uniData, by = "Uni")
test  <- merge(test, uniData, by = "Uni")

# --- Visualization 1: Tuition Distribution by Private Status ---
ggplot(train, aes(x = Private, y = Tuition)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Tuition Distribution by Private Status", x = "Private University", y = "Tuition")

# --- Visualization 2: Salary vs GPA by Location ---
ggplot(train, aes(x = GPA, y = Salary, color = Location)) +
  geom_point() +
  labs(title = "Salary vs GPA by Location", x = "GPA", y = "Salary")

# --- Visualization 3: Average Salary by University Type ---
train %>%
  group_by(Private) %>%
  summarise(AverageSalary = mean(Salary)) %>%
  ggplot(aes(x = Private, y = AverageSalary, fill = Private)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Salary by University Type", x = "Private University", y = "Average Salary")

# --- Visualization 4: Correlation Heatmap ---
encoded_data <- model.matrix(~ Location + Private, data = train)[, -1]
numeric_data <- cbind(train %>% select(GPA, Grad, Salary, Tuition), encoded_data)
cor_matrix <- cor(numeric_data)
corrplot(cor_matrix, method = "color", type = "upper", addCoef.col = "black", tl.cex = 0.8)

# --- Visualization 5: Salary vs Graduation Year by Private Status ---
ggplot(train, aes(x = Grad, y = Salary, color = Private)) +
  geom_point() +
  labs(title = "Salary vs Graduation Year", x = "Graduation Year", y = "Salary")

# --- Predict salaries using linear for public, quadratic for private ---
predictions <- train %>%
  group_split(Uni) %>%
  map_dfr(function(df) {
    if (unique(df$Private) == "Yes") {
      model <- lm(Salary ~ Grad + I(Grad^2), data = df)
    } else {
      model <- lm(Salary ~ Grad, data = df)
    }
    df$predicted <- predict(model, newdata = df)
    return(df)
  })

# Update train with predictions
train <- predictions

# --- Compute MSE per university ---
mse_per_uni <- train %>%
  group_by(Uni) %>%
  summarise(MSE = mean((Salary - predicted)^2))

# --- Visualization 6: MSE Bar Chart ---
ggplot(mse_per_uni, aes(x = reorder(Uni, -MSE), y = MSE)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "Mean Squared Error per University (Quadratic for Private)", x = "University", y = "MSE") +
  theme_minimal()

# --- Visualization 7: Actual vs Predicted by University ---
ggplot(train, aes(x = Grad, y = Salary, color = Uni)) +
  geom_point(alpha = 0.6) +
  geom_line(aes(y = predicted), size = 1) +
  labs(title = "Actual vs Predicted Salary by University", x = "Graduation Year", y = "Salary") +
  theme_minimal()

# --- Visualization 8: Salary vs GPA for Private Universities Only ---
ggplot(train %>% filter(Private == "Yes"), aes(x = GPA, y = Salary, color = Uni)) +
  geom_point() +
  labs(title = "Salary vs GPA for Private Universities", x = "GPA", y = "Salary") +
  theme_minimal()

# --- Student-level error table for Private Universities ---
private_errors <- train %>%
  filter(Private == "Yes") %>%
  mutate(
    Error = Salary - predicted,
    SquaredError = Error^2
  ) %>%
  arrange(desc(SquaredError)) %>%
  select(Uni, GPA, Grad, Salary, Predicted = predicted, Error, SquaredError)
print(private_errors)

# --- Interpolation for Private Universities (connect-the-dots) ---
private_interp <- train %>%
  filter(Private == "Yes") %>%
  group_split(Uni) %>%
  purrr::map_dfr(function(df) {
    df <- df[order(df$Grad), ]
    interp_fun <- approxfun(df$Grad, df$Salary, method = "linear", rule = 2)
    df$pred_interp <- interp_fun(df$Grad)
    return(df)
  })

# --- Compute MSE per private university using interpolation ---
private_mse_interp <- private_interp %>%
  group_by(Uni) %>%
  summarise(MSE_interp = mean((Salary - pred_interp)^2)) %>%
  arrange(desc(MSE_interp))
print(private_mse_interp)

# --- Visualization: MSE per Private University (Interpolation) ---
ggplot(private_mse_interp, aes(x = reorder(Uni, MSE_interp), y = MSE_interp)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Private Universities: MSE of Linear Interpolation Model",
    x = "University",
    y = "Mean Squared Error"
  ) +
  theme_minimal()

# === FINAL BLOCK: Predict on Test Set and Write Submission ===

# --- Apply the same model to the test set and predict salaries for ALL students ---
test_predictions <- test %>%
  group_split(Uni) %>%
  map_dfr(function(df) {
    train_uni <- train %>% filter(Uni == unique(df$Uni))
    
    # Fallback if university not in training set
    if (nrow(train_uni) == 0) {
      fallback_model <- lm(Salary ~ Grad, data = train)
      df$Predicted <- predict(fallback_model, newdata = df)
      return(df)
    }
    
    # Apply private or public model
    if (unique(df$Private) == "Yes") {
      model <- lm(Salary ~ Grad + I(Grad^2), data = train_uni)
    } else {
      model <- lm(Salary ~ Grad, data = train_uni)
    }
    
    df$Predicted <- predict(model, newdata = df)
    return(df)
  })

# --- Write predictions to submission.csv ---
submission <- test_predictions %>%
  select(ID, Predicted)

write.csv(submission, "submission.csv", row.names = FALSE)
