# Load necessary libraries
library(caret)
library(ggplot2)

# Load the wine quality dataset
data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", header = TRUE, sep = ";")

# Explore the dataset
summary(data)
head(data)

# Preprocess the data
# Ensure 'quality' column is present and properly formatted
if (!"quality" %in% colnames(data)) {
  stop("Column 'quality' not found in the dataset.")
}

# Split the dataset into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data$quality, p = .8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Train a predictive model
model <- train(quality ~ ., data = trainData, method = "lm")

# Make predictions
predictions <- predict(model, newdata = testData)

# Evaluate the model
rmse <- sqrt(mean((predictions - testData$quality)^2))
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Create a Line Graph for Predicted vs. Actual Wine Quality
df <- data.frame(Actual = testData$quality, Predicted = predictions)

ggplot(df, aes(x = 1:nrow(df))) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  labs(title = "Predicted vs. Actual Wine Quality",
       x = "Observations",
       y = "Wine Quality") +
  theme_minimal()

# Visualize Residuals
residuals <- testData$quality - predictions
ggplot() +
  geom_histogram(aes(x = residuals), bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Residuals",
       x = "Residuals",
       y = "Frequency")

# Additional Details
cat("Intercept:", coef(model$finalModel)[1], "\n")
cat("Coefficients:\n", coef(model$finalModel)[-1], "\n")


