# Author:     Changce Lv
# Email:      thisislcc@mail.ustc.edu.cn
# Date:       2024-04-04
#
# --------------------------------------
# Step 1: Load required libraries
library(caret)
library(randomForest)
library(ggplot2)

# Step 2: Load the dataset (assuming 'data' is your dataframe)
data <- mtcars

# Step 3: Data preparation and pre-process
# In this example, we'll perform scaling of numeric features
numeric_features <- sapply(data, is.numeric)
data_scaled <- data
data_scaled[, numeric_features] <- scale(data_scaled[, numeric_features])

# Step 4: Feature selection and visualization
# We'll use variable importance plot to visualize feature importance
set.seed(123)
model <- randomForest(mpg ~ ., data = data_scaled)
varImpPlot(model)

# Step 5: Split the data into training and testing sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data$mpg, p = 0.8, list = FALSE)
trainData <- data_scaled[trainIndex, ]
testData <- data_scaled[-trainIndex, ]

# Step 6: Train and tune the Random Forest model
# Here, we'll use 5-fold cross-validation to tune the model
ctrl <- trainControl(method = "cv", number = 5)
tuneGrid <- expand.grid(mtry = c(2, 3, 4))  # Tuning parameters
model <- train(mpg ~ ., data = trainData, method = "rf", trControl = ctrl, tuneGrid = tuneGrid)

# Step 7: Evaluate the model
print(model)

# Step 8: Make predictions and evaluate performance on test data
predictions <- predict(model, testData)
rmse <- RMSE(predictions, testData$mpg)
print(paste("Root Mean Squared Error (RMSE):", rmse))
