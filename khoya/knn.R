library(class)
library(caret)
library(ggplot2)
library(dplyr)

mydata <- read.csv("C:/Users/Shashank/Downloads/Book1.csv")

set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(mydata), 0.7 * nrow(mydata))  # 70% for training
train_set <- mydata[train_indices, ]
test_set <- mydata[-train_indices, ]

input_vars <- subset(train_set, select = c(2:3,7))  # Replace feature1, feature2, ... with your actual feature names
output_var <- train_set$R4  # Replace target_column with the actual name of your target column

k <- 5  # Set the value of K
knn_model <- knn(train = input_vars, test = input_vars, cl = output_var, k = k)

new_data <- subset(test_set, select = c(2:3,7))  
predictions <- knn(train = input_vars, test = new_data, cl = output_var, k = k)

plot(predictions)
plot(knn_model)

actual_values <- test_set$R4  # Replace target_column with the actual name of your target column
prediction_df <- data.frame(Actual = actual_values, Predicted = predictions)

ggplot(prediction_df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(color = "red", linetype = "dashed") +
  labs(x = "Actual", y = "Predicted") +
  ggtitle("Actual vs. Predicted Values")

predicted_classes <- ifelse(predictions >0.5, 1, 0)  # Adjust the threshold based on your specific problem
actual_classes <- test_set$R4 # Replace target_column with the actual name of your target column
accuracy <- sum(predicted_classes == actual_classes) / length(actual_classes)

RMSE(predictions,test_set$R4)
summary(knn_model)
summary(mydata)
mae <- mean(abs(predictions - test_set$R4))
print(paste("MAE:",mae))

summary(knn_model)
