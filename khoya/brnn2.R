#brnn 
library(brnn)
library(ggplot2)
library(dplyr)
library(Matrix)
dataset<- read.csv("C:/Users/Shashank/Downloads/Book1.csv")

set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(dataset), 0.75 * nrow(dataset))  # 75% for training
train_set <- dataset[train_indices, ]
test_set <- dataset[-train_indices, ]

input_vars <- subset(train_set, select = c("Temp", "Aw","R2"))  # Replace feature1, feature2, ... with your actual feature names
output_var <- train_set$R2  # Replace target_column with the actual name of your target column

brnn_model <- brnn( R2 ~ Temp + Aw, data = input_vars, hidden=c(10,5),lrnrate=0.01,epochs = 1000, neurons=100)

new_data <- subset(test_set, select = c("Temp", "Aw"))  # Replace feature1, feature2, ... with your actual feature names
predictions <- predict(brnn_model, newdata = new_data)

rmse(test_set$R2 ,predictions)

mae <- mean(abs(predictions - test_set$R2))
print(paste("MAE:",mae))

plot(predictions)

actual_values <- test_set$R2  # Replace target_column with the actual name of your target column
prediction_df <- data.frame(Actual = actual_values, Predicted = predictions)

ggplot(prediction_df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(color = "red", linetype = "dashed") +
  labs(x = "Actual", y = "Predicted") +
  ggtitle("Actual vs. Predicted Values")


input_features <- data.frame(Temp = 35, Aw = 0.6)

# Evaluate the target value for the input features
predicted_value <- neuralnet::compute(nn_model, input_features)$net.result

print(predicted_value)
