library(neuralnet)
library(dplyr)
library(ggplot2)
library(MASS)
library(caTools)
library(Metrics)
mydata <- read.csv("C:/Users/Shashank/Downloads/Book1.csv")

set.seed(123)  
train_indices <- sample(1:nrow(mydata), 0.7 * nrow(mydata))  # 70% for training
train_set <- mydata[train_indices, ]
test_set <- mydata[-train_indices, ]

input_vars <- subset(train_set, select = c("Temp", "Aw","R2"))  
output_var <- train_set$R2
nn_model <- neuralnet( R2 ~ Temp + Aw, data = input_vars, hidden =c(5,2),stepmax = 1e6)

new_data <- subset(test_set, select = c("Temp", "Aw"))  # Replace feature1, feature2, ... with your actual feature names
predictions<- neuralnet::compute(nn_model, new_data)$net.result
rmse(test_set$R2 ,predictions)

plot(predictions)
plot(nn_model)

mae <- mean(abs(predictions - test_set$R2))
print(paste("MAE:",mae)) 

actual_values <- test_set$R2  # Replace target_column with the actual name of your target column
prediction_df <- data.frame(Actual = actual_values, Predicted = predictions)

ggplot(prediction_df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(color = "red", linetype = "dashed") +
  labs(x = "Actual", y = "Predicted") +
  ggtitle("Actual vs. Predicted Values")

predicted_classes <- ifelse(predictions >= 0.5, 1, 0)  # Adjust the threshold based on your specific problem
actual_classes <- test_set$R2 # Replace target_column with the actual name of your target column
accuracy <- sum(predicted_classes == actual_classes) / length(actual_classes)

mae <- mean(abs(predictions - test_set$R2))
print(paste("MAE:",mae))


# Create a data frame with specific feature values
input_features <- data.frame(Temp = 40, Aw = 0.6)

# Evaluate the target value for the input features
predicted_value <- neuralnet::compute(nn_model, input_features)$net.result

print(predicted_value)

