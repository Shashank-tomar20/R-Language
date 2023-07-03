library(nnet)
library(ggplot2)
library(Metrics)
mydata <- read.csv("C:/Users/Shashank/Downloads/Book1.csv")

set.seed(123)  
train_indices <- sample(1:nrow(mydata), 0.7 * nrow(mydata))  # 70% for training
train_set <- mydata[train_indices, ]
test_set <- mydata[-train_indices, ]

input_vars <- subset(train_set, select = c("Temp", "Aw","R2"))  
output_var <- train_set$R2 

model <- nnet(R2 ~ Temp + Aw , data = train_set, size = 1, decay = 0.01, maxit = 100)

predictions <- predict(model, test_set)
plot(predictions)

rmse(test_set$R2,predictions)
mae <- mean(abs(predictions - test_set$R2))
print(paste("MAE:",mae))

input_features <- data.frame(Temp = 40, Aw = 0.6)

# Evaluate the target value for the input features
predicted_value <- predict(model, newdata=input_features)

print(predicted_value)

actual_values <- test_set$R2  # Replace target_column with the actual name of your target column
prediction_df <- data.frame(Actual = actual_values, Predicted = predictions)

ggplot(prediction_df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(color = "red", linetype = "dashed") +
  labs(x = "Actual", y = "Predicted") +
  ggtitle("Actual vs. Predicted Values")

input_features <- data.frame(Temp = 35, Aw = 0.6)

# Evaluate the target value for the input features
predicted_value <- nnet::compute(model, input_features)$net.result

print(predicted_value)