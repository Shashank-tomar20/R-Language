library(brnn)
library(caret)
library(dplyr)
library(ggplot2)
library(Metrics)
mydata <- read.csv("D:/r Language/Banana Peel/banana1.csv")

set.seed(123)  
train_indices <- sample(1:nrow(mydata), 0.7 * nrow(mydata))  # 70% for training
train_set <- mydata[train_indices, ]
test_set <- mydata[-train_indices, ]

input_vars <- subset(train_set, select = c("Temp", "Aw","Xe"))  
output_var <- train_set$Xe

control <- trainControl(method = "repeatedcv", number = 5,repeats = 2)

model <- train(x = input_vars, y = output_var, method = "brnn", trControl = control)

predictions <- predict(model, newdata = test_set)
plot(model)

rmse(test_set$Xe ,predictions)
mae <- mean(abs(predictions - test_set$Xe))
print(paste("MAE:",mae))

actual_values <- test_set$Xe  # Replace target_column with the actual name of your target column
prediction_df <- data.frame(Actual = actual_values, Predicted = predictions)

ggplot(prediction_df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(color = "red", linetype = "dashed") +
  labs(x = "Actual", y = "Predicted") +
  ggtitle("Actual vs. Predicted Values")