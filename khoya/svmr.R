library(e1071)
library(ggplot2)

mydata <- read.csv("C:/Users/Shashank/Downloads/Book1.csv")

set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(mydata), 0.7 * nrow(mydata))
train_set <- mydata[train_indices, ]
test_set <- mydata[-train_indices, ]

input_vars <- subset(train_set, select = c("Aw","Temp"))  
output_var <- train_set$R4  

svm_model <- svm(output_var ~ ., data = input_vars, kernel = "radial")

new_data <- subset(test_set, select = c("Aw" ,"Temp")) 

predictions <- predict(svm_model, newdata = new_data)

plot(predictions)
plot(svm_model)

actual_values <- test_set$R4  # Replace target_column with the actual name of your target column
prediction_df <- data.frame(Actual = actual_values, Predicted = predictions)

ggplot(prediction_df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(color = "red", linetype = "dashed") +
  labs(x = "Actual", y = "Predicted") +
  ggtitle("Actual vs. Predicted Values")

RMSE(predictions, test_set$R4)

mae <- mean(abs(predictions - test_set$R4))
print(paste("MAE: ", mae))

summary(svm_model)
