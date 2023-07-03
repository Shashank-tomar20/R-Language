library(neuralnet)
library(dplyr)
library(caTools)
library(ggplot2)

dataset <- read.csv("C:/Users/Shashank/Downloads/Book1.csv")

set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(dataset), 0.7 * nrow(dataset))  # 70% for training
train_set <- dataset[train_indices, ]
test_set <- dataset[-train_indices, ]

input_vars <- subset(train_set, select = c("Temp", "Aw","R4"))  # Replace feature1, feature2, ... with your actual feature names
output_var <- train_set$R4  # Replace target_column with the actual name of your target column

brnn_model <- neuralnet(R4 ~ Temp + Aw , data = input_vars, hidden = c(7,4), act.fct = "tanh", linear.output = TRUE, algorithm = "backprop", learningrate = 0.02)

new_data <- subset(test_set, select = c("Temp","Aw","R4"))  # Replace feature1, feature2, ... with your actual feature names
predictions <- neuralnet::compute(brnn_model, new_data)

mae <- mean(abs(predictions - test_set$R4))
print(paste("MAE:",mae))

RMSE(predictions, test_set$R4)

summary(brnn_model)