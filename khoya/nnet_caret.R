library(nnet)
library(caret)
library(ggplot2)
library(datasets)

dataset <- read.csv("C:/Users/Shashank/Downloads/Book1.csv")
# Example: Handling missing values
dataset <- na.omit(dataset)

# Example: Encoding categorical variables
encoded_data <- model.matrix(~., data = dataset)

# Example: Splitting the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(dataset), 0.7 * nrow(dataset))
training_data <- dataset[train_indices, ]
testing_data <- dataset[-train_indices, ]

# Example: Selecting relevant features
selected_features <- c("Temp", "Aw","R2")
dataset <- dataset[, selected_features]

# Example: Using cross-validation to find optimal hyperparameters
library(caret)
grid <- expand.grid(.size = c(5, 10, 15), .decay = c(0.01, 0.001))
ctrl <- trainControl(method = "cv", number = 5)
model <- train(R4 ~ Temp + Aw , data = training_data, method = "nnet",
               trControl = ctrl, tuneGrid = grid)

# Example: Fitting the neural network model
model <- nnet(R2~ Temp + Aw , data = training_data, size = 2, maxit = 10)

# Example: Evaluating the model performance on the testing data
predictions <- predict(model, newdata = testing_data)
accuracy <- mean(predictions == testing_data$R2)

# Example: Adding weight decay for regularization
model <- nnet(R2~ Temp + Aw , data = training_data, size =2, maxit = 10, decay = 0.01)

# Example: Visualizing the learned weights
plotnet(model)
rmse(predictions , testing_data$R2)

mae <- mean(abs(predictions - testing_data$R2))
print(paste("MAE:",mae))

