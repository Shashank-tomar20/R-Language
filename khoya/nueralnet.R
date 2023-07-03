# Step 1: Install and load the required packages
library(metrix)
library(neuralnet)


mydata <- read.csv("C:/Users/Shashank/Downloads/Book1.csv")
# Step 2: Prepare your data (assuming you have a dataset named 'khoya_data' with columns: temperature, water_activity, and emc)
# Normalize the input features (temperature and water activity)
mydata$Temp <- scale(mydata$Temp)
mydata$Aw <- scale(mydata$Aw)

# Step 3: Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(mydata), 0.7 * nrow(mydata))  # 70% for training
train_data <- mydata[train_indices, ]
test_data <- mydata[-train_indices, ]

# Step 4: Define the neural network architecture
formula <- as.formula("R2 ~ Temp + Aw")  # Define the formula

# Step 5: Train the neural network
model <- neuralnet(
  formula,
  data = train_data,
  hidden = c(10, 5),  # Number of nodes in hidden layers (you can adjust as per your requirements)
  linear.output = TRUE,  # Set to TRUE since EMC is a continuous variable
  lifesign = "minimal"  # Show minimal training progress information
)

# Step 6: Make predictions
predictions <- neuralnet::compute(model, test_data[, c("Temp", "Aw")])  # Use test data for prediction
predicted_emc <- predictions$net.result

# Step 7: Evaluate the model
# Calculate mean squared error (MSE)
mse <- mean((predicted_emc - test_data$R2)^2)
print(paste("Mean Squared Error (MSE):", mse))
rmse(test_data$R2 , predicted_emc)
mae <- mean(abs(predicted_emc - test_data$R2))
print(paste("MAE:",mae))

