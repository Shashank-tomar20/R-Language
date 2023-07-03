library(caret)
library(randomForest)
library(fields)
library(ggplot2)
library(caTools)
library(dplyr)

mydata <- read.csv("C:/Users/Shashank/Downloads/Book1.csv")
newdata1 <- subset(mydata,select=c(-1))

split1 <- sample.split(newdata1$Temp,SplitRatio = 0.7)
split1

train_data <- subset(newdata1,split="TRUE") 
test_data <- subset(newdata1,split="FALSE")

set.seed(123)
input_vars <- subset(train_data,select=c("Temp","Aw"))
output_var <- train_data$R4

rf_model <- randomForest(x=input_vars,y=output_var)

data_new<- subset(test_data,select=c("Temp","Aw"))
predictions <- predict(rf_model, newdata=data_new)

actual_values <- test_data$R4
accuracy <- sum(predictions==actual_values)/length(actual_values)
RMSE(predictions , test_data$R4)


plot(rf_model)

mae <- mean(abs(predictions - test_data$R4))
print(paste("MAE:",mae))

