library(caret)
library(ggplot2)
library(caTools)
library(dplyr)
library(lattice)
mydata <- read.csv("C:/Users/Shashank/Downloads/Book1.csv")
View(mydata)

summary(mydata,)
newdata <- subset(mydata,select = c(-1))
View(newdata)

#splitting mydata
split1 <- sample.split(newdata$Temp,SplitRatio = 0.7)
split1

train_data <- subset(newdata,split="TRUE") 
test_data <- subset(newdata,split="FALSE")

model <- lm(R4 ~ Temp + Aw ,data = train_data)

summary(model)

predic <- predict (model,test_data)
predic

#to compare predicted values and actual values we can use plots
plot(test_data$R4,type="l",lty =1.8 , col = "green")
lines(predic ,type="l",col="blue")

RMSE(predic,test_data$R4)
plot(model)


mae <- mean(abs(predic - test_data$R4))
print(paste("Mae:",mae))

