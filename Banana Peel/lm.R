library(caret)
library(ggplot2)
library(caTools)
library(dplyr)
library(lattice)
newdata <- read.csv("D:/r Language/Banana Peel/banana1.csv")

summary(newdata)
View(newdata)

#splitting mydata
split1 <- sample.split(newdata$Temp,SplitRatio = 0.7)
split1

train_data <- subset(newdata,split="TRUE") 
test_data <- subset(newdata,split="FALSE")

model <- lm("Xe ~ Temp + Aw" ,data = train_data)

summary(model)

predic <- predict (model,test_data)
predic

#to compare predicted values and actual values we can use plots
plot(test_data$Xe,type="l",lty =1.8 , col = "green")
lines(predic ,type="l",col="blue")

RMSE(predic,test_data$Xe)
plot(model)


mae <- mean(abs(predic - test_data$Xe))
print(paste("Mae:",mae))

