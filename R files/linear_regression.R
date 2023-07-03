library(MASS)
data("Boston")
View(Boston)

#for data description 
?Boston
#we will split the data into training and testing sets
set.seed(2)
library(caTools)
split <- sample.split(Boston$medv,SplitRatio = 0.7)
split
training_data<- subset(Boston,split=="TRUE")
testing_data<- subset (Boston,split=="FALSE")
#to view the correlation of variables
plot(Boston$crim,Boston$medv,cex=0.5,xlab = "Crime rate",ylab="Price")
cr<-cor(Boston)

#creating scatterplot matrix
attach(Boston)
library(lattice)
splom(-Boston[c(1:6,14)],groups=NULL,data.Boston,axis.line.tck=0,axis.text.alpha=0)
splom(-Boston[c(7:14)],groups=NULL,data.Boston,axis.line.tck=0,axis.text.alpha=0)

# studying rm and medv
plot(rm,medv)
abline(lm(medv-rm),col="red") #regression fit line

#we can corrplot to visualize
library(corrplot)

corrplot(cr,type="lower")
corrplot(cr,method="number")

#finding multicollinearity
library(caret)
Boston_a=subset(Boston,select=-c(medv))
numericData<- Boston_a(sapply(Boston_a,is.numeric))
descrCOr <- cor(numericData)

#creating the model 
model <- lm(medv~.,data = training_data)

summary(model)

predic <- predict (model,testing_data)
predic
#to compare predicted values and actual values we can use plots
plot(testing_data$medv,type="l",lty =1.8 , col = "green")
lines(predic ,type="l",col="blue")


