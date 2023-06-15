data("iris")
str(iris)
library(ggplot2)
qplot(Petal.Length,Petal.Width,data=iris,color=Species)
library(e1071)
mymodel <- svm(Species~.,data = iris,jernel ="radial")
summary(mymodel)
plot(mymodel,data=iris,Petal.Width~Petal.Length , slice = list(Sepal.Width = 3 ,Sepal.Length=4))

#Confusion Matrix and Misclassification Error
pred <- predict(mymodel , iris)
tab <- table(Predicted=pred , Actual = iris$Species)
tab

#error
1-sum(diag(tab))/sum(tab)
#linear basis
model <- svm(Species~. , data = iris, kernel = "linear")
plot(model,data=iris ,Sepal.Width~Sepal.Length , slice = list(Petal.Width= 4 , Petal.Length=3))
#confusion matrix 
pred <-predict(model,iris)
tab <- table(Predicted = pred, Actual = iris$Species)
tab

1-sum(diag(tab))/sum(tab)
