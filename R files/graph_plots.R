library(datasets)
head(iris)
plot(iris$Species)#Categorical variable
plot(iris$Petal.Length)#Quantitative
plot(iris$Species,iris$Petal.Width) #cat x quant
plot(iris$Petal.Length,iris$Petal.Width) #Quant pair
plot(iris) #entire data frame

#formula plot with options
plot(dnorm,-4,4,
     col = "#cc3300",
     lwd = 4,
     main = "Standard Normal Distribution",
     xlab = "z-scores",
     ylab ="Density"
     )
detach("packages:datasets",unload =TRUE)
library(datasets)
head(iris)

hist(iris$Petal.Width[iris$Species == "versicolor"],
     xlim = c(0,3),
     breaks=5,
     main = "petal width for versicolor",
     xlab = "versicolor",
     col="green")

 ?lynx
head(lynx)
hist(lynx,
     breaks=14,
     freq = FALSE,
     col = "thistle1",
     main = "",
     xlab = "no. of lynx"
     )

curve(dnorm(x,mean=mean(lynx),sd = sd(lynx)),
      col="thistle4",   #color of curve 
      lwd = 3 ,   #line width of 3 pixels
      add = TRUE) #superimpose on previous graph


#add two kernel density estimators 
lines(density(lynx),col ="blue",lwd = 2)
lines(density(lynx,adjust=3),col ="green",lwd = 2)

#add a rug plot
rug(lynx,lwd = 2 , col = "gray")