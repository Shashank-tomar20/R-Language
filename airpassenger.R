data("AirPassengers")
class(AirPassengers)   #ts is time series class
start(AirPassengers)
end(AirPassengers)
frequency(AirPassengers)

summary(AirPassengers)
plot(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers)))  #gives mean of time series analysis
cycle(AirPassengers)
plot(aggregate(AirPassengers,FUN=mean))
boxplot(AirPassengers~cycle(AirPassengers))

plot(diff(log(AirPassengers)))

library(tseries)
adf.test(diff(log(AirPassengers)),alternative = c("stationary","explosive"),)
#ARIMA = Auto regressive indication moving average 
#acf = auto corelation function 

plot(log(AirPassengers))
#AR I MA
#p  d  q
acf(AirPassengers)  #determines the value of q

acf(diff(log(AirPassengers))) #determines the value of p

pacf(diff(log(AirPassengers)))



#lets fit an arima model and predict the future 10 years 
#c(p,d,q)
fit <- arima(log(AirPassengers),c(0,1,1),seasonal=list(order=c(0,1,1),period=12))

pred <- predict(fit,n.ahead = 10*12) # years * months

pred1 <- 2.718 ^ pred$pred  # e value 

ts.plot(AirPassengers,2.718^pred$pred, log ="y",lty =c(1,3))
# dotted line is predicted


#testing our model 
datawide <- ts(AirPassengers,frequency=12,start=c(1949,12),end=c(1959,12))

fit <- arima(log(datawide),c(0,2,1),seasonal=list(order=c(0,1,1),period=12))

pred<- predict(fit,n.ahead = 10*12)

pred1 <- 2.718^pred$pred

data1 <- head(pred1,12)
-
predicted_1960 <- round(data1,digits=0)
#round off

original_1960 <- tail(AirPassengers,12)

ts.plot(AirPassengers,2.718^pred$pred ,log="y",lty=c(1,3))
