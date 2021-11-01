#11920031- Dhruv Cairae

install.packages("TSA")
install.packages("xts")
install.packages("timeSeries")
install.packages("vcdExtra")
install.packages("spatialEco")
library("imputeTS")
library(forecast)
library(fpp2)
library(wavelets)
library(zoo)
library(ggpubr)
library(TSA)
library(xts)
library(timeSeries)
library("vcdExtra")
library(readxl)
library("boot")
library(spatialEco)
library("Hmisc")
library(tseries)
library("hts")
library(bptest)

sales<- read_excel("C:\\Users\\Dhruv Cairae\\Documents\\SouvenirSales.xlsx")
sales
sales.ts <- ts(sales$Sales, start = c(1995,1), frequency = 12)
sales.ts

autoplot.zoo((sales.ts))

#Model A
train <- window(sales.ts,end=c(1998,12), frequency=12)
test <- window(sales.ts,start=c(1999,1), frequency=12)

fit <- tbats(train) 
seasonal <- !is.null(fit$seasonal)
seasonal

train.linear.trend <- tslm(train ~ trend+season)  
train.lm.pred <- forecast(train.linear.trend, h=length(test ), level = 0) 
plot(train.lm.pred)
lines(train.linear.trend$fitted,lwd=2)


summary(train.linear.trend )  
summary(train.lm.pred )
accuracy(train.lm.pred$mean,test)
plot(train.lm.pred$residuals)


#ModelB

train.exponential.trend <- tslm(train ~trend+season,lambda = 0)
train.exp.pred <- forecast(train.exponential.trend, h=length(test ),level=0)


summary(train.exponential.trend)  
summary(train.exp.pred)

accuracy(train.exp.pred,test)

plot(train.exp.pred$residuals)

#Forecasting Model
entire <- window(sales.ts,end=c(2001,12), frequency=12)
entire.exponential.trend <- tslm(entire ~trend+season,lambda = 0)
summary(entire.exponential.trend)
future <- window(sales.ts,end=c(2002,12), frequency=12)
future.exp <- forecast(entire.exponential.trend, h=length(future ),level=0)
future.exp

# AR
adf.test(train.exponential.trend$residuals)

Acf(train.exponential.trend$residuals,lag.max =  20)
Pacf(train.exponential.trend$residuals,lag.max =  20)
a=arima(train.exp.pred$residuals,order = c(2,0,0))
a
b=auto.arima(train.exponential.trend$residuals)
b

automodel <- auto.arima(entire)
forecast.auto <- forecast(automodel,h=24)
forecast.auto

