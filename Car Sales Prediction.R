rm(list=ls())
library(readxl)
library(car)
car_sales=read_excel("6304 Module 7 Assignment Data.xlsx",sheet="Quebec Car Sales" ,skip=3)
names(car_sales)[2] <- "sales"
car_sales$year=as.numeric(format(car_sales$yrmo,'%Y'))
car_sales$month=as.numeric(format(car_sales$yrmo,'%m'))
car_sales$item = seq(1,108)
attach(car_sales)
View(car_sales)
#1.
#Basic time series regression model
reg.out=lm(sales~item,data=car_sales)
summary(reg.out)
plot(car_sales$item,reg.out$residuals,type="o",pch=19,main="Residual Plot")
abline(0,0,lwd=3,col="blue")
plot(car_sales$item,car_sales$sales,type="o",lwd=3,main="Actual vs Fitted")
points(car_sales$item,reg.out$fitted.values,type="l",col="red",lwd=3)
cor(car_sales$sales,reg.out$fitted.values)

#LINE Check 
par(mfrow=c(2,2))
plot(reg.out)

# Durbin Watson test for Autocorrelation
# Testing for the "I" in "LINE"

durbinWatsonTest(reg.out)

#2.
# Construct a set of Seasonal Indices assuming 12-month cycle
# First Creating a Data Frame to hold them.

seasonal_indices=data.frame(month=1:12,average=0,index=0)
View (seasonal_indices)

for(i in 1:12) 
{  
  count=0
  for(j in 1:nrow(car_sales)) {
    if(i==car_sales$month[j]) {
      seasonal_indices$average[i]=seasonal_indices$average[i]+car_sales$sales[j]
      count=count+1
    }
  }
  
  seasonal_indices$average[i]=seasonal_indices$average[i]/count
  seasonal_indices$index[i]=seasonal_indices$average[i]/mean(car_sales$sales)
  }

# Deseasonalize sales data
for(i in 1:12) 
{     
  for(j in 1:nrow(car_sales)) {
  if(i==car_sales$month[j]) {
    car_sales$deseason[j]=car_sales$sales[j]/seasonal_indices$index[i]
  }
}
}

#3.
# Deseasonalized Time Series Regression Model
deseasonal_reg.out=lm(deseason~item,data=car_sales)
summary(deseasonal_reg.out)
par(mfrow=c(2,2))
plot(deseasonal_reg.out)
plot(car_sales$item,deseasonal_reg.out$residuals,type="o",pch=19,main="Residuals Plot - Seasonalized")
abline(0,0,col="blue",lwd=3)
durbinWatsonTest(deseasonal_reg.out) 
plot(car_sales$item,car_sales$deseason,type="o",lwd=3,main="Deseasonalized Actuals v. Fitted Values")
points(car_sales$item,deseasonal_reg.out$fitted.values,type="l",col="blue",lwd=3)
plot(car_sales$item,rstandard(deseasonal_reg.out),type="o",lwd=3,main="Residuals Plot - Deseasonalized")
abline(0,0,col="blue",lwd=3)

# Reseasonalizing the forecasts and calculating error manually
for(j in 1:nrow(car_sales)) {
  xx=car_sales$month[j]
  car_sales$reseason.y.hat[j]=deseasonal_reg.out$fitted.values[j]*seasonal_indices$index[xx]
  car_sales$reseason.error[j]=car_sales$sales[j]-car_sales$reseason.y.hat[j]
}



# Plot of reseasonalized residuals.
plot(car_sales$item,car_sales$reseason.error,type="o",pch=19,main="Resonalized Errors Plot") 
abline(0,0,col="blue",lwd=3)

# Plot of standardized reseasonalized residuals.

xx=scale(car_sales$reseason.error)
plot(car_sales$item,xx,pch=19,type="o",main="Standardized Reseasonalized Errors Plot")
abline(0,0,col="blue",lwd=3)

plot(car_sales$item,car_sales$sales,type="o",lwd=3,main="Original vs.Reseasonalized")
points(car_sales$item,car_sales$reseason.y.hat,type="l",col="blue",lwd=3)
cor(car_sales$sales,car_sales$reseason.y.hat)


