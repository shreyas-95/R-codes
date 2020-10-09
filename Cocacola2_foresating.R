# Forecast the CocaCola prices data set. Prepare a document for each model explaining 
# how many dummy variables you have created and RMSE value for each model. Finally which model you will use for Forecasting.
# Install required packages.
library(readxl)
library(forecast)
library(fpp)
library(smooth)
library(tseries)

# Using Arima Model - 
Cocacola<-read_excel(file.choose()) # read the Cocacola data
Cocacola <- Cocacola$Sales
Cocacola <- as.ts(Cocacola)
View(Cocacola)
class(Cocacola)
## [1] "ts"
Cocacola1 <- ts(Cocacola,start=c(1986,1),end=c(1995,6),frequency=4)

start(Cocacola1)
## [1] 1986    1
end(Cocacola1)
## [1] 1996    2
class(Cocacola1)
## [1] "ts"
sum(is.na(Cocacola1))
## [1] 0
summary(Cocacola1)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1548    2160    2782    2994    3609    5253
View(Cocacola1)

# decomdata<- decompose(Cocacola1, "additive")
decomdata<- decompose(Cocacola1, "multiplicative")
plot(decomdata)
plot(decomdata$seasonal)
plot(decomdata$trend)
plot(decomdata$random)

# EDA on the Original Data
plot(Cocacola1)
abline(reg=lm(Cocacola1~time(Cocacola1)))

cycle(Cocacola1)
# Boxplot by Cycle
boxplot(Cocacola1~cycle(Cocacola1,xlab = "Date", ylab = "Passenger Number(100's)",
                        main = "Monthly Boxplot of passengers from 1995 to 2002"))


# Use Auto Arima for the Best Model 
Newmodel <- auto.arima(Cocacola1)
Newmodel

# Use the trace function to understand the determine the best p,d,q values that were selected.
auto.arima(Cocacola1, ic = "aic", trace = TRUE)
plot.ts(Newmodel$residuals)
acf(ts(Newmodel$residuals),main = 'ACF Residual')
pacf(ts(Newmodel$residuals),main = 'PACF Residual')


# Forecast for next 2 year
Pass_Forecast <- forecast(Newmodel,Level=c(95),h=10*12)
## Warning in forecast.Arima(Newmodel, Level = c(95), h = 10 * 12): The non-
## existent Level arguments will be ignored.
plot(Pass_Forecast)


# Test your final model

Box.test(Newmodel$resid, lag = 5, type = "Ljung-Box")

Box.test(Newmodel$resid, lag = 15, type = "Ljung-Box")

Box.test(Newmodel$resid, lag = 10, type = "Ljung-Box")
