# Forecast the Airlines Passengers data set. Prepare a document for each model explaining
# how many dummy variables you have created and RMSE value for each model. Finally which model you will use for Forecasting.
# Install required packages.

library(rmarkdown)
library(forecast)
library(readxl)
library(fpp)
library(smooth)

airlines <- read_excel(file.choose())
View(airlines)
attach(airlines)
summary(airlines)
str(airlines)
skim(airlines)

plot(airlines$Passengers)

# Their is a time series of 12 months , so creating 12 dummy variables.
X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0)
View(X)
colnames(X) <- month.abb
View(X)
airlines1 <- cbind(airlines,X)
View(airlines1)
airlines1["t"]<- 1:96   # Creating t values.
airlines1["log_passengers"]<-log(airlines1$Passengers) # creating log of passengers.
airlines1["t_square"]<-airlines1["t"]*airlines1["t"]  # Creating t2 value.
attach(airlines1)
View(airlines1)

# Creating test and training data.
train <-airlines1[1:84,]
test <- airlines1[85:96,]

########################### LINEAR MODEL #############################

linear_model<-lm(Passengers ~ t, data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Passengers - linear_pred$fit)^2,na.rm = T))
rmse_linear # 53.199
fc <- forecast(linear_model, newdata = test)
plot(fc)

######################### Exponential #################################

expo_model<-lm(log_passengers~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Passengers - exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 46.057

######################### Quadratic ####################################

Quad_model<-lm(Passengers~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 48.051

######################### Additive Seasonality #########################

sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 132.81

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Passengers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 35.34

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 26.36

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 140.06

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 10.51

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Multiplicative Seasonality Linear trend has least RMSE value

new_model <- lm(log_passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data =airlines1)
new_model
new_model_pred<-data.frame(predict(new_model,newdata=airlines1,interval='predict'))
new_model_fin <- exp(new_model$fitted.values)

# Ploting ACF on residuals.
resid <- residuals(new_model)
resid[1:10]
windows()
acf(resid,lag.max = 10)
# By principal of parcimony we will consider lag - 1  as we have so many significant lags.
# Building Autoregressive model on residuals consider lag-1.

AR1 <- arima(resid, order=c(1,0,0))
str(AR1)

View(data.frame(res=resid,newresid=AR1$residuals))
windows()
acf(AR1$residuals,lag.max = 15)
pred_res<- predict(arima(AR1$residuals,order=c(1,0,0)),n.ahead = 12)
str(pred_res)
pred_res$pred
acf(AR1$residuals)
hist(AR1$residuals)
qqnorm(AR1$residuals)
qqline(AR1$residuals)

write.csv(airlines1,file="airlines.csv",col.names = F,row.names = F)
getwd()
####################### Predicting new data #############################
library(readxl)
test_data<-read_excel(file.choose(),1)
View(test_data)
pred_new<-data.frame(predict(new_model,newdata=test_data,interval = 'predict'))
View(pred_new)
pred_new$fit <- pred_new$fit+pred_res$pred
View(pred_new)
exp(pred_new)
#fc1 <- forecast(new_model, newdata = test_data)
#plot(fc1)
#autoplot(fc1) +ggtitle("Forecasts of ridership using regression") +xlab("index") + ylab("passengers")
#######################################################
