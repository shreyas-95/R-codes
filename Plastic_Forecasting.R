# Forecast the plastic sales data set. Prepare a document for each model explaining,
# how many dummy variables you have created and RMSE value for each model. Finally which model you will use for Forecasting.
# Install required packages.

library(rmarkdown)
library(forecast)
library(readxl)
library(fpp)
library(smooth)

plastic <- read.csv(file.choose())
View(plastic)
attach(plastic)
summary(plastic)
str(plastic)
skim(plastic)

plot(plastic$Sales)

# creating dummy variables for 12months as their is 12 months timeseries.
X <- data.frame(outer(rep(month.abb,length=60),month.abb,"==")+0)
colnames(X) <- month.abb
View(X)
plastic1 <- cbind(plastic,X)
plastic1["t"] <- 1:60
plastic1["t_square"] <- plastic1$t*plastic1$t
plastic1["log_sales"] <- log(plastic1$Sales)
View(plastic1)

# creating test data and train data.
train <- plastic1[1:48,]
test <- plastic1[49:60,]

########################### LINEAR MODEL #############################

linear_model<-lm(Sales ~ t, data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales - linear_pred$fit)^2,na.rm = T))
rmse_linear # 260.93
fc <- forecast(linear_model, newdata = test)
plot(fc)

######################### Exponential #################################

expo_model<-lm(log_sales~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales - exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 268.69

######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 260.93

######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 235.60

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 135.5536

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 135.55

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 239.65

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 160.68

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Multiplicative Seasonality Linear trend  has least RMSE value

new_model<-lm(log_sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = plastic1)
new_model_pred<-data.frame(predict(new_model,newdata=plastic1,interval='predict'))
## Warning in predict.lm(new_model, newdata = Plasticsdata, interval =
## "predict"): prediction from a rank-deficient fit may be misleading
new_model_fin <- exp(new_model$fitted.values)
View(new_model_fin)

Month <- as.data.frame(plastic1$Month)
Final <- as.data.frame(cbind(Month,plastic1$Sales, new_model_fin))
colnames(Final) <-c("Month","Sales","New_Pred_Value")
View(Final)

# Plots
plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Months",col.axis="blue",type="o") 
# Ploting ACF on residuals.
resid <- residuals(new_model)
resid[1:10]
windows()
acf(resid,lag.max = 10)
# By principal of parcimony we will consider lag - 1  as we have so many significant lags.
# Building Autoreggressive model on residuals consider lag-1.

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

####################################################################################################