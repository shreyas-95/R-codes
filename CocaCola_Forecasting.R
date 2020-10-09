# Forecast the CocaCola prices data set. Prepare a document for each model explaining 
# how many dummy variables you have created and RMSE value for each model. Finally which model you will use for Forecasting.
# Install required packages.

library(rmarkdown)
library(forecast)
library(fpp)
library(smooth)
library(readxl)

cocacola <- read_excel(file.choose())
View(cocacola)
attach(cocacola)
summary(cocacola)
str(cocacola)
skim(cocacola)

plot(cocacola$Sales)

# Data has quarter time series data, so create 4 dummy variables.
Q1 <- ifelse(grepl("Q1",cocacola$Quarter),"1","0")
Q2 <- ifelse(grepl("Q2",cocacola$Quarter),"1","0")
Q3 <- ifelse(grepl("Q3",cocacola$Quarter),'1','0')
Q4 <- ifelse(grepl("Q4",cocacola$Quarter),'1','0')

cocacola1 <- cbind(cocacola,Q1,Q2,Q3,Q4)
View(cocacola1)
attach(cocacola1)
cocacola1["t"] <- 1:42
cocacola1["log_sales"] <- log(cocacola1$Sales)
cocacola1["t_square"] <- cocacola1$t*cocacola1$t
View(cocacola1)

# creating training and test data.
train <- cocacola1[1:36,]
test <- cocacola1[37:40,]

########################### LINEAR MODEL #############################

linear_model<-lm(Sales~t,data = train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear <- sqrt(mean((test$Sales - linear_pred$fit)^2,na.rm=T))
rmse_linear # 644.01
fc <- forecast(linear_model, newdata = test)
plot(fc)

######################### Exponential #################################

expo_model<-lm(log_sales~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales - exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 524.73

######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 434.71

######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~Q1+Q2+Q3+Q4,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 1785.135

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Sales~t+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 534.69

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 236.70

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_sales~Q1+Q2+Q3+Q4,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 1871.203

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_sales~t+Q1+Q2+Q3+Q4,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 335.1026

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Additive Seasonality with Quadratic trend  has least RMSE value

new_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=cocacola1)
new_model_pred<-data.frame(predict(new_model,newdata=cocacola1,interval='predict'))
rmse_new_model<-sqrt(mean((test$Sales-new_model_pred$fit)^2,na.rm=T))
rmse_new_model # 1771.263
new_model_fin <- new_model$fitted.values
View(new_model_fin)

pred_res<- predict(arima(sales,order=c(1,0,0)),n.ahead = 12)
Quarter <- as.data.frame(cocacola1$Quarter)

Final <- as.data.frame(cbind(Quarter,cocacola1$Sales,new_model_fin))
colnames(Final) <-c("Quarter","Sales","New_Pred_Value")

# Plots 
acf(AR1$residuals)
hist(AR1$residuals)
qqnorm(AR1$residuals)
qqline(AR1$residuals)
plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Quarter",col.axis="Green",type="s")
plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Quarter",col.axis="blue",type="o") 

#####################################################################################################