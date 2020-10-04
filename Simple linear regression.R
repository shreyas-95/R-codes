# Calories_consumed-> predict weight gained using calories consumed

wt.ca <- read.csv(file.choose())
View(wt.ca)
attach(wt.ca)
plot(wt.ca)
hist(Weight.gained..grams.)
hist(Calories.Consumed)
boxplot(wt.ca)
summary(wt.ca) # EDA 
cor <- cor(wt.ca) # corelation
cor
Lreg <- lm(Weight.gained..grams.~Calories.Consumed ) # linear regression model
Lreg
summary(Lreg)
# above model is a good model with r2 value is 0.8968
cint <- confint(Lreg, level = 0.95)
cint
predict(Lreg,interval = "predict")
#log transformation
ltrans <- lm(Weight.gained..grams.~log(Calories.Consumed))
ltrans
summary(ltrans) # in log transformation r2 value is reduced compared to first model.



# Delivery_time -> Predict delivery time using sorting time 

dt.st <- read.csv("G:\\ExcelR Material DATA SCIENCE\\Assignments\\Simple Linear Regression\\delivery_time.csv")
View(dt.st)
attach(dt.st)
plot(dt.st) # positively corelated and linear in nature
pairs(dt.st)
boxplot(dt.st) # their are no outliers
summary(dt.st)
corr <- cor(dt.st)
corr
li_reg <- lm(Delivery.Time~Sorting.Time)
summary(li_reg) # r squared value is less, checking transformation
confint(li_reg, level = 0.95)
predict(li_reg, interval = "predict")
logg <- lm(Delivery.Time~log(Sorting.Time))
summary(logg) #pvalue is >0.05 and r2 value is 0.69
logg1 <- lm(log(Delivery.Time)~Sorting.Time)
summary(logg1) #here r2 value is 0.71 which is better compared to above lm models
confint(logg1, level = 0.95)
predict(logg1, interval = "predict")



# Emp_data -> Build a prediction model for Churn_out_rate 

emp_data <- read.csv(file.choose())
View(emp_data)
attach(emp_data)
plot(emp_data) # negatively corellated
summary(emp_data)
cor(Salary_hike,Churn_out_rate)
model1 <- lm(Salary_hike~Churn_out_rate)
summary(model1) # r squared value is 0.83
model2 <- lm(Salary_hike~log(Churn_out_rate))
summary(model2) # r2 value is 0.87
model3 <- lm(log(Salary_hike)~Churn_out_rate)
summary(model3) # r2 value is 0.84 , hence model2 to has better r2 value and can build good model
confint(model2,level = 0.95)
predict(model2, interval = "predict")



#Salary_hike -> Build a prediction model for Salary_hike


hike <- read.csv("G:\\ExcelR Material DATA SCIENCE\\Assignments\\Simple Linear Regression\\Salary_Data.csv")
View(hike)
attach(hike)
summary(hike)
plot(hike) # positely corelated
cor(hike)
mod1 <- lm(YearsExperience~Salary)
summary(mod1) # r2 value is 0.957
mod2 <-  lm(YearsExperience~log(Salary))
summary(mod2) #r2 value is 0.93
mod3 <-  lm(log(YearsExperience)~Salary)
summary(mod3) #r2 is 0.85 ,hence mod1 as better model
confint(mod1, level = 0.95)
predict(mod1, interval = "predict")

