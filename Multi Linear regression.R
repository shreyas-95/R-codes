#Prepare a prediction model for profit of 50_startups data.

MLR1 <- read.csv("G:\\ExcelR Material DATA SCIENCE\\Assignments\\Multi Linear Regression\\50_Startups.csv")
View(MLR1)
attach(MLR1)
plot(MLR1)
pairs(MLR1)
str(MLR1)
summary(MLR1)
mod1 <- lm(Profit~.,data = MLR1[,-4])
summary(mod1)
mod2 <- lm(Profit~R.D.Spend+Administration+Marketing.Spend)    #alternative method
summary(mod2)
cor(MLR1[,-4])
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(MLR1[,-4]))    #partial corelation
mod3 <- lm(Profit~R.D.Spend,data = MLR1)
summary(mod3)     # r2 value is better here
mod4 <- lm(Profit~Administration)
summary(mod4)    # pvalue is more than 0.05, r2 value is not good 
library(car)
install.packages("car")
influence.measures(mod1)
influenceIndexPlot(mod1,id.n=2)
influencePlot()
influencePlot(mod1,id.n=2)
avPlots(mod1,id.n=2,id.cex=0.7)
vif(mod1)          #variation inflation factors
avPlot
mod5 <- lm(Profit~.,data = MLR1[-c(49,50),-c(2,4)])
summary(mod5)
install.packages("skimr")
library(skimr)
install.packages("dplyr")
library(dplyr)
install.packages("stats")
library(stats)
install.packages("e1071")



#  Predict Price of the computer


MLR2 <- read.csv(file.choose())
View(MLR2)
attach(MLR2)
str(MLR2)
summary(MLR2)
plot(MLR2)
pairs(MLR2)
cor(MLR2[-c(7,8,9)])
install.packages("dummies")
library(dummies)
cd1 <- ifelse(cd=="no",0,1)    # creation of dummy variable
multi1 <- ifelse(multi=="yes",0,1)
premium1 <- ifelse(premium=="no",0,1)
MLR <- cbind.data.frame(X,price,speed,hd,ram,screen,cd1,multi1,premiun1,ads,trend)
View(MLR)
attach(MLR)
mod11 <- lm(price~.,data = MLR)
summary(mod11)
confint(mod11,level = 0.95)   
predict(mod11,interval = "predict")
pairs(MLR)
mod12 <- lm(price~.,data = MLR[,-1])
summary(mod12)
influence.measures(mod12)
influenceIndexPlot(mod12,id.no=4)
influencePlot(mod12)
vif(mod12)    #variation influence fator
avPlots(mod12,id.no=3,id.cex=0.7)
mod13 <- lm(price~.,data = MLR[-c(1441,1701)])    #this model has a better r2 value
summary(mod13)




#consider only the below columns and prepare a prediction model for predicting Price.
#Corolla<-Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]


MLR3 <- read.csv("G:\\ExcelR Material DATA SCIENCE\\Assignments\\Multi Linear Regression\\ToyotaCorolla.csv")
View(MLR3)
MLR3 <- MLR3[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(MLR3)
attach(MLR3)
str(MLR3)
summary(MLR3)
pairs(MLR3)
cor(MLR3)
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(MLR3))
model1 <- lm(Price~.,data = MLR3)
summary(model1)
modelcc <- lm(Price~Doors)
summary(modelcc)    #r2 vlue is not good for cc and doors
model2 <- lm(Price~ cc+Doors)
summary(model2)     #r2 value is not good in all scenerios
influence.measures(model1)
influenceIndexPlot(model1,id.n=4)
influencePlot(model1,id.n=4)
install.packages("car")
library(car)
model3 <- lm(Price~.,data = MLR3[-81,])
summary(model3)
model4 <- lm(Price~.,data = MLR3[-c(81,6)])
summary(model4)
vif(model1)
avplots(model1,id.n=3,id.cex=0.5)
qqplot(model1,id.n =7)
model5 <- lm(Price~.,data = MLR3[-c(,(5,6))])
summary(model5)       #fimal model r2value is 0.8636
library(psych)
install.packages("psych")
pairs.panels(MLR3)


