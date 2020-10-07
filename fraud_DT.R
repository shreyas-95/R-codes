#Use decision trees to prepare a model on fraud data .
#treating those who have taxable_income <= 30000 as "Risky" and others are "Good".

#Install Packages Required.

install.packages("C50")
install.packages("party")
install.packages("tree")
install.packages("png")
library(C50)
library(tree)
library(party)
library(gmodels)
library(caret)
library(knitr)
library(png)
library(rpart)

fraud_data <- read.csv(file.choose())
View(fraud_data)
attach(fraud_data)
summary(fraud_data)
str(fraud_data)
hist(fraud_data$Taxable.Income)

Risky_good <- ifelse(fraud_data$Taxable.Income<=30000,"Risky","Good")
fraud1 <- data.frame(fraud_data,Risky_good)
View(fraud1)

# Data partition.
set.seed(123)
ind <- sample(2,nrow(fraud1),replace = TRUE,prob = c(0.5,0.5))
fd_train <- fraud1[ind==1,]
fd_test <- fraud1[ind==2,]

# building model using ctree function.
library(rpart)
fit <- rpart(Risky_good~Undergrad + Marital.Status + City.Population + 
               Work.Experience + Urban, data=fd_train)
fit
plot(fit,margin=0.1)
text(fit, use.n = TRUE, pretty = TRUE, cex=0.6)
# Training accuracy
fit.pred1 <- predict(fit, newdata=fd_train, type="class")

mean(fd_train$Risky_good==fit.pred1) # 78.62% Accuracy

fit.perf <- table(fd_train$Risky_good, fit.pred1,dnn=c("Actual", "Predicted"))
fit.perf

library(caret)
confusionMatrix(fit.pred1,fd_train$Risky_good)

# predicting on test data
fit.pred2 <- predict(fit, newdata=fd_test, type="class")
fit.perf2 <- table(fd_test$Risky_good, fit.pred2,dnn=c("Actual", "Predicted"))
fit.perf2
mean(fit.pred2==fd_test$Risky_good) # 78.06% accuracy 
confusionMatrix(fit.pred2,fd_test$Risky_good)
library(gmodels)
# Cross tablez
CrossTable(fd_test$Risky_good,fit.pred2)



