# Prepare a classification model using SVM for salary data.
# Install required packages.
library(kernlab)
library(caret)
library(plyr)
library(ggplot2)
library(psych)
library(e1071)

salary_train <- read.csv(file.choose())
View(salary_train)
attach(salary_train)
summary(salary_train)
skim(salary_train)
str(salary_train)
salary_train$educationno <- as.factor(salary_train$educationno)

salary_test <- read.csv(file.choose())
View(salary_test)
attach(salary_test)
summary(salary_test)
skim(salary_test)
str(salary_test)
salary_test$educationno <- as.factor(salary_test$educationno)

data.table(table(salary_train$educationno))
prop.table(table(salary_train$educationno))
?ksvm

#  Different types of kernels. 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

# Building the SVM model.
# Kernal Method = vanilldot.
model1 <- ksvm(salary_train$Salary~., data=salary_train,kernel="vanilladot")
model1
predict1 <- predict(model1,salary_test)
predict1
table(predict1,salary_test$Salary)
agreement1 <- predict1==salary_test$Salary
table(agreement)
prop.table(table(agreement))
mean(predict1==salary_test$Salary)*100  #Accuracy = 84.64.

# kernel Method = rbfdot.
model2 <- ksvm(salary_train$Salary~., data=salary_train,kernel="rbfdot")
model2
predict2 <- predict(model2,salary_test)
predict2
table(predict2,salary_test$Salary)
agreement2 <- predict2==salary_test$Salary
table(agreement)
prop.table(table(agreement))
mean(predict2==salary_test$Salary)*100  # accuracy = 85.205

# kernel Method = polydot.
model3 <- ksvm(salary_train$Salary~.,data=salary_train,kernal="polydot")
model3
predict3 <- predict(model3,salary_test)
predict3
table(predict3,salary_test$Salary)
agreement3 <- predict3==salary_test$Salary
table(agreement3)
prop.table(table(agreement3))
mean(predict3==salary_test$Salary)  # accuracy = 85.20

# kernel method = tanhdot.
model4 <- ksvm(salary_train$Salary~.,data=salary_train,kernal="tanhdot")
model4
predict4 <- predict(model4,salary_test)
predict4
table(predict4,salary_test$Salary)
agreement4 <- predict4==salary_test$Salary
table(agreement4)
prop.table(table(agreement4))
mean(predict4==salary_test$Salary) # accuracy = 85.20584

# kernel method = laplacedot.
model5 <- ksvm(salary_train$Salary~.,data=salary_train,kernal="laplacedot")
model5
predict5 <- predict(model5,salary_test)
predict5
table(predict5,salary_test$Salary)
agreement5 <- predict5==salary_test$Salary
table(agreement5)
prop.table(table(agreement5))
mean(predict5==salary_test$Salary)  # accuracy = 85.205

#kernel method = anovadot
model6 <- ksvm(salary_train$Salary~.,data=salary_train,kernal="anovadot")
model6
predict6 <- predict(model6,salary_test)
predict6
table(predict6,salary_test$Salary)
agreement6 <- predict6==salary_test$Salary
table(agreement6)
prop.table(table(agreement6))
mean(predict6==salary_test$Salary)  # accuracy = 85.192
