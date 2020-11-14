#classify the Size_Categorie using SVM.
# Install required packages.
library(kernlab)
library(caret)
library(plyr)
library(ggplot2)
library(psych)
library(e1071)

forest <- read.csv(file.choose())
View(forest)
attach(forest)
summary(forest)
str(forest)
skim(forest)
hist(forest$area)
rug(forest$area)

# Transform the Area value to Y 
FF1 <- mutate(forest, y = log(area + 1))  # default is to the base e, y is lower case.
hist(FF1$y)

# Normalization of the data.
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
forest$temp = normalize(forest$temp)
forest$RH   = normalize(forest$RH)
forest$wind = normalize(forest$wind)
forest$rain = normalize(forest$rain)
forest$FFMC = normalize(forest$FFMC)
forest$DMC = normalize(forest$DMC)
forest$DC = normalize(forest$DC)
forest$ISI = normalize(forest$ISI)
forest$area = normalize(forest$area)
 
# Partitioning of data.
set.seed(123)
ind <- sample(2,nrow(forest),replace = TRUE, prob = c(0.7,0.3))
train <- forest[ind==1,]
test <- forest[ind==2,]
?ksvm
# Building of SVM model.
# kernel method = vanilladot.
model1 <- ksvm(size_category~.,data=train,kernel="vanilladot")
model1
predict1 <- predict(model1,test)
predict1
table(predict1,test$size_category)
agreement1 <- predict1==test$size_category
table(agreement1)
prop.table(table(agreement1))
mean(predict1==test$size_category)  #accuracy = 86.30

# kernel method = rbfdot.
model2 <- ksvm(train$size_category~.,data=train,kernel="rbfdot")
model2
predict2 <- predict(model2,test)
predict2
table(predict2,test$size_category)
agreement2 <- predict2==test$size_category
table(agreement2)
prop.table(table(agreement2))
mean(predict2==test$size_category)  #accuracy = 70.54

# kernel method = polydot.
model3 <- ksvm(train$size_category~.,data=train,kernel="polydot")
model3
predict3 <- predict(model3,test)
predict3
table(predict3,test$size_category)
agreement3 <- predict3==test$size_category
table(agreement3)
prop.table(table(agreement3))
mean(predict3==test$size_category)  #accuracy = 86.30

# kernel method = tanhdot.
model4 <- ksvm(train$size_category~.,data=train,kernel="tanhdot")
model4
predict4 <- predict(model4,test)
predict3
table(predict4,test$size_category)
agreement4 <- predict4==test$size_category
table(agreement4)
prop.table(table(agreement4))
mean(predict4==test$size_category)  #accuracy = 61.64

# kernel method = laplacedot.
model4 <- ksvm(train$size_category~.,data=train,kernel="laplacedot")
model4
predict4 <- predict(model4,test)
predict4
table(predict4,test$size_category)
agreement4 <- predict4==test$size_category
table(agreement4)
prop.table(table(agreement4))
mean(predict4==test$size_category)  #accuracy = 68.49

# kernel method = besseldot.
model5 <- ksvm(train$size_category~.,data=train,kernel="besseldot")
model5
predict5 <- predict(model5,test)
predict5
table(predict5,test$size_category)
agreement5 <- predict5==test$size_category
table(agreement5)
prop.table(table(agreement5))
mean(predict5==test$size_category)  #accuracy = 57.53

# kernel method = splinedot.
model6 <- ksvm(train$size_category~.,data=train,kernel="splinedot")
model6
predict6 <- predict(model6,test)
predict6
table(predict6,test$size_category)
agreement6 <- predict6==test$size_category
table(agreement6)
prop.table(table(agreement6))
mean(predict6==test$size_category)  #accuracy = 62.32

# kernel method = anovadot.
model7 <- ksvm(train$size_category~.,data=train,kernel="anovadot")
model7
predict7 <- predict(model7,test)
predict7
table(predict7,test$size_category)
agreement7 <- predict7==test$size_category
table(agreement7)
prop.table(table(agreement7))
mean(predict7==test$size_category)  #accuracy = 90.41
