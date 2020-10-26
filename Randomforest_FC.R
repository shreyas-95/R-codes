#Use decision trees to prepare a model on fraud data.
#treating those who have taxable_income <= 30000 as "Risky" and others are "Good".

#Install packages required.
install.packages("randomForest")
library(randomForest)
install.packages("caret")
library(caret)
install.packages("party")
library(party)
library(rpart)
library(e1071)
install.packages("C50")
library(C50)

fraud <- read.csv(file.choose())
View(fraud)
attach(fraud)
summary(fraud)
str(fraud)
hist(fraud$Taxable.Income,col = "orange")

Risky_good <- ifelse(fraud$Taxable.Income<=30000,"Risky","Good")
fc <- data.frame(fraud[,-3],Risky_good)
View(fc)

# partion of data.
set.seed(123)
ind <- sample(2,nrow(fc),replace = TRUE,prob = c(0.7,0.3))
fc_train <- fc[ind==1,]
fc_test <- fc[ind==2,]

# building the model radom forest/bagging concept.
model1 <- randomForest(Risky_good~.,data=fc_train, na.action=na.roughfix,importance=TRUE)
model1

# Description of the random forest with no of trees, mtry = no of variables for splitting.
attributes(model1)
importance(model1)
# no of nodes of trees
hist(treesize(model1), main = "No of Nodes for the trees", col = "green")

# training
pred1 <- predict(model1, fc_train)
perf1 <- table(fc_train$Risky_good, pred1,dnn=c("Actual", "Predicted"))
perf1
mean(fc_train$Risky_good==pred1) # 87.97 accuracy 
confusionMatrix(pred1,fc_train$Risky_good)

#testing
pred2 <- predict(model1, fc_test)
perf2 <- table(fc_test$Risky_good, pred2,dnn=c("Actual", "Predicted"))
perf2
mean(fc_test$Risky_good==pred2) # 72.72% accuracy 
confusionMatrix(pred2, fc_test$Risky_good)

plot(model1,lwd=2)
legend("topleft", colnames(model1$err.rate),col=1:4,cex=0.8,fill=1:4)
varImpPlot(model1,col="red") # variable importance chart.

#### Tune Random Forest Model ####

tune <- tuneRF(fc_train[,-3], fc_train[,3], stepFactor = 2, plot = TRUE, ntreeTry = 300,trace = TRUE, improve = 1)

model2 <- randomForest(Risky_good~., data=fc_train, ntree = 300, mtry = 6, importance = TRUE,proximity = TRUE)
model2
#tune training.
pred3 <- predict(model2, fc_train)
confusionMatrix(pred3, fc_train$Risky_good)  # 100 % accuracy on training data 
# test data prediction using the Tuned model2.
pred4 <- predict(model2, fc_test)
confusionMatrix(pred4, fc_test$Risky_good) # 67.6 % accuracy on test data

# no of nodes of trees
hist(treesize(model2), main = "No of Nodes for the trees", col = "orange")
varImpPlot(model2)
importance(model2)
# which predictor variables are actually used in the random forest.
varUsed(model2)  

# Partial Dependence Plot 
partialPlot(model2, fc_train,city.population)

# Extract single tree from the forest :
getTree(model2, 1, labelVar = TRUE)

# Multi Dimension scaling plot of proximity Matrix
MDSplot(model2, fc$Risky_good)

### using cforest function###########

library(party)
rf1 <- cforest(Risky_good~., data=fc_train)
rf1
#attributes(rf1)
# training
pred_rf1 <- predict(rf1, newdata=fc_train)
perf_rf1 <- table(fc_train$Risky_good, pred_rf1,dnn=c("Actual", "Predicted"))
perf_rf1
mean(fc_train$Risky_good==pred_rf1) # 81.1% accuracy 
confusionMatrix(pred_rf1, fc_train$Risky_good)

#testing
pred2_rf1 <- predict(rf1, newdata=fc_test)
perf2_rf1 <- table(fc_test$Risky_good, pred2_rf1,dnn=c("Actual", "Predicted"))
perf2_rf1
mean(fc_test$Risky_good==pred2_rf1) # 75% accuracy 
confusionMatrix(pred2_rf1, fc_test$Risky_good)

########### using boosting technique########

# Building model on training data 
library(C50)
rf3 <- C5.0(fc[,-3],fc$Risky_good, trials = 5) #C5.0(x,y)
rf3
summary(rf3)
#training
pred_rf3 <- predict(rf3, newdata=fc_train)
perf_rf3 <- table(fc_train$Risky_good, pred_rf3,dnn=c("Actual", "Predicted"))
perf_rf3
mean(fc_train$Risky_good==pred_rf3) # 100% accuracy 
confusionMatrix(pred_rf3,fc_train$Risky_good)

#testing
pred2_rf3 <- predict(rf3, newdata=fc_test)
perf2_rf3 <- table(fc_test$Risky_good, pred2_rf3,dnn=c("Actual", "Predicted"))
perf2_rf3
mean(fc_test$Risky_good==pred2_rf3) # 100% accuracy 
confusionMatrix(pred2_rf3, fc_test$Risky_good)
plot(rf3,cex=0.1)

############ rf3 model has 100% accuracy ###########


