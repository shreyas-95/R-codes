#Problem Statement:
#A cloth manufacturing company is interested to know about the segment or attributes causes high sale. 
#Approach - A decision tree can be built with target variable Sale.
#(we will first convert it in categorical variable) & all other variable will be independent in the analysis.  
# Install packages required.

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

company_data <- read.csv(file.choose())
View(company_data)
attach(company_data)
summary(company_data)
str(company_data)
hist(company_data$Sales,col = "blue")

High <- ifelse(company_data$Sales<10,"NO","YES")
CD <- data.frame(company_data[2:11],High)
View(CD)

#Partition of data set.
set.seed(123)
ind <- sample(2,nrow(CD),replace = TRUE,prob = c(0.7,0.3))
train <- CD[ind==1,]
test <- CD[ind==2,]
View(test)

# Building radom forest/bagging technique.

model1 <- randomForest(High ~., data=train, na.action=na.roughfix,importance=TRUE)
model1

# Description of the random forest with no of trees, mtry = no of variables for splitting.
attributes(model1)
importance(model1)
# no of nodes of trees
hist(treesize(model1), main = "No of Nodes for the trees", col = "purple")

# training
pred1 <- predict(model1, train)
perf1 <- table(train$High, pred1,dnn=c("Actual", "Predicted"))
perf1
mean(train$High==pred1) # 100% accuracy 
confusionMatrix(pred1, train$High)

#testing
pred2 <- predict(model1, test)
perf2 <- table(test$High, pred2,dnn=c("Actual", "Predicted"))
perf2
mean(test$High==pred2) # 84.3% accuracy 
confusionMatrix(pred2, test$High)

plot(model1,lwd=2)
legend("topleft", colnames(model1$err.rate),col=1:4,cex=0.8,fill=1:4)
varImpPlot(model1,col="red") # variable importance chart.

#### Tune Random Forest Model ####

tune <- tuneRF(train[,-11], train[,11], stepFactor = 2, plot = TRUE, ntreeTry = 300,trace = TRUE, improve = 1)

model2 <- randomForest(High~., data=train, ntree = 300, mtry = 6, importance = TRUE,proximity = TRUE)
model2
#tune training.
pred3 <- predict(model2, train)
confusionMatrix(pred3, train$High)  # 100 % accuracy on training data 
# test data prediction using the Tuned model2.
pred4 <- predict(model2, test)
confusionMatrix(pred4, test$High) # 84.35 % accuracy on test data

# no of nodes of trees
hist(treesize(model2), main = "No of Nodes for the trees", col = "orange")
varImpPlot(model2)
importance(model2)
# which predictor variables are actually used in the random forest.
varUsed(model2)  

# Partial Dependence Plot 
partialPlot(model2, train, Population)

# Extract single tree from the forest :
getTree(model2, 1, labelVar = TRUE)

# Multi Dimension scaling plot of proximity Matrix
MDSplot(model2, CD$High)


### using cforest function###########
library(party)
rf1 <- cforest(High~., data=train)
rf1
#attributes(rf1)
# training
pred_rf1 <- predict(rf1, newdata=train)
perf_rf1 <- table(train$High, pred_rf1,dnn=c("Actual", "Predicted"))
perf_rf1
mean(train$High==pred_rf1) # 90.87% accuracy 
confusionMatrix(pred_rf1, train$High)

#testing
pred2_rf1 <- predict(rf1, newdata=test)
perf2_rf1 <- table(test$High, pred2_rf1,dnn=c("Actual", "Predicted"))
perf2_rf1
mean(test$highsales==pred2_rf1) # 78.2% accuracy 
confusionMatrix(pred2_rf1, test$High)

########### using boosting technique########

# Building model on training data 
library(C50)
rf3 <- C5.0(CD[,-11],CD$High, trials = 5) #C5.0(x,y)
rf3
summary(rf3)
pred_rf3 <- predict(rf3, newdata=train)
perf_rf3 <- table(train$High, pred_rf3,dnn=c("Actual", "Predicted"))
perf_rf3
mean(train$High==pred_rf3) # 100% accuracy 
confusionMatrix(pred_rf3, train$High)

#testing
pred2_rf3 <- predict(rf3, newdata=test)
perf2_rf3 <- table(test$High, pred2_rf3,dnn=c("Actual", "Predicted"))
perf2_rf3
mean(test$High==pred2_rf3) # 100% accuracy 
confusionMatrix(pred2_rf3, test$High)
plot(rf3,cex=0.1)

############ rf3 model has 100% accuracy ###########


