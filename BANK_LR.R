#Output variable -> y.
#y -> Whether the client has subscribed a term deposit or not.
#Binomial ("yes" or "no").

# Install Packages required.
library(mlbench)
library(Amelia)
library(e1071)
library(skimr)
library(corrplot)
library(data.table)
library(psych)
library(mvtnorm)
library(caret)
library(PRROC)
library(ggplot2)
library(caTools)
library(pROC)
library(dplyr)
library(DMwR)
library(ROSE)

bank <- read.csv(file.choose(),sep = ";")
View(bank)
attach(bank)
summary(bank)
str(bank)
skim(bank)
dim(bank)
table(bank$y)
prop.table(table(bank$y))
sum(is.na(bank))
bank <- na.omit(bank) # Omiting NA values which has atleast one NA's in a row.
bank
missmap(bank,col = c("red","blue"),legend = FALSE) #Checking for missing data.
boxplot(bank)

# building logistic regression model using imbalanced data.
model <- glm(y~.,data =bank,family = "binomial")
summary(model)
exp(coef(model)) # this is bcz o/p variable of interest is log(intercept values).

# Forming of probability by predict and confusion matrix.
prob <- predict(model,bank,type = "response")
prob
# We are going to use NULL and Residual Deviance to compare the between different models.
# Confusion matrix and considering the threshold value as 0.5.
confusion <- table(prob>0.5,bank$y)
confusion

# Model accuracy.
accuracy <- sum(diag(confusion)/sum(confusion))
accuracy  # accuracy of the model is 90.35%.

# Creating empty vectors to store predicted classes based on threshold value.
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,"1","0")
yes_no <- ifelse(prob>=0.5,"YES","NO")

# Creating new column to store the above values.
bank[,"prob"] <- prob
bank[,"pred_values"] <- pred_values
bank[,"yes_no"] <- yes_no

View(bank)
table(bank$y,bank$pred_values) # Calculate the below metrics.

# ROC(receiver operating characteristic curve) Curve => used to evaluate the betterness of the logistic model.
# more area under ROC curve better is the model.
# We will use ROC curve for any classification technique not only for logistic.
library(ROCR)
ROCpredict <- prediction(prob,bank$y)
ROCperform <- performance(ROCpredict,'tpr','fpr') #tpr-true positives/true positive=false negatives

str(ROCperform)
plot(ROCperform,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained.
## Getting cutt off or threshold value along with true positive and false positive rates in a data frame .
ROC_cutoff <- data.frame(cut_off = ROCperform@alpha.values[[1]],fpr=ROCperform@x.values,tpr=ROCperform@y.values)
colnames(ROC_cutoff) <- c("cut_off","FPR","TPR")
View(ROC_cutoff)

library(dplyr)
ROC_cutoff$cut_off <- round(ROC_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
ROC_cutoff <- arrange(ROC_cutoff,desc(TPR))
View(ROC_cutoff)

#### Building logistic regression by balancing of data.
#### partition of data.
set.seed(123)
ind <- sample(2,nrow(bank),replace = TRUE,prob = c(0.8,0.2))
train <- bank[ind==1,]
test <- bank[ind==2,]

prop.table(table(train$y))
prop.table(table(test$y))

#simple model
model_simple <- glm(y~., data = train, family = binomial)
pred_simple <- predict(model_simple, newdata = train[-17])
roc.curve(train$y, pred_simple) #AUC=0.902
?roc.curve

# balancing using different balancing technique.
#over sampling
data_balanced_over <- ovun.sample(y ~ ., data = train, method = "over",N = 64056)$data
#N must be double the traing observations.
table(data_balanced_over$y)

#under sampling
data_balanced_under <- ovun.sample(y ~ ., data = train, method = "under", N = 8496, seed = 1)$data
#N must be double of test observations.
table(data_balanced_under$y)

#mixed sampling
data_balanced_both <- ovun.sample(y ~ ., data = train, method = "both", p=0.5, N=36276, seed = 1)$data 
#N is 367+113
table(data_balanced_both$y)

#rose sampling 
data.rose <- ROSE(y ~ ., data = train, seed = 111)$data
table(data.rose$y)

#SMOTE sampling
data_balanced_smote <- SMOTE(y ~ ., data = train, perc.over = 64056, perc.under=8496)
table(data_balanced_smote$y)

###################Apply Logistic classifier on balanced data###########################
#over
model_over <- glm(y~., data = data_balanced_over, family = binomial)
pred_over <- predict(model_over, newdata = test[-17])
roc.curve(test$y, pred_over) #AUC=0.908

#under
model_under <- glm(y~., data = data_balanced_under, family = binomial)
pred_under <- predict(model_under, newdata = test[-17])
roc.curve(test$y, pred_under) #AUC=0.905

#mixed
model_both <- glm(y~., data = data_balanced_both, family = binomial)
pred_both <- predict(model_both, newdata = test[-17])
roc.curve(test$y, pred_both) #AUC=0.906

#rose
model_rose <- glm(y~., data = data.rose, family = binomial)
pred_rose <- predict(model_rose, newdata = test[-17])
roc.curve(test$y, pred_rose) #AUC=0.903

#smote
#model_smote <- glm(y~., data = data_balanced_smote, family = binomial)
#pred_smote <- predict(model_smote, newdata = test[-17])
#roc.curve(test$y, pred_smote) #AUC=

#### mixed & over gives better AUC ##
final_both <- ovun.sample(y ~ ., data = data, method = "both", p=0.5, N=36726, seed = 1)$data 
table(final_both$y)

model_final <- glm(y~., data = final_both, family = binomial)
pred_final <- predict(model_final, newdata = data)
roc.curve(data$y, pred_final) #AUC=0.910

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(pred_final>0.5,data$y)
confusion

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 87.38
