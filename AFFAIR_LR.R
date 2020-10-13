#classify that a person had an affair or not .
# install required packages.

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
library(ROSE)

affair <- read.csv(file.choose())
View(affair)
attach(affair)
summary(affair)
skim(affair)
str(affair)
dim(affair)
boxplot(affair)
missmap(affair,col = c("blue","red"),legend = FALSE)

library(plyr)
affair$affairs[affair$affairs>0] <- 1
affair$affairs[affair$affairs==0] <- 0
affair$gender <- as.factor(revalue(affair$gender,c("male"=1,"female"=0)))
affair$children <- as.factor(revalue(affair$children,c("no"=0,"yes"=1)))
affair$affairs <- as.factor(affair$affairs)
train$affairs <- as.factor(train$affairs)
test$affairs <- as.factor(test$affairs)
View(affair)

as.data.frame(table(affair$affairs))
prop.table(table(affair$affairs)) #finding proportion of affairs table.

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(affair), replace = TRUE, prob = c(0.8,0.2))
train <- affair[ind==1,]
test  <- affair[ind==2,]

prop.table(table(train$affairs))
prop.table(table(test$affairs))

# build glm model
datamodel<-glm(affairs~gender + age + yearsmarried + 
                 children + factor(religiousness) + factor(education) + factor(occupation) + 
                 factor(rating),data = affair,family = binomial)
summary(datamodel)

exp(coef(datamodel))

# Confusion matrix table .
prob <- predict(datamodel,affair,type="response")
prob

# Confusion matrix and considering the threshold value as 0.5.
confusion1<-table(prob>0.5,affair$affairs)
confusion1

# Model Accuracy 
Accuracy<-sum(diag(confusion1)/sum(confusion1))
Accuracy # 77.87

# Creating empty vectors to store predicted classes based on threshold value.
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
affair[,"prob"] <- prob
affair[,"pred_values"] <- pred_values
affair[,"yes_no"] <- yes_no

View(affair[,c(1,9:11)])

table(affair$affairs,affair$pred_values)

library(ROCR)
rocrpred<-prediction(prob,affair$affairs)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

roc.curve(rocrperf,rocrpred, plotit = TRUE)

rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))

## balancing the data ##
attach(affair)

#over sampling
data_balanced_over <- ovun.sample(affairs ~ ., data = train, method = "over",N = 734)$data #N must be double of good points 367*2
as.data.frame(table(data_balanced_over$affairs))

#under sampling
data_balanced_under <- ovun.sample(affairs ~ ., data = train, method = "under", N = 226, seed = 1)$data #N must be double of negative points 113*2
table(data_balanced_under$affairs)

#mixed sampling
data_balanced_both <- ovun.sample(affairs ~ ., data = train, method = "both", p=0.5, N=480, seed = 1)$data #N is 367+113
table(data_balanced_both$affairs)

#rose sampling 
data.rose <- ROSE(affairs ~ ., data = train, seed = 111)$data
table(data.rose$affairs)

#SMOTE sampling
data_balanced_smote <- SMOTE(affairs ~ ., data = train, perc.over = 734, perc.under=226)
table(data_balanced_smote$affairs)

###################Apply Logistic classifier on balanced data###########################
#over
model_over <- glm(affairs~., data = data_balanced_over, family = binomial)
pred_over <- predict(model_over, newdata = test[-1])
roc.curve(test$affairs, pred_over) #AUC=0.719

#under
model_under <- glm(affairs~., data = data_balanced_under, family = binomial)
pred_under <- predict(model_under, newdata = test[-1])
roc.curve(test$affairs, pred_under) #AUC=0.681

#mixed
model_both <- glm(affairs~., data = data_balanced_both, family = binomial)
pred_both <- predict(model_both, newdata = test[-1])
roc.curve(test$affairs, pred_both) #AUC=0.751
??seed
#rose
model_rose <- glm(affairs~., data = data.rose, family = binomial)
pred_rose <- predict(model_rose, newdata = test[-1])
roc.curve(test$affairs, pred_rose) #AUC=0.726

#smote
model_smote <- glm(affairs~., data = data_balanced_smote, family = binomial)
pred_smote <- predict(model_smote, newdata = test[-1])
roc.curve(test$affairs, pred_smote) #AUC=0.715

#### mixed gives better AUC ##
final_both <- ovun.sample(affairs ~ ., data = affair, method = "both", p=0.5, N=601, seed = 1)$data #N is 451+150
table(final_both$affairs)

model_final <- glm(affairs~., data = final_both, family = binomial)
pred_final <- predict(model_final, newdata = affair)
roc.curve(affairs_data$affairs, pred_final) #AUC=0.7

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(pred_final>0.5,affairs_data$affairs)
confusion

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 68.05





