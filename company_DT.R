#Problem Statement:
#A cloth manufacturing company is interested to know about the segment or attributes causes high sale. 
#Approach - A decision tree can be built with target variable Sale.
#(we will first convert it in categorical variable) & all other variable will be independent in the analysis.  

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

Company <- read.csv(file.choose())
View(Company)
attach(Company)
summary(Company)
str(Company)
hist(Company$Sales)
High <- ifelse(Company$Sales<10,"NO","YES")
CD <- data.frame(Company,High)
View(CD)

# Partition of data.
set.seed(123)
ind <- sample(2,nrow(CD),replace = TRUE,prob = c(0.5,0.5))
cd_train <- CD[ind==1,]
View(cd_train)
cd_test  <- CD[ind==2,]

# building model using rpart function.

library(rpart)
fit2 <- rpart(High~CompPrice + Income + Advertising + 
                Population + Price + ShelveLoc + Age + Education + Urban + US, data=cd_train)
plot(fit2,margin=0.1)
text(fit2, use.n = TRUE, pretty = TRUE, cex=0.8)

# Training accuracy
#predicting on train data
pred3 <- predict(fit2, newdata=cd_train, type="class")

mean(cd_train$High==pred3) # 91.01% Accuracy

ctree.perf3 <- table(cd_train$High, pred3,
                     dnn=c("Actual", "Predicted"))
ctree.perf3
??dnn

library(caret)
confusionMatrix(pred3,cd_train$High)


# predicting on test data
pred4 <- predict(fit2, newdata=cd_test, type="class")

mean(cd_test$High==pred4) # 83.89% Accuracy

ctree.perf4 <- table(cd_test$High, pred4,
                     dnn=c("Actual", "Predicted"))
ctree.perf4

confusionMatrix(pred4,cd_test$High)
library(gmodels)
# Cross tablez
CrossTable(cd_test$High,pred4)
