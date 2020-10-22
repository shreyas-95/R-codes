# Build a Neural Network model for 50_startups data to predict profit.
# Install required packages.

library(neuralnet)
library(nnet)
library(NeuralNetTools)
library(plyr)
library(skimr) 

startups <- read.csv(file.choose())
View(startups)
attach(startups)
skim(startups)
str(startups)
summary(startups)

startups$State <- as.numeric(revalue(startups$State,c("New York"="0","California"="1","Florida"="2")))
cor(startups)
plot(startups)
plot(startups$R.D.Spend)
plot(startups$Administration)

# Normalization of data.
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
Startups_norm<-as.data.frame(lapply(startups,FUN = normalize))
summary(Startups_norm$Profit) 
View(Startups_norm)
summary(Startups_norm)

# Data partitioning.
set.seed(123)
ind <- sample(2,nrow(Startups_norm),replace = TRUE,prob = c(0.8,0.2))
train <- Startups_norm[ind==1,]
test <- Startups_norm[ind==2,]

# Building a neural network model.
model1 <- neuralnet(train$Profit~., data = train)
model1
str(model1)
plot(model1,rep="best")

# Evaluating model performance.
# Compute function to generate ouput for the model prepared.
set.seed(12323)
model_results <- compute(model1,test[1:4])
str(model_results)
predict_strength <- model_results$net.result
# predicted_strength
# model_results$neurons

cor(predict_strength,test$Profit)  # 0.94
plot(predict_strength,test$Profit)

# Building model for original data.
model<-neuralnet(Profit~.,data=Startups_norm,hidden = 5)
plot(model)
model_res<-compute(model,test[1:4])
predict <-model_res$net.result
cor(predict,test$Profit)  # 0.98
plot(predict,test$Profit)
# SSE has reduced and training steps had been increased as the number of neurons. 
# under hidden layer are increased.