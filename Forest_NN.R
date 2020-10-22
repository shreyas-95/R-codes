# PREDICT THE BURNED AREA OF FOREST FIRES WITH NEURAL NETWORKS.
# Install required packages.

library(neuralnet)
library(nnet)
library(NeuralNetTools)
library(plyr)
library(skimr) 

forest <- read.csv(file.choose())
View(forest)
attach(forest)
skim(forest)
str(forest)
summary(forest)

#creating dummies
forest$month <- as.integer(factor(forest$month,levels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"),labels = c(1,2,3,4,5,6,7,8,9,10,11,12)))
forest$day <- as.integer(factor(forest$day,levels = c("sun","mon","tue","wed","thu","fri","sat"),labels = c(1,2,3,4,5,6,7)))
forest$size_category=as.integer(factor(forest$size_category,levels = c("large","small"),labels = c(1,0)))
str(forest)

# Normalization of data.
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
forest_norm <- as.data.frame(lapply(forest,FUN = normalize))
View(forest_norm)

# Partion of data.
set.seed(1234)
ind <- sample(2,nrow(forest_norm),replace = TRUE,prob = c(0.7,0.3))
train <- forest_norm[ind==1,]
test <- forest_norm[ind==2,]

# Building neural network model.
model1 <- neuralnet(train$area~.,data = train)
model1
str(model1)
plot(model1,rep="best")

# Evaluating model performance.
# Compute function to generate ouput for the model prepared.
set.seed(12323)
model_results <- compute(model1,test)
str(model_results)
predict_strength <- model_results$net.result
# predicted_strength
# model_results$neurons

cor(predict_strength,test$area)  # 0.52 corelation is very poor.
plot(predict_strength,test$area)

# Building model for original data.
model<-neuralnet(area~.,data=forest_norm,hidden = 5)
plot(model)
model_res<-compute(model,test)
predict <-model_res$net.result
cor(predict,test$area)  # 0.89 
plot(predict,test$area)
