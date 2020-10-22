# Prepare a model for strength of concrete data using Neural Networks.
# Install required packages.

library(neuralnet)
library(nnet)
library(NeuralNetTools)
library(plyr)
library(skimr)

concrete <- read.csv(file.choose())
View(concrete)
attach(concrete)
skim(concrete)
summary(concrete)
str(concrete)

#plots.
hist(concrete$strength,prob=TRUE,breaks = 50)
lines(density(concrete$strength))
hist(concrete$slag,prob=TRUE,breaks = 30)
lines(density(concrete$slag))

# Normalizing of data.
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
concrete_norm <- as.data.frame(lapply(concrete,FUN = normalize))
summary(concrete_norm)
View(concrete_norm)                              

# Partition of data.
set.seed(1234)
ind <- sample(2,nrow(concrete_norm),replace = TRUE,prob = c(0.7,0.3))
train <- concrete_norm[ind==1,]
test <- concrete_norm[ind==2,]

# Building neural network model.
concrete_model <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data =train)
str(concrete_model)
concrete_model
plot(concrete_model,rep = "best")

# Evaluating model performance.
# Compute function to generate ouput for the model prepared.
set.seed(12323)
model_results <- compute(concrete_model,test[1:8])
str(model_results)
predicted_strength <- model_results$net.result
# predicted_strength
# model_results$neurons

cor(predicted_strength,test$strength)  # 0.84
plot(predicted_strength,test$strength)

# since the prediction is in Normalized form, we need to de-normalize it 
# to get the actual prediction on strength.
str_max <- max(concrete$strength)
str_min <- min(concrete$strength)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

Actualarea_pred <- unnormalize(predicted_strength,str_min,str_max)
head(Actualarea_pred)


# Buildingmodel for original data.
model<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data= concrete_norm,hidden = 5)
plot(model)
model_res<-compute(model,test[1:8])
predict <-model_res$net.result
cor(predict,test$strength)  # 0.95
plot(predict,test$strength)
# SSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased

model1 <- neuralnet(strength~.,data = concrete_norm,hidden = 5)
plot(model1)
model1_res <- compute(model1,test[1:8])
predict1 <- model1_res$net.result
cor(predict1,test$strength)
plot(predict1,test$strength)

###############################################333
