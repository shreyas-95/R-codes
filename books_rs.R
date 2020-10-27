library(recommenderlab)
library(caTools)
library(reshape2)
library(skimr)

book_data <- read.csv(choose.files())
View(book_data)
str(book_data)
summary(book_data)
skim(book_data)

book_data_matrix <- as(book_data, 'realRatingMatrix')

head(as(book_data_matrix,"data.frame")) # to view realratingmatrix
head(as(book_data_matrix,"matrix"))[,1:10]
hist(getRatings(book_data_matrix),breaks = 100)

methods(class=class(book_data_matrix))
recommendermodels <- recommenderRegistry$get_entries(datatype="realRatingMatrix")
names(recommendermodels)

# Data partition
#set.seed(123)
#ind <- sample(2,nrow(book_data_matrix), replace = T, prob = c(0.8,0.2))
#train <- book_data_matrix[ind==1,]
#test <- book_data_matrix[ind==2,]

#model1 <- Recommender(train, method = "UBCF")
#model1

#pred1 <- predict(model1, newdata=test, n=10)
#pred1
#as(pred1, "list")

#Popularity based 

book_recomm_model1 <- Recommender(book_data_matrix, method="POPULAR")

#Predictions for 25 users 
# n is no. of recommendations
recommended_items1 <- predict(book_recomm_model1, book_data_matrix[1:25], n=5) 
as(recommended_items1, "list")

## Popularity model recommends the same books for all users , we need to improve our model using # # Collaborative Filtering

#User Based Collaborative Filtering

book_recomm_model2 <- Recommender(book_data_matrix, method="UBCF")

#Predictions for 25 users 
recommended_items2 <- predict(book_recomm_model2, book_data_matrix[1:25], n=5)
as(recommended_items2, "list")

#Item Based Collaborative Filtering

book_recomm_model3 <- Recommender(book_data_matrix, method="IBCF")

#Predictions for 25 users 
recommended_items3 <- predict(book_recomm_model3, book_data_matrix[1:25], n=5)
as(recommended_items3, "list")

#SVD Based 

book_recomm_model4 <- Recommender(book_data_matrix, method="SVD")

#Predictions for 25 users 
recommended_items4 <- predict(book_recomm_model4, book_data_matrix[1:25], n=5)
as(recommended_items4, "list")

