#  Non-hierarchial clustering or K-means clustering.

install.packages("xlsx")
library(xlsx)
install.packages("plyr")
library(plyr)
airlines <- read.xlsx(file.choose(),2)
View(airlines)
attach(airlines)
str(airlines)
summary(airlines)

mydata <- airlines[1:3999,1:12]
normal <- scale(mydata[,2:12])

# finding of k.
#Method1
wss = (nrow(normal)-1)*sum(apply(normal,2,var))
for (i in 2:12) wss[i]= sum(kmeans(normal,centers = i)$withinss)
plot(1:12, wss, type = "b",xlab="number of clusters",ylab="within the sum of square")
title(sub = "kmeans clustering screew plot")
  
#Method2
install.packages("kselection")
library(kselection)
install.packages("doparallel")
library(doParallel)
registerDoParallel(cores=2)

k <- kselection(mydata[,2:12], parallel = TRUE ,k_threshold = 0.9,max_centers = 12)
k

km <- kmeans(normal,4)
str(km)

final <- data.frame(mydata,km$cluster)
View(final)
