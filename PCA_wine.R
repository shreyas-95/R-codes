# Perform Principal component analysis and perform clustering using first 
# 3 principal component scores (both heirarchial and k mean clustering(scree plot or elbow curve) and obtain 
# optimum number of clusters and check whether we have obtained same number of clusters with the original data 
# (class column we have ignored at the begining who shows it has 3 clusters)df.

# packages to be installed.
install.packages("xlsx")
installed.packages("gdata")
library(xlsx)
library(gdata)

wine <- read.csv("G:\\ExcelR Material DATA SCIENCE\\Assignments\\PCA\\wine.csv")
View(wine)

help("princomp")
mydata <- wine[,-1] # removing the 1st column has it indicates types.
View(mydata)
attach(mydata)
cor(mydata)
cov(mydata)
pca <- princomp(mydata,cor = TRUE,scores = TRUE,covmat = NULL)

str(pca)
summary(pca)
loadings(pca)
plot(pca)
biplot(pca)

# Showing the increase of variance with considering principal components
# Which helps in choosing number of principal components
plot(cumsum(pca$sdev*pca$sdev)*100/(sum(pca$sdev*pca$sdev)),type="b")


mydata <- cbind(mydata,pca$scores[,1:8])
View(mydata)

# clustering process
# (considering only pca scores as they represent the entire data)
 clust <- mydata[,14:21]
normalize <- scale(clust) 
distance <- dist(normalize,method = "euclidean")
fit <- hclust(distance,method = "single")
plot(fit)
plot(fit,hang = -1)
group <- cutree(fit,5)
rect.hclust(fit,k=5,border = "red")
final <- cbind(group,mydata)
View(final)
View(aggregate(final[,-c(,14:21)],by=list(final),FUN=mean))

# kmeans clustering
clust <- mydata[,14:21]
normalize <- scale(clust) 
wss = (nrow(normalize)-1)*sum(apply(normalize, 2, var))
# Determine number of clusters by scree-plot 
for (i in 14:21) wss[i] = sum(kmeans(normalize, centers=i)$withinss)
plot(1:21, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

fit1 <- kmeans(normalize,4)
summary(fit)
final1 <- data.frame(mydata,fit1$cluster)
View(final1)
aggregate(mydata[,14:21], by=list(fit1$cluster), FUN=mean)
