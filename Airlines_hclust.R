install.packages("xlsx")
library(xlsx)

# 1. hierarchial clustering for obtaining optimum numbers of cluster.
# EDA part
airlines <- read.xlsx("G:\\ExcelR Material DATA SCIENCE\\Assignments\\Clustering\\EastWestAirlines.xlsx",2)
View(airlines)
attach(airlines)
summary(airlines)

library(skimr)
skim(airlines)
var(airlines$Qual_miles)

library(e1071)
skewness(airlines$Bonus_miles)
kurtosis(airlines$ID.)
str(airlines)
plot(airlines)

# hierarchial clustering

mydata <- airlines[1:3999,1:12]
normalizing <- scale(mydata[,2:12])
distance <- dist(normalizing,method = "manhattan")
dist2 <- dist(normalizing,method = "euclidean")

fit <- hclust(distance, method = "complete")

plot(fit)
plot(fit, hang=-1)

# inference is in heirarchial we cant get clusters for the huge data set.


