crime <- read.csv(file.choose())
View(crime)
attach(crime)

# exploratory data analysis

plot(crime)
summary(crime)
str(crime)

library(skimr)
skim(crime)
var(crime$Murder)
var(crime$Assault)
var(crime$UrbanPop)
var(crime$Rape)

library(stats)
library(e1071)

skewness(crime$Murder)
skewness(crime$Assault)
skewness(crime$UrbanPop)
skewness(crime$Rape)

kurtosis(crime$Murder)
kurtosis(crime$Assault)
kurtosis(crime$UrbanPop)
kurtosis(crime$Rape)

getmode <- function(crime){
  u <- unique(crime)
  u[which.max(tabulate(match(crime,u)))]
}
getmode(crime$Murder)
getmode(crime$X)
getmode(crime$Assault)
getmode(crime$UrbanPop)
getmode(crime$Rape)

cor(crime[-1])

# clustering part


mydata <- crime[1:50,1:5]
normalize <- scale(mydata[,2:5])
distance <- dist(normalize,method = "euclidean")
summary(normalize)

summary(distance)

fit <- hclust(distance, method = "complete")
plot(fit)
plot(fit,hang = -1)
str(fit)

group <- cutree(fit, k=5)
group
rect.hclust(fit, k=5,border ="blue")

crimerate <- as.matrix(group)

final <- data.frame(crime,crimerate)
View(final)


install.packages("xlsx")
library(xlsx)
write.xlsx(final, file="final.xlsx")



# non hierarachial clustering / kmeans clustering.

mydata <- crime[1:50,1:5]
normalize <- scale(mydata[,2:5])

install.packages("plyr")
install.packages("kselection")
install.packages("doParallel")                 

wss = (nrow(normalize)-1)*sum(apply(normalize,2,var))
for (i in 2:5) wss[i]= sum(kmeans(normalize,centers = i)$withinss)
plot(1:5,wss,type = "b", xlab = "number of cluster",ylab = "with in the sum of the square")
title(sub = "k means screew plot")

k <- kselection(mydata[,2:5], parallel = TRUE, k_threshold = 0.9,max_centers = 12)
k

km <- kmeans(normalize,2)
str(km)

final1 <- data.frame(mydata,km$cluster)
