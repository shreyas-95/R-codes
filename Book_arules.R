#Prepare rules for the all the data sets. 
#1) Try different values of support and confidence. Observe the change in number of rules for different support,confidence values
#2) Change the minimum length in apriori algorithm.
#3) Visulize the obtained rules using different plots.

# Install packages required.
library(arules)
library(arulesViz)
library(tm)
library(rmarkdown)
library(Matrix)
library(grid)

book <- read.csv("G:\\ExcelR Material DATA SCIENCE\\Assignments\\Association Rules\\book.csv")
View(book)
attach(book)
summary(book)
str(book)
book_trans<-as(as.matrix(book),"transactions")
inspect(book_trans[1:100])
barplot(sapply(book,sum),col=1:11)

#Applying apriori algorithm to get relevant rules
rules <- apriori(as.matrix(book),parameter = list(support=0.02,confidence=0.05,minlen=2))
inspect(rules)
plot(rules)

#Sorting rules by confidence 
rules_conf <- sort(rules,by="confidence")
inspect(rules_conf)
#Sorint rules by lift ratio
rules_lift <- sort(rules,by="lift")
inspect(rules_lift)

# Visualizing rules in scatter plot
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")
plot(rules,method = "mosaic")

########### Rule 2 ###########
rules2 <- apriori(as.matrix(book),parameter = list(support=0.04,confidence=0.05,minlen=4))
inspect(rules2)
plot(rules2)

#Sorting rules by confidence 
rules_conf <- head(sort(rules,by="confidence"))
inspect(rules_conf)
#Sorint rules by lift ratio
rules_lift <- sort(rules,by="lift")
inspect(rules_lift)

# Visualizing rules in scatter plot
plot(rules2,method = "scatterplot")
plot(rules2,method = "grouped")
plot(rules2,method = "graph")
plot(rules2,method = "mosaic")

########## Rule 3 ###########
rules3 <- apriori(as.matrix(book),parameter = list(support=0.05,confidence=0.05,minlen=5))
inspect(rules3)
plot(rules3)

#Sorting rules by confidence 
rules_conf <- head(sort(rules,by="confidence"))
inspect(rules_conf)
#Sort rules by lift ratio
rules_lift <- sort(rules,by="lift")
inspect(rules_lift)

# Visualizing rules in scatter plot
plot(rules3,method = "scatterplot")
plot(rules3,method = "grouped")
plot(rules3,method = "graph")
plot(rules3,method = "mosaic")


