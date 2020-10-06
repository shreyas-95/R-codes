#Prepare rules for the all the data sets.
#1) Try different values of support and confidence. Observe the change in number of rules for different support,confidence values
#2) Change the minimum length in apriori algorithm.
#3) Visulize the obtained rules using different plots.

# Packages installed.
library(arules)
library(arulesViz)
library(rmarkdown)

# Exploratory data analysis.
movies <- read.csv("G:\\ExcelR Material DATA SCIENCE\\Assignments\\Association Rules\\my_movies.csv")
View(movies)
attach(movies)
summary(movies)
str(movies)
barplot(sapply(movies[6:15],sum),col = 6:15)

# Building model for association rules.
rules <- apriori(as.matrix(movies[,6:15]),parameter = list(support=0.002,confidence=0.04,minlen=2))
inspect(rules[1:100])
plot(rules)
# sorting rules bu confidence and lift
Sort_conf <- sort(rules,by="confidence")
sort_lift <- sort(rules,by="lift")
inspect(head(Sort_conf[1:25]))
inspect(head(sort_lift))
# ploting of sort rules.
plot(rules,method = "scatterplot")
plot(rules,method ="grouped")
plot(rules,method = "graph")
plot(rules,method = "mosaic")

####### rule 2 #############
rules2 <- apriori(as.matrix(movies[,6:15]),parameter = list(support=0.004,confidence=0.04,minlen=4))
inspect(rules2[1:100])
plot(rules2)
# sorting rules bu confidence and lift
Sort_conf <- sort(rules2,by="confidence")
sort_lift <- sort(rules2,by="lift")
inspect(head(Sort_conf[1:25]))
inspect(head(sort_lift))
# ploting of sort rules.
plot(rules2,method = "scatterplot")
plot(rules2,method ="grouped")
plot(rules2,method = "graph")
plot(rules2,method = "mosaic")

############ rule 3 ############
rules3 <- apriori(as.matrix(movies[,6:15]),parameter = list(support=0.008,confidence=0.4,minlen=2))
inspect(rules3[1:100])
plot(rules3)
# sorting rules bu confidence and lift
Sort_conf <- sort(rules3,by="confidence")
sort_lift <- sort(rules3,by="lift")
inspect(head(Sort_conf[1:25]))
inspect(head(sort_lift))
# ploting of sort rules.
plot(rules3,method = "scatterplot")
plot(rules3,method ="grouped")
plot(rules3,method = "graph")
plot(rules3,method = "mosaic")

################# rule 4 ###################
rules4 <- apriori(as.matrix(movies[,6:15]),parameter = list(support=0.003,confidence=0.04,minlen=3))
inspect(rules4[1:100])
plot(rules4)
# sorting rules bu confidence and lift
Sort_conf <- sort(rules4,by="confidence")
sort_lift <- sort(rules4,by="lift")
inspect(head(Sort_conf[1:25]))
inspect(head(sort_lift))
# ploting of sort rules.
plot(rules4,method = "scatterplot")
plot(rules4,method ="grouped")
plot(rules4,method = "graph")
plot(rules4,method = "mosaic")

############## rule 5 ###################
rules5 <- apriori(as.matrix(movies[,6:15]),parameter = list(support=0.007,confidence=0.05,minlen=5))
inspect(rules[1:100])
plot(rules5)
# sorting rules bu confidence and lift
Sort_conf <- sort(rules5,by="confidence")
sort_lift <- sort(rules5,by="lift")
inspect(head(Sort_conf[1:25]))
inspect(head(sort_lift))
# ploting of sort rules.
plot(rules5,method = "scatterplot")
plot(rules5,method ="grouped")
plot(rules5,method = "graph")
plot(rules5,method = "mosaic")

